{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Link.ObjFile where
import Util.FileBuf
import Util.Num
import Util.Sequence
import Util.Pointed
import Util.Tuples

import Control.Monad
import System.IO
import Data.Bits
import GHC.Word

-- compile-objects, suitable for linking or otherwise analyzing
data CObj         = CObj [Sect] [Symbol] deriving (Eq, Show)
data Sect         = Sect String [Word8] [Fixup] [SectFlag] deriving (Eq, Show)
data SectFlag     = Code | Data | Read | Write deriving (Eq, Show)
data Fixup        = Fixup Word32 Word32 FixupType deriving (Eq, Show)
data FixupType    = Relative | Absolute | RVAAbsolute deriving (Eq, Show)
data Symbol       = Symbol String Word32 Word32 SymbolType StorageClass deriving (Eq, Show)
data SymbolType   = Export | Import deriving (Eq, Show)
data StorageClass = External | Internal deriving (Eq, Show)

show_obj :: CObj -> String
show_obj obj@(CObj sects syms) =
    "==================================================\n\n" ++
    concat [show_sect obj s ++ "\n" | s <- sects] ++
    "==================================================\n\n" ++
    "Symbols:\n" ++
    concat [show_sym obj s ++ "\n" | s <- syms] ++ "\n" ++
    "==================================================\n"

show_sect :: CObj -> Sect -> String
show_sect cobj (Sect sname sdata fixups flags) =
    "Section: '" ++ sname ++ "'\n" ++
    "Section raw data size: " ++ show (length sdata) ++ " (B)\n" ++
    "Section flags: " ++ show flags ++ "\n" ++
    "Fixups:\n" ++
    concat ["    " ++ show_fixup cobj fixup ++ "\n" | fixup <- fixups] ++
    "\n"

show_fixup :: CObj -> Fixup -> String
show_fixup cobj (Fixup vaddr sym ty) =
    "Patch '" ++ symName (objectSymbols cobj !! castn sym) ++ "' at " ++ show vaddr ++ " [type " ++ show ty ++ "]"
 
show_sym :: CObj -> Symbol -> String
show_sym cobj (Symbol sname v i ty _) =
    sname ++ "@" ++ sectName (objectSections cobj !! castn i) ++ ": " ++ show v ++ " [type " ++ show ty ++ "]"

objectSections :: CObj -> [Sect]
objectSections (CObj ss _) = ss

objectSymbols :: CObj -> [Symbol]
objectSymbols (CObj _ syms) = syms

objectInsertSection :: CObj -> Sect -> CObj
objectInsertSection obj sect = first $ objectUpsertSection obj (sectName sect) (const sect)

objectFindOrInsertSection :: CObj -> Sect -> (CObj, Sect, Int)
objectFindOrInsertSection obj sect = objectUpsertSection obj (sectName sect) (chooseDefault sect)
    
objectUpsertSection :: CObj -> String -> (Maybe Sect -> Sect) -> (CObj, Sect, Int)
objectUpsertSection (CObj sects syms) sname cfn = (CObj sects' syms, sect, i) where
    (sects', sect, i) = upsertSeq sects ((== sname) . sectName) cfn

objectInsertSymbol :: CObj -> Symbol -> CObj
objectInsertSymbol obj sym = first $ objectUpsertSymbol obj (symName sym) (symType sym) (const sym)

objectFindOrInsertSymbol :: CObj -> Symbol -> (CObj, Symbol, Int)
objectFindOrInsertSymbol obj sym = objectUpsertSymbol obj (symName sym) (symType sym) (chooseDefault sym)
    
objectUpsertSymbol :: CObj -> String -> SymbolType -> (Maybe Symbol -> Symbol) -> (CObj, Symbol, Int)
objectUpsertSymbol (CObj sects syms) sname sty cfn = (CObj sects syms', sym, i) where
    (syms', sym, i) = upsertSeq syms (\s -> symName s == sname && symType s == sty) cfn

sectName :: Sect -> String
sectName (Sect name _ _ _) = name
    
sectData :: Sect -> [Word8]
sectData (Sect _ sdata _ _) = sdata

sectFixups :: Sect -> [Fixup]
sectFixups (Sect _ _ fixups _) = fixups

sectFlags :: Sect -> [SectFlag]
sectFlags (Sect _ _ _ sfs) = sfs

symName :: Symbol -> String
symName (Symbol s _ _ _ _) = s

symValue :: Num n => Symbol -> n
symValue (Symbol _ v _ _ _) = castn v

symSectIndex :: Symbol -> Int
symSectIndex (Symbol _ _ i _ _) = castn i

symType :: Symbol -> SymbolType
symType (Symbol _ _ _ sty _) = sty

symStorageClass :: Symbol -> StorageClass
symStorageClass (Symbol _ _ _ _ sclass) = sclass

-- parse an object file on disk
readCObject :: String -> IO CObj
readCObject fpath = do
    -- begin reading the object
    b <- openBuf fpath ReadMode;
    
    -- read the object header
    m <- readW16 b;
    when (m /= 0x14c) $ do
        fail ("Invalid or corrupt object file: " ++ fpath);
    slen   <- readW16 b;
    ts     <- readW32 b;
    saddr  <- readW32 b;
    scount <- readW32 b;
    _      <- readW16 b;
    _      <- readW16 b;
    
    -- read sections and symbols from the file
    sects <- readCSections b (castn slen);
    syms  <- readCSymbols  b (castn saddr) (castn scount);

    -- and that's it
    closeBuf b;
    return (CObj sects syms)
    
readCSections :: FileBuf -> Int -> IO [Sect]
readCSections b sects = do
    takeM sects $ do
        name  <- readChBuf b 55;
        _     <- readW32 b;
        _     <- readW32 b;
        sdlen <- readW32 b;
        dloc  <- readW32 b;
        ploc  <- readW32 b;
        _     <- readW32 b;
        flen  <- readW16 b;
        _     <- readW16 b;
        flag  <- readW32 b;
        sdata <- atLoc b (castn dloc) $ do
            sd <- readBuf b (castn sdlen);
            return sd
        fixups <- atLoc b (castn ploc) $ takeM (castn flen) $ do
            vaddr <- readW32 b;
            sym   <- readW32 b;
            ty    <- readW16 b;
            return (Fixup vaddr sym (decodeFixupType ty))
        return (Sect name sdata fixups (decodeSectFlags flag))

readCSymbols :: FileBuf -> Integer -> Int -> IO [Symbol]
readCSymbols b sloc len = do
    jumpBuf b sloc;
    takeM len $ do
        dloc     <- readW32 b;
        val      <- readW32 b;
        sectidx  <- readW32 b;
        sty      <- readW16 b;
        stoclass <- readW8 b;
        sdata    <- atLoc b (castn dloc) (readStr b);
        return (Symbol sdata val sectidx (decodeSymbolType sty) (decodeStorageClass stoclass))

-- store an object file to disk
writeCObject :: CObj -> String -> IO ()
writeCObject (CObj sects syms) fpath = do
    -- begin writing the object
    b <- openBuf fpath WriteMode;
    t <- ctime;
    
    -- write the object header
    writeBuf b $ w16 0x14c;
    writeBuf b $ cw16 (length sects);
    writeBuf b $ w32 t;
    writeBuf b $ w32 0;                 -- <- we'll need to patch this up later with the sym-table address
    writeBuf b $ cw32 (length syms);
    writeBuf b $ w16 0;
    writeBuf b $ w16 0;
    
    -- write each section
    writeCSections b sects;
    
    -- now we know where the symbol table will be, fixup its pointer
    rewriteCurLoc b 8;
    
    -- write the symbol table
    writeCSymbols b syms;
    
    -- and we've finished
    closeBuf b;
    return ()

writeCSections :: FileBuf -> [Sect] -> IO ()
writeCSections b sects = do
    hdr_fixups <- mapM writeHdr sects;
    ssave (map first  hdr_fixups) (writeBuf b) (map sectData sects);
    ssave (map second hdr_fixups) writeFixups (map sectFixups sects);
    return ()
    where
    ssave patch_locs write_fn sdata = do
        forM_ (zip patch_locs sdata) (\(p, d) -> do { rewriteCurLoc b p; write_fn d })
    writeHdr (Sect name sdata fixups flag) = do
        writeBuf b $ chbuf name 55;
        writeBuf b $ w32 0;
        writeBuf b $ w32 0;
        writeBuf b $ cw32 (length sdata);
        sd_loc <- tellBuf b;
        writeBuf b $ w32 0;
        fixups_loc <- tellBuf b;
        writeBuf b $ w32 0;
        writeBuf b $ w32 0;
        writeBuf b $ cw16 (length fixups);
        writeBuf b $ w16 0;
        writeBuf b $ w32 (encodeSectFlags flag);
        return (sd_loc, fixups_loc)
    writeFixups fixups = do
        forM_ fixups writeFixup
    writeFixup (Fixup vaddr sym ty) = do
        writeBuf b $ w32 vaddr
        writeBuf b $ w32 sym
        writeBuf b $ w16 (encodeFixupType ty)

writeCSymbols :: FileBuf -> [Symbol] -> IO ()
writeCSymbols b syms = do
    spatches <- mapM writeSymHdr syms;
    forM_ (zip spatches syms) writeSymData;
    where
    writeSymHdr (Symbol _ val sectidx sty stoclass) = do
        dataloc <- tellBuf b;
        writeBuf b $ w32 0;
        writeBuf b $ w32 val;
        writeBuf b $ w32 sectidx;
        writeBuf b $ w16 (encodeSymbolType sty);
        writeBuf b $ w8  (encodeStorageClass stoclass);
        return dataloc
    writeSymData (p, (Symbol sdata _ _ _ _)) = do
        rewriteCurLoc b p;
        writeBuf b $ str sdata
        
decodeSectFlags :: Word32 -> [SectFlag]
decodeSectFlags n | (n .&. 4) == 4 = Write:(decodeSectFlags (n `xor` 4))
decodeSectFlags n | (n .&. 2) == 2 = Read:(decodeSectFlags (n `xor` 2))
decodeSectFlags 1                  = [Data]
decodeSectFlags 0                  = [Code]
decodeSectFlags _                  = []

encodeSectFlags :: [SectFlag] -> Word32
encodeSectFlags (Write:rs) = 4 .|. encodeSectFlags rs
encodeSectFlags (Read:rs)  = 2 .|. encodeSectFlags rs
encodeSectFlags (Data:rs)  = 1 .|. encodeSectFlags rs
encodeSectFlags (Code:rs)  = encodeSectFlags rs
encodeSectFlags [] = 0

decodeSymbolType :: Word16 -> SymbolType
decodeSymbolType 0 = Export
decodeSymbolType _ = Import

encodeSymbolType :: SymbolType -> Word16
encodeSymbolType Export = 0
encodeSymbolType Import = 1

decodeStorageClass :: Word8 -> StorageClass
decodeStorageClass 0 = Internal
decodeStorageClass _ = External

encodeStorageClass :: StorageClass -> Word8
encodeStorageClass Internal = 0
encodeStorageClass External = 1

decodeFixupType :: Word16 -> FixupType
decodeFixupType 0 = Relative
decodeFixupType 1 = Absolute
decodeFixupType _ = RVAAbsolute

encodeFixupType :: FixupType -> Word16
encodeFixupType Relative    = 0
encodeFixupType Absolute    = 1
encodeFixupType RVAAbsolute = 2
