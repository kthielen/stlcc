{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Link.Windows.PE where
import Link.Windows.DLL
import Link.ObjFile
import Util.FileBuf
import Util.Num
import Util.String
import Util.Sequence
import Util.Tuples

import qualified Data.Map  as Map
import qualified Data.List as List
import Control.Monad
import System.IO
import Data.Bits
import GHC.Word

-- Link a set of object files together into a target executable
link :: [CObj] -> String -> IO ()
link objs fpath = do
    b                    <- openBuf fpath WriteMode;
    eobjs                <- withExportObject objs;
    let image_base        = 0x01000000;
    let section_alignment = 0x00001000;
    let file_alignment    = 0x00000200;
    let stack_reserve     = 0x00040000;
    let stack_commit      = 0x00004000;
    let heap_reserve      = 0x00100000;
    let heap_commit       = 0x00010000;
    let gsects            = gatherSections (mainImportObject:eobjs);
    let section_base      = rvaBase (Map.size gsects) section_alignment;
    let linked_sections   = coalesceSections (image_base + section_base) section_alignment gsects;
    let sum_sect_size f   = sum [length (sectData s) | s <- linked_sections, f `elem` sectFlags s];
    let sect_offset f     = meither id 0 $ mhead [align (length (sectData s)) section_alignment | s <- takeWhile (not . elem f . sectFlags) linked_sections];
    let sect_rva s        = meither (+ section_base) 0 $ maybeFindSymbol section_alignment gsects s;
    let sect_bounds s     = (b, (sect_rva ("$" ++ s ++ "_end")) - b) where b = sect_rva ("$" ++ s ++ "_begin");
    let import_info       = sect_bounds "import";
    let iat_info          = sect_bounds "iat";
    let export_info       = sect_bounds "export";
    let res_info          = sect_bounds "res";
    
    writeDOSStub b;
    writeWinHeader b
        (castn $ Map.size gsects)
        0
        0
        (castn $ sum_sect_size Code)
        (castn $ sum_sect_size Data)
        0                                                                   -- sizeof-bss
        (castn $ section_base + findSymbol section_alignment gsects "main") -- entry point
        (castn $ section_base + sect_offset Code)                           -- code sect base
        (castn $ section_base + sect_offset Data)                           -- data sect base
        (castn image_base)
        (castn section_alignment)
        (castn file_alignment)
        (castn $ section_base + sum [align (length $ sectData s) section_alignment | s <- linked_sections]) -- sizeof image
        stack_reserve
        stack_commit
        heap_reserve
        heap_commit
        (castn $ first  export_info) -- export rva
        (castn $ second export_info) -- export size
        (castn $ first  import_info) -- import rva
        (castn $ second import_info) -- import size
        (castn $ first  iat_info)    -- iat rva
        (castn $ second iat_info)    -- iat size
        (castn $ first  res_info)    -- res rva
        (castn $ second res_info)    -- res size
        
    -- now that headers are written, write the actual section data
    writeSections b file_alignment section_alignment linked_sections;
    
    -- at this point, the complete executable file is written
    closeBuf b;
    return ()

-- Write the Windows header for an executable
writeWinHeader :: FileBuf -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
writeWinHeader b section_count psymtable symbol_count sizeof_code sizeof_data sizeof_bss pentrypoint pcodesect pdatasect image_base section_alignment file_alignment sizeof_image stack_reserve stack_commit heap_reserve heap_commit export_rva export_size import_rva import_size iat_rva iat_size res_rva res_size = do
    let characteristics = 0x0001 .|. 0x0002 .|. 0x0100 .|. 0x0004 .|. 0x0008; -- temporary[?] - assume an executable without relocations
    ct <- ctime;
    
    -- write the NT header
    writeW32 b 0x00004550;                   -- signature
    writeW16 b 0x014c;                       -- machine identifier (x86)
    writeW16 b section_count;                -- number of sections
    writeW32 b ct;                           -- current time
    writeW32 b psymtable;                    -- pointer to the symbol table
    writeW32 b symbol_count;                 -- number of symbols in the table
    writeW16 b 0x00E0; --1792;                         -- (int16 . `(struct-size nil (map head opt-header))) ; the optional header follows this one
    writeW16 b characteristics;              -- is this an executable? relocs stripped? etc ...

    -- write the "optional" header
    writeW16 b 0x10b;                                  -- 'magic' signature
    writeW8  b 7;                                      -- major linker version
    writeW8  b 10;                                     -- minor linker version
    writeW32 b sizeof_code;                            -- size of the 'code' sections
    writeW32 b sizeof_data;                            -- size of the 'initialized data' sections
    writeW32 b sizeof_bss;                             -- size of the 'uninitialized data' sections
    writeW32 b pentrypoint;                            -- the RVA of the first instruction to execute
    writeW32 b pcodesect;                              -- the RVA of the first byte of the code section
    writeW32 b pdatasect;                              -- the RVA of the first byte of the data section
    writeW32 b image_base;                             -- the base address for the whole image
    writeW32 b section_alignment;                      -- load-time section alignment
    writeW32 b file_alignment;                         -- storage-time section alignment
    writeW16 b 0x0005;                                 -- major OS version
    writeW16 b 0x0001;                                 -- minor OS version
    writeW16 b 0x0005;                                 -- major image version
    writeW16 b 0x0001;                                 -- minor image version
    writeW16 b 0x0004;                                 -- major subsystem version
    writeW16 b 0x0000;                                 -- minor subsystem version
    writeW32 b 0x00000000;                             -- Win32 version
    writeW32 b sizeof_image;                           -- total image size
    writeW32 b (rvaBase section_count file_alignment); -- the size of all headers plus section tables
    writeW32 b 0;                                      -- a 'checksum'
    writeW16 b 2;                                      -- program runs in the Windows GUI subsystem
    writeW16 b 0;                                      -- dll characteristics
    writeW32 b stack_reserve;                          -- stack space to reserve
    writeW32 b stack_commit;                           -- stack space to commit
    writeW32 b heap_reserve;                           -- heap space to reserve
    writeW32 b heap_commit;                            -- heap space to commit
    writeW32 b 0;                                      -- loader flags
    writeW32 b 16;                                     -- number of data directory entries to follow

    writeW32 b export_rva; writeW32 b export_size;     -- 00 [export directory]
    writeW32 b import_rva; writeW32 b import_size;     -- 01 [import directory]
    writeW32 b res_rva;    writeW32 b res_size;        -- 02 [resource directory]
    writeW32 b 0;          writeW32 b 0;               -- 03 [exception directory]
    writeW32 b 0;          writeW32 b 0;               -- 04 [security directory]
    writeW32 b 0;          writeW32 b 0;               -- 05 [base relocation directory]
    writeW32 b 0;          writeW32 b 0;               -- 06 [debug directory]
    writeW32 b 0;          writeW32 b 0;               -- 07 [processor-specific directory]
    writeW32 b 0;          writeW32 b 0;               -- 08 [global pointer directory (?)]
    writeW32 b 0;          writeW32 b 0;               -- 09 [TLS directory]
    writeW32 b 0;          writeW32 b 0;               -- 10 [load config directory]
    writeW32 b 0;          writeW32 b 0;               -- 11 [bound import directory]
    writeW32 b iat_rva;    writeW32 b iat_size;        -- 12 [IAT directory]
    writeW32 b 0;          writeW32 b 0;               -- 13 [delay-load directory]
    writeW32 b 0;          writeW32 b 0;               -- 14 [COM runtime directory]
    writeW32 b 0;          writeW32 b 0;               -- 15 [??? directory]

-- Write all section data
writeSections :: FileBuf -> Int -> Int -> [Sect] -> IO ()
writeSections b file_alignment sect_alignment sects = do
    -- write section headers
    foldlM' (0, 0) sects $ \(foffset, soffset) sect -> do
        let fsize = align (length (sectData sect)) file_alignment;
        let ssize = align (length (sectData sect)) sect_alignment;
        
        writeBuf b (chbuf (sectName sect) 8);
        writeW32 b $ castn (length (sectData sect));
        writeW32 b $ castn (soffset + rvaBase (length sects) sect_alignment);
        writeW32 b $ castn fsize;
        writeW32 b $ castn (foffset + rvaBase (length sects) file_alignment);
        writeW32 b 0;
        writeW32 b 0;
        writeW16 b 0;
        writeW16 b 0;
        writeW32 b (sectionCharacteristics sect);
        
        return (foffset + fsize, soffset + ssize)
    
    -- add padding to align the section table "section" on file alignment
    writePadding b $ (rvaBase (length sects) file_alignment) - (rvaBase (length sects) 1)
    
    -- finally, write actual section data
    forM_ sects $ \s -> do
        let dsize = length (sectData s);
        writeBuf b (sectData s);
        writePadding b $ (align dsize file_alignment) - dsize;

-- PE-header flags for a section
sectionCharacteristics :: Sect -> Word32
sectionCharacteristics s = code_bits .|. data_bits .|. read_bits .|. write_bits where
    sflags     = sectFlags s
    code_bits  = sel Code  (cIMAGE_SCN_CNT_CODE .|. cIMAGE_SCN_MEM_READ .|. cIMAGE_SCN_MEM_EXECUTE)
    data_bits  = sel Data  (cIMAGE_SCN_CNT_INITIALIZED_DATA .|. cIMAGE_SCN_MEM_READ)
    read_bits  = sel Read  cIMAGE_SCN_MEM_READ
    write_bits = sel Write cIMAGE_SCN_MEM_WRITE
    sel c bits = if c `elem` sflags then bits else 0
    
    cIMAGE_SCN_CNT_CODE             = 0x00000020
    cIMAGE_SCN_CNT_INITIALIZED_DATA = 0x00000040
    cIMAGE_SCN_MEM_READ             = 0x40000000
    cIMAGE_SCN_MEM_WRITE            = 0x80000000
    cIMAGE_SCN_MEM_EXECUTE          = 0x20000000
        
-- Write padding bytes
writePadding :: FileBuf -> Int -> IO ()
writePadding b n | n <= 0 = return ()
writePadding b n          = writeBuf b (take n $ repeat 0)
        
-- Write the DOS stub portion of an executable
writeDOSStub :: FileBuf -> IO ()
writeDOSStub b = do
    writeBufs b $ map w16 $
        [
            0x5A4D, 0x0090, 0x0003, 0x0000, 0x0004, 0x0000, 0xFFFF, 0x0000, 0x00B8, 0x0000, 0x0000,
            0x0000, 0x0040, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
            0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
        ];
    writeW32 b 0x000000E0;
    writeBuf b $
        [
            0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,
            0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,
            0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E, 0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
            0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0xEC, 0x85, 0x5B, 0xA1, 0xA8, 0xE4, 0x35, 0xF2, 0xA8, 0xE4, 0x35, 0xF2, 0xA8, 0xE4, 0x35, 0xF2,
            0x6B, 0xEB, 0x3A, 0xF2, 0xA9, 0xE4, 0x35, 0xF2, 0x6B, 0xEB, 0x55, 0xF2, 0xA9, 0xE4, 0x35, 0xF2,
            0x6B, 0xEB, 0x68, 0xF2, 0xBB, 0xE4, 0x35, 0xF2, 0xA8, 0xE4, 0x34, 0xF2, 0x63, 0xE4, 0x35, 0xF2,
            0x6B, 0xEB, 0x6B, 0xF2, 0xA9, 0xE4, 0x35, 0xF2, 0x6B, 0xEB, 0x6A, 0xF2, 0xBF, 0xE4, 0x35, 0xF2,
            0x6B, 0xEB, 0x6F, 0xF2, 0xA9, 0xE4, 0x35, 0xF2, 0x52, 0x69, 0x63, 0x68, 0xA8, 0xE4, 0x35, 0xF2,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ];

-- perform the actual linking step of merging several sections together
data GSection  = GSection Sect Int [Symbol] deriving (Eq, Show)
type GSections = Map.Map String [GSection]

instance Ord GSection where
    compare (GSection s0 _ _) (GSection s1 _ _) = compare (sectName s0) (sectName s1)

coalesceSections :: Int -> Int -> GSections -> [Sect]
coalesceSections sbase salignment gsects = reverse $ second $ foldl addSection (0, []) (Map.toList gsects) where
    addSection (n, ss) (sn, gs@((GSection g _ _):_)) = (n', s:ss) where
        n' = n + (castn $ align (gsectionsSize gs) salignment)
        s  = Sect sn (coalesceGSects n gs) [] (sectFlags g)
        
    coalesceGSects n gs = second $ foldl (cgStep n) (0, []) gs
    cgStep cbase (n, bs) (GSection s i syms) = (n', bs'') where
        n'   = n + (castn $ length (sectData s))
        bs'  = bs ++ sectData s
        bs'' = foldl applyFixup bs' (sectFixups s)
        applyFixup bs (Fixup va symIdx fty) = updW32 bs (castn $ n + va) (castn $ fixupVal fty) where
            fval = findSymbol salignment gsects (symName $ syms !! castn symIdx)
            fixupVal Relative    = fval - (castn $ cbase + n + va + 4)
            fixupVal Absolute    = fval + sbase
            fixupVal RVAAbsolute = fval + rvaBase (Map.size gsects) salignment

findSymbol :: Int -> GSections -> String -> Int
findSymbol salignment gsects sym = suj (maybeFindSymbol salignment gsects sym) error_message where
    error_message = "Error: Undefined symbol: " ++ sym

maybeFindSymbol :: Int -> GSections -> String -> Maybe Int
maybeFindSymbol salignment gsects sym = search_gsections_seq 0 (Map.toList gsects) where
    search_gsections_seq o [] = Nothing
    search_gsections_seq o ((s, gs):ss) = case search_gsections o gs of
        Just x  -> Just x
        Nothing -> search_gsections_seq (align (o + gsectionsSize gs) salignment) ss
        
    search_gsections o []     = Nothing
    search_gsections o (g:gs) = case search_gsection g of
        Just x  -> Just (o + x)
        Nothing -> search_gsections (o + gsectionSize g) gs
        
    search_gsection (GSection sect i syms) = fmap symValue $ mhead syms' where
        syms' = [s | s <- syms, symName s == sym, symSectIndex s == i, symType s == Export]

rvaBase :: (Integral a, Integral b) => a -> b -> b
rvaBase scount alignment = align (0xE0 + 248 + (scount * 40)) alignment

gatherSections :: [CObj] -> GSections
gatherSections objs = Map.fromList [(sn, gatherSection objs sn) | sn <- sectionNames objs]

gatherSection :: [CObj] -> String -> [GSection]
gatherSection objs sn = List.sort [gsect | obj <- objs, gsect <- baseNamedGlobalizedSections sn obj]

baseNamedGlobalizedSections :: String -> CObj -> [GSection]
baseNamedGlobalizedSections bn obj = [gsect | gsect <- globalizedSections obj, baseGSectionName gsect == bn]

globalizedSections :: CObj -> [GSection]
globalizedSections obj = [GSection s i (objectSymbols obj) | (s, i) <- zip (objectSections obj) [0..]]

baseSectionName :: Sect -> String
baseSectionName s = first (lsplit (sectName s) "$")

baseGSectionName :: GSection -> String
baseGSectionName (GSection s _ _) = baseSectionName s

gsectionsSize :: [GSection] -> Int
gsectionsSize gs = sum $ map gsectionSize gs

gsectionSize :: GSection -> Int
gsectionSize (GSection s _ _) = length (sectData s)

sectionNames :: [CObj] -> [String]
sectionNames objs = unique [baseSectionName s | obj <- objs, s <- objectSections obj]
