{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Link.Windows.DLL where
import Link.ObjFile
import Util.FileBuf
import Util.Num
import Util.Sequence

import Control.Monad
import System.IO
import Data.Bits
import GHC.Word

-- the main import object must be included to complete the linking process
mainImportObject :: CObj
mainImportObject = CObj [s0z, s2z] [iend, iatend] where
    s0z    = Sect   ".idata$0z" [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] [] [Code, Write]
    s2z    = Sect   ".idata$2z" [] [] [Code, Write]
    iend   = Symbol "$import_end" 20 0 Export Internal
    iatend = Symbol "$iat_end"    0  1 Export Internal

-- import table support
importObject :: String -> [String] -> CObj
importObject dllname symnames = CObj sects (pfxsyms ++ impsyms) where
    symcount    = length symnames
    iatSymOff i = castn $ (castn i) + length pfxsyms
    ictSymOff i = castn $ (castn i) + iatSymOff symcount
    sects       = [ict, ift, lname, iat, int, ifnnames]
    pfxsyms     =
        [
            Symbol ("$" ++ dllname)           0 2 Export Internal,
            Symbol ("$" ++ dllname ++ "_iat") 0 3 Export Internal,
            Symbol ("$" ++ dllname ++ "_int") 0 4 Export Internal,
            Symbol ("$import_begin")          0 1 Export Internal,
            Symbol ("$iat_begin")             0 3 Export Internal
        ]
    impsyms =
        [Symbol ("$_ent_" ++ s) (castn o) 5 Export Internal | (_, o, s) <- tail (scanl (\(o, _, _) s -> (o + 2 + length s + 1, o, s)) (0,0,"") symnames)] ++
        [Symbol ("$_imp_" ++ s) (4 * i)   3 Export Internal | (s, i) <- zip symnames [0..]] ++
        [Symbol s               (6 * i)   0 Export Internal | (s, i) <- zip symnames [0..]]
    
    -- a jump table for imported functions
    ict =
        Sect ".text"
            -- jmp DWORD PTR [import_sym]
            (concat [[0xff, 0x25, 0x00, 0x00, 0x00, 0x00] | s <- symnames])
            [Fixup (castn $ (6 * i) + 2) (ictSymOff i) Absolute | (s, i) <- zip symnames [0..]]
            [Code]
            
    -- the import function table header
    ift =
        Sect ".idata$0"
            [
                0x00, 0x00, 0x00, 0x00, -- RVA to INT
                0x00, 0x00, 0x00, 0x00, -- 'timestamp' (must be 0)
                0xff, 0xff, 0xff, 0xff, -- no forwarder chain (-1)
                0x00, 0x00, 0x00, 0x00, -- RVA to dll name
                0x00, 0x00, 0x00, 0x00  -- RVA to IAT
            ]
            [
                Fixup 0  2 RVAAbsolute,
                Fixup 12 0 RVAAbsolute,
                Fixup 16 1 RVAAbsolute
            ]
            [Code, Write]
            
    -- the DLL name
    lname =
        Sect ".idata$1"
            (str dllname)
            []
            [Code, Write]
            
    -- the import address table
    iat =
        Sect ".idata$2"
            -- records for each function patched at runtime (terminated by a null dword)
            (concat [[0x00, 0x00, 0x00, 0x00] | s <- symnames] ++ [0x00, 0x00, 0x00, 0x00])
            [Fixup (i * 4) (iatSymOff i) RVAAbsolute | (s, i) <- zip symnames [0..]]
            [Code, Write]
            
    -- the import name table (a copy of the import address table)
    int =
        Sect ".idata$3"
            -- records for each function patched at runtime (terminated by a null dword)
            (concat [[0x00, 0x00, 0x00, 0x00] | s <- symnames] ++ [0x00, 0x00, 0x00, 0x00])
            [Fixup (i * 4) (iatSymOff i) RVAAbsolute | (s, i) <- zip symnames [0..]]
            [Code, Write]
            
    -- the imported function names
    ifnnames =
        Sect ".idata$6"
        (concat [[0x00, 0x00] ++ str s | s <- symnames])
        []
        [Code, Write]

-- export table support
withExportObject :: [CObj] -> IO [CObj]
withExportObject cobjs = if null syms then return cobjs else do { o <- makeExportObject syms; return (o:cobjs) } where
    syms = unique [symName s | o <- cobjs, s <- objectSymbols o, symStorageClass s == External]

makeExportObject :: [String] -> IO CObj
makeExportObject esyms = do
    ct <- ctime;
    let (_, edata, fixups, syms) = foldl merge_seg (0, [], [], []) (mdata ct);
    return $ CObj [Sect ".edata" edata [] [Data]] (reverse syms)
    where
    merge_seg (o, edata, fixups, syms) (rdata, Nothing)          = (o + length rdata, edata ++ rdata, fixups, syms)
    merge_seg (o, edata, fixups, syms) (rdata, Just (Left  sym)) = (o + length rdata, edata ++ rdata, fixups, (Symbol sym (castn o) 0 Export Internal):syms)
    merge_seg (o, edata, fixups, syms) (rdata, Just (Right sym)) = (o + length rdata, edata ++ rdata, (Fixup (castn o) (castn $ length syms) RVAAbsolute):fixups, (Symbol sym (castn o) 0 Import Internal):syms)
    sym s    = Just $ Left s
    fixup s  = Just $ Right s
    no_sym   = Nothing
    mdata ct =
        [
            (w32 0,  sym "$export_begin"),
            (w32 ct, no_sym),
            (w16 0,  no_sym),
            (w16 0,  no_sym),

            -- add RVA to DLL name
            (w32 0,  fixup "$export_DLLNAME"),

            (w32 0, no_sym),
            (cw32 (length esyms), no_sym),
            (cw32 (length esyms), no_sym),

            -- add RVA to the EAT
            (w32 0, fixup "$export_EAT"),

            -- add RVA to the ENT
            (w32 0, fixup "$export_ENT"),

            -- add RVA to the EOT
            (w32 0, fixup "$export_EOT")
        ] ++
        -- write the EAT
        [([], sym "$export_EAT")] ++
        [(w32 0, fixup s) | s <- esyms] ++
        
        -- write the ENT
        [([], sym "$export_ENT")] ++
        [(w32 0, fixup ("$export_" ++ s ++ "_name")) | s <- esyms] ++
        
        -- write the EOT
        [([], sym "$export_EOT")] ++
        [(cw32 i, no_sym) | i <- [0..length esyms - 1]] ++ 

        -- write the DLL name
        [(str "JIMMY.DLL", sym "$export_DLLNAME")] ++

        -- write the ENT
        [(str s, sym ("$export_" ++ s ++ "_name")) | s <- esyms] ++

        -- mark the end of the export table
        [([], sym "$export_end")]
