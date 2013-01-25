{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Link.Linux.ELF where
import qualified Link.Linux.ELFFile as ELFF
import qualified Link.Linux.ELFStructs as ELFT
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
    let section_alignment = 0x00000010;
    let image_base        = 0x08040000;
    let gsects            = gatherSections objs;
    let section_base      = rvaBase (Map.size gsects) section_alignment;
    let linked_sections   = coalesceSections (0x6C + image_base + section_base) section_alignment gsects;
    let entryPoint        = castn $ findSymbol section_alignment gsects "main";

    let esects = [ELFF.ELFSection {ELFF.sectName=sectName s, ELFF.sectType=ELFT.STProgBits, ELFF.sectFlags=[ELFT.SFAlloc,ELFT.SFExecInstr], ELFF.sectLink=Nothing, ELFF.sectInfo=0, ELFF.sectAddrAlign=castn section_alignment, ELFF.sectEntrySize=0, ELFF.sectData=sectData s} | s <- linked_sections];
    let tpseg  = ELFF.ELFSegment {ELFF.segType=ELFT.SegTLoad, ELFF.segVAddr=castn image_base, ELFF.segPAddr=castn image_base, ELFF.segFlags=[ELFT.SegFX,ELFT.SegFR], ELFF.segAlign=0x1000, ELFF.segSectionNames=map ELFF.sectName esects};
    let f      = ELFF.ELFFile (ELFF.ELFFInit {ELFF.fType=ELFT.FTExec,ELFF.fMachine=ELFT.MA386,ELFF.fVersion=ELFT.FVCurrent,ELFF.fFlags=0}) [tpseg] esects;
    
    ELFF.writeELFFile fpath f (image_base + entryPoint)
    
-- Write padding bytes
writePadding :: FileBuf -> Int -> IO ()
writePadding b n | n <= 0 = return ()
writePadding b n          = writeBuf b (take n $ repeat 0)

-- perform the actual linking step of merging several sections together
data GSection  = GSection Sect Int [Symbol] deriving Eq
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
rvaBase scount _ = castn $ ELFT.sizeofHeader + ELFT.sizeofSegHeader * 1 + ELFT.sizeofSectHeader * (castn scount)

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
