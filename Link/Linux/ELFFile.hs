module Link.Linux.ELFFile where
import Link.Linux.ELFStructs
import Util.FileBuf
import Util.Num
import Util.Sequence

import qualified Data.Map  as Map
import qualified Data.List as List
import Control.Monad
import System.IO
import Data.Bits
import Data.Char
import GHC.Word

data ELFFile    = ELFFile ELFFInit [ELFSegment] [ELFSection] deriving (Eq, Show)
data ELFFInit   = ELFFInit { fType :: ELFFileType, fMachine :: ELFMachArch, fVersion :: ELFFileVersion, fFlags :: ELFWord } deriving (Eq, Show)
data ELFSegment = ELFSegment { segType :: ELFSegmentType, segVAddr :: ELFAddr, segPAddr :: ELFAddr, segFlags :: [ELFSegFlag], segAlign :: ELFWord, segSectionNames :: [String] } deriving (Eq, Show)
data ELFSection = ELFSection { sectName :: String, sectType :: ELFSectType, sectFlags :: [ELFSectFlag], sectLink :: Maybe Int, sectInfo :: ELFWord, sectAddrAlign :: ELFWord, sectEntrySize :: ELFWord, sectData :: [Word8] } deriving (Eq, Show)

writeELFFile :: String -> ELFFile -> Int -> IO ()
writeELFFile fpath (ELFFile finit segs sects') entryPoint = do
    b <- openBuf fpath WriteMode;
    let nullSect   = ELFSection {sectName="",sectType=STNull,sectFlags=[],sectLink=Nothing,sectInfo=0,sectAddrAlign=0,sectEntrySize=0, sectData=[]};
    let strtabSect = ELFSection {sectName=".shstrtab",sectType=STStrTab,sectFlags=[],sectLink=Nothing,sectInfo=0,sectAddrAlign=0,sectEntrySize=0, sectData=[]};
    let sects''    = nullSect : defaultSection sects' strtabSect;
    
    let (strtabSect', nameIdxs) = addSectNames (uj $ namedSection sects'' ".shstrtab") sects'';
    let sects                   = replaceSection sects'' ".shstrtab" strtabSect';
    let f                       = ELFFile finit segs sects;
    
    let isects     = zip sects [0..];
    let sectDefMap = Map.fromList [(i, elfRefSegment f sect) | (sect, i) <- isects, hasRefSegment f sect];
    
    -- write the ELF header
    writeBuf b $ serialize (elfFileHeader f entryPoint);
    
    -- write each segment
    forM_ segs $ \seg -> do
        writeBuf b $ serialize (elfSegHeader f seg)
    
    -- write each section
    forM_ (zip3 sects [0..] nameIdxs) $ \(sect,i,nameIdx) -> do
        let sAddr = meither (\rseg -> segSectAddress f rseg sect) 0 $ Map.lookup i sectDefMap;
        
        writeBuf b $ serialize $ elfSectHeader f sect sAddr nameIdx (meither id 0 $ sectLink sect);
        atLoc b (elfFileSectOffset f i) (writeBuf b (sectData sect))

    -- and we've finished the file
    closeBuf b;
    return ()

elfFileHeader :: ELFFile -> Int -> ELFHeader
elfFileHeader f@(ELFFile finit segs sects) entryPoint = h where
    h          = ELFHeader { htype=htype, hmachine=hmachine, hversion=hversion, hentry=hentry, hphoff=hphoff, hshoff=hshoff, hflags=hflags, hehsize=hehsize, hphentsize=hphentsize, hphnum=hphnum, hshentsize=hshentsize, hshnum=hshnum, hshstrndx=hshstrndx }
    htype      = fType finit
    hmachine   = fMachine finit
    hversion   = fVersion finit
    hentry     = castn $ entryPoint + segDisplacement f (elfRefSegment f (uj $ namedSection sects ".text"))
    hphoff     = castn $ if length segs == 0 then 0 else sizeofHeader
    hshoff     = castn $ sizeofHeader + sizeofSegHeader * (castn $ length segs)
    hflags     = castn $ fFlags finit
    hehsize    = castn $ sizeofHeader
    hphentsize = castn $ sizeofSegHeader
    hphnum     = castn $ length segs
    hshentsize = castn $ sizeofSectHeader
    hshnum     = castn $ length sects
    hshstrndx  = castn $ sectIndex sects ".shstrtab"

elfSegHeader :: ELFFile -> ELFSegment -> ELFSegHeader
elfSegHeader f@(ELFFile finit segs sects) seg = h where
    h       = ELFSegHeader  { gtype=gtype, goffset=goffset, gvaddr=gvaddr, gpaddr=gpaddr, gfilesz=castn gfilesz, gmemsz=castn gmemsz, gflags=gflags, galign=galign }
    gtype   = segType seg
    goffset = intAlign firstoff galign
    gvaddr  = segVAddr seg
    gpaddr  = segPAddr seg
    gfilesz = elfFileSectOffset f lastsecti + elfSectSize lastsect + segDisplacement f seg
    gmemsz  = gfilesz + sum [elfSectSize sect | sect <- segSects f seg, emptySect sect]
    gflags  = castn $ bor $ map (r32 . serialize) (segFlags seg)
    galign  = segAlign seg
    sectidxs   = map (sectIndex sects) (segSectionNames seg)
    firstoff   = meither (elfFileSectOffset f) 0 $ mhead sectidxs
    lastsecti  = last sectidxs
    lastsect   = sects !! lastsecti

elfSectHeader :: ELFFile -> ELFSection -> Int -> Int -> Int -> ELFSectHeader
elfSectHeader f@(ELFFile finit segs sects) sect sectAddr nameIndex linkSect = h where
    h           = ELFSectHeader { shname=shname, shtype=shtype, shflags=shflags, shaddr=shaddr, shoffset=shoffset, shsize=shsize, shlink=shlink, shinfo=shinfo, shaddralign=shaddralign, shentsize=shentsize }
    shname      = castn nameIndex
    shtype      = sectType sect
    shflags     = castn $ bor $ map (r32 . serialize) (sectFlags sect)
    shaddr      = castn sectAddr
    shoffset    = if emptySect sect then 0 else elfFileSectOffset f (at sect sects)
    shsize      = castn $ length (sectData sect)
    shlink      = castn linkSect
    shinfo      = sectInfo sect
    shaddralign = sectAddrAlign sect
    shentsize   = sectEntrySize sect
    
elfRefSegment :: ELFFile -> ELFSection -> ELFSegment
elfRefSegment f@(ELFFile _ segs sects) sect = selectSeg [g | g <- segs, sect `elem` segSects f g] where
    selectSeg []    = error "Internal linker error (ELF section unreferenced by ELF segment)."
    selectSeg (x:_) = x
    
hasRefSegment :: ELFFile -> ELFSection -> Bool
hasRefSegment f@(ELFFile _ segs sects) sect = selectSeg [g | g <- segs, sect `elem` segSects f g] where
    selectSeg []    = False
    selectSeg _     = True
    
elfFileSectOffset :: Num n => ELFFile -> Int -> n
elfFileSectOffset (ELFFile _ segs sects) i = castn $ off where
    segCount  = castn (length segs)
    sectCount = castn (length sects)
    initOff   = sizeofHeader + sizeofSegHeader * segCount + sizeofSectHeader * sectCount
    
    sect = sects !! i
    off  = leadingOffset + paddOff sect leadingOffset
    
    leadingOffset         = foldl accumOffset initOff (take i sects)
    accumOffset sum sect' = sum + paddOff sect' sum + (castn $ elfSectSize sect')
    
    paddOff sect r = if offNecessary then align - alignoff else 0 where
        align        = castn $ sectAddrAlign sect
        offNecessary = align > 1 && alignoff > 0 && not (emptySect sect)
        alignoff     = r `mod` align

segSects :: ELFFile -> ELFSegment -> [ELFSection]
segSects (ELFFile _ _ sects) seg = [uj $ namedSection sects n | n <- segSectionNames seg]

namedSection :: [ELFSection] -> String -> Maybe ELFSection
namedSection sects n = select [sect | sect <- sects, sectName sect == n] where
    select []    = Nothing
    select (s:_) = Just s
    
sectIndex :: [ELFSection] -> String -> Int
sectIndex sects n = atBy (((==) n) . sectName) sects

replaceSection :: [ELFSection] -> String -> ELFSection -> [ELFSection]
replaceSection sects n sect = upd sects where
    upd (sect':sects') | sectName sect' == n = sect  : sects'
    upd (sect':sects')                       = sect' : upd sects'
    upd []                                   = []
    
defaultSection :: [ELFSection] -> ELFSection -> [ELFSection]
defaultSection sects sect = select (namedSection sects (sectName sect)) where
    select (Just _) = sects
    select Nothing  = sect:sects

segSectAddress :: ELFFile -> ELFSegment -> ELFSection -> Int
segSectAddress f@(ELFFile _ segs sects) seg sect = accum (castn $ segPAddr seg) (segSects f seg) where
    accum n (sect':_) | sect == sect' = n + off
    accum n (sect':sects')            = accum (n + length (sectData sect')) sects'
    accum n []                        = n
    off                               = segDisplacement f seg

addSectData :: ELFSection -> [Word8] -> ELFSection
addSectData sect d = sect {sectData=sectData sect ++ d}

addSectNames :: ELFSection -> [ELFSection] -> (ELFSection, [Int])
addSectNames strtbl sects = (strtbl', reverse snidxs) where
    (strtbl', snidxs, _) = foldl addSectName (strtbl, [], length (sectData strtbl)) sects
    
    addSectName (stbl, idxs, off) sect = (addSectData stbl sname, off : idxs, off + length sname) where
        sname = str $ sectName sect
    
segDisplacement :: ELFFile -> ELFSegment -> Int
segDisplacement f@(ELFFile _ _ sects) seg = castn $ firstoff - goffset where
    salign   = if segAlign seg > 0 then segAlign seg else 1
    goffset  = intAlign firstoff salign
    firstoff = elfFileSectOffset f (head (map (sectIndex sects) $ segSectionNames seg))
    
elfSectSize :: ELFSection -> Int
elfSectSize = length . sectData

emptySect :: ELFSection -> Bool
emptySect sect = (sectType sect) `elem` [STNull, STNoBits]