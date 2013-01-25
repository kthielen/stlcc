module Link.Linux.ELFStructs where
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

data ELFFileType       = FTNone | FTRel | FTExec | FTDyn | FTCore | FTLoos | FTHios | FTLoproc | FTHiproc deriving (Show, Eq)
data ELFMachArch       = MANone | MAM32 | MASparc | MA386 | MA68K | MA88K | MA860 | MAMIPS | MAS370 | MAMIPS_RS3_LE | MAPARISC | MAVPP500 | MASPARC32PLUS | MA960 | MAPPC | MAPPC64 | MAS390 | MAV800 | MAFR20 | MARH32 | MARCE | MAARM | MAALPHA | MASH | MASPARCV9 | MATRICORE | MAARC | MAH8_300 | MAH8_300H | MAH8S | MAH8_500 | MAIA_64 | MAMIPS_X | MACOLDFIRE | MA68HC12 | MAMMA | MAPCP | MANCPU | MANDR1 | MASTARCORE | MAME16 | MAST100 | MATINYJ | MAX86_64 | MAPDSP | MAFX66 | MAST9PLUS | MAST7 | MA68HC16 | MA68HC11 | MA68HC08 | MA68HC05 | MASVX | MAST19 | MAVAX | MACRIS | MAJAVELIN | MAFIREPATH | MAZSP | MAMMIX | MAHUANY | MAPRISM | MAAVR | MAFR30 | MAD10V | MAD30V | MAV850 | MAM32R | MAMN10300 | MAMN10200 | MAPJ | MAOPENRISC | MAARC_A5 | MAXTENSA deriving (Show, Eq)
data ELFFileVersion    = FVNone | FVCurrent deriving (Show, Eq)
data ELFIDIndex        = MAG0 | MAG1 | MAG2 | MAG3 | CLASS | DATA | VERSION | OSABI | ABIVERSION | PAD | NIDENT deriving (Show, Eq)
data ELFFileClass      = FCNone | FC32 | FC64 deriving (Eq, Show)
data ELFByteOrder      = BONone | BOLittleEndian | BOBigEndian deriving (Eq, Show)
data ELFOS             = OSNone | OSHPUX | OSNetBSD | OSLinux | OSSolaris | OSAIX | OSIRIX | OSFreeBSD | OSTRU64 | OSModesto | OSOpenBSD deriving (Eq, Show)
data ELFSectIndex      = SINone | SILOReserve | SILOProc | SIHIProc | SILOOS | SIHIOS | SIAbs | SICommon | SIXIndex | SIHIReserve deriving (Eq, Show)
data ELFSectType       = STNull | STProgBits | STSymTab | STStrTab | STRela | STHash | STDynamic | STNote | STNoBits | STRel | STSHLIB | STDynSym | STInitArray | STFinArray | STPreInitArray | STGroup | STSymTabSHNdx | STLOOS | STHIOS | STLOProc | STHIProc | STLOUser | STHIUser deriving (Eq, Show)
data ELFSectFlag       = SFWrite | SFAlloc | SFExecInstr | SFMerge | SFStrings | SFInfoLink | SFLinkOrder | SFOSNonConforming | SFGroup | SFTLS | SFMaskOS | SFMaskProc deriving (Eq, Show)
data ELFGroupFlag      = GFComDat | GFMaskOS | GFMaskProc deriving (Eq, Show)
data ELFSymBinding     = SBLocal | SBGlobal | SBWeak | SBLoOS | SBHiOS | SBLoProc | SBHiProc deriving (Eq, Show)
data ELFSymType        = SYTNone | SYTObject | SYTFunc | SYTSection | SYTFile | SYTCommon | SYTTLS | SYTLoOS | SYTHiOS | SYTLoProc | SYTHiProc deriving (Eq, Show)
data ELFSymVisibility  = SYVDefault | SYVInternal | SYVHidden | SYVProtected deriving (Eq, Show)
data ELFRelocationType = RT386None | RT38632 | RT386PC32 | RT386GOT32 | RT386PLT32 | RT386COPY | RT386GLOB_DAT | RT386JMP_SLOT | RT386RELATIVE | RT386GOTOFF | RT386GOTPC deriving (Eq, Show)
data ELFSegmentType    = SegTNull | SegTLoad | SegTDynamic | SegTInterp | SegTNote | SegTSHLib | SegTPHDR | SegTTLS | SegTLoOS | SegTHiOS | SegTLoProc | SegTHiProc deriving (Eq, Show)
data ELFSegFlag        = SegFX | SegFW | SegFR | SegFMaskOS | SegFMaskProc deriving (Eq, Show)
data ELFDynArray       = DTNull | DTNeeded | DTPLTRelsz | DTPLTGot | DTHash | DTStrTab | DTSymTab | DTRela | DTRelaSZ | DTRelaNT | DTStrSZ | DTSymENT | DTInit | DTFini | DTSOName | DTRPath | DTSymbolic | DTRel | DTRelSZ | DTRelENT | DTPLTRel | DTDebug | DTTExtRel | DTJMPRel | DTBindNow | DTInitArray | DTFiniArray | DTInitArraySZ | DTFiniArraySZ | DTRunPath | DTFlags | DTEncoding | DTPreInitArray | DTPreInitArraySZ | DTLoOS | DTHiOS | DTLoProc | DTHiProc deriving (Eq, Show)
data ELFDTFlag         = DFOrigin | DFSymbolic | DFTextRel | DFBindNow | DFStaticTLS deriving (Eq, Show)
data ELFHeader         = ELFHeader     { htype :: ELFFileType, hmachine :: ELFMachArch, hversion :: ELFFileVersion, hentry :: ELFAddr, hphoff :: ELFOff, hshoff :: ELFWord, hflags :: ELFWord, hehsize :: ELFHalf, hphentsize :: ELFHalf, hphnum :: ELFHalf, hshentsize :: ELFHalf, hshnum :: ELFHalf, hshstrndx :: ELFHalf } deriving (Eq, Show)
data ELFSectHeader     = ELFSectHeader { shname :: ELFWord, shtype :: ELFSectType, shflags :: ELFWord, shaddr :: ELFAddr, shoffset :: ELFOff, shsize :: ELFWord, shlink :: ELFWord, shinfo :: ELFWord, shaddralign :: ELFWord, shentsize :: ELFWord } deriving (Eq, Show)
data ELFSegHeader      = ELFSegHeader  { gtype :: ELFSegmentType, goffset :: ELFOff, gvaddr :: ELFAddr, gpaddr :: ELFAddr, gfilesz :: ELFWord, gmemsz :: ELFWord, gflags :: ELFWord, galign :: ELFWord } deriving (Eq, Show)
data ELFSymTabEnt      = ELFSymTabEnt  { symname :: ELFWord, symvalue :: ELFAddr, symsize :: ELFWord, syminfo :: Word8, symother :: Word8, symshndx :: ELFHalf } deriving (Eq, Show)
data ELFRelEnt         = ELFRelEnt     { reoffset :: ELFAddr, reinfo :: ELFWord } deriving (Eq, Show)
data ELFRelaEnt        = ELFRelaEnt    { raoffset :: ELFAddr, rainfo :: ELFWord, raaddend :: ELFSword } deriving (Eq, Show)

type ELFAddr  = Word32
type ELFHalf  = Word16
type ELFOff   = Word32
type ELFWord  = Word32
type ELFSword = Int

sizeofELFAddr  = 4
sizeofELFHalf  = 2
sizeofELFOff   = 4
sizeofELFWord  = 4
sizeofELFSword = 4
sizeofWord8    = 1

sizeofHeader     = castn $ length $ serialize $ ELFHeader     {htype=FTNone,hmachine=MANone,hversion=FVNone,hentry=0,hphoff=0,hshoff=0,hflags=0,hehsize=0,hphentsize=0,hphnum=0,hshentsize=0,hshnum=0,hshstrndx=0}
sizeofSectHeader = castn $ length $ serialize $ ELFSectHeader {shname=0,shtype=STNull,shflags=0,shaddr=0,shoffset=0,shsize=0,shlink=0,shinfo=0,shaddralign=0,shentsize=0}
sizeofSegHeader  = castn $ length $ serialize $ ELFSegHeader  {gtype=SegTNull,goffset=0,gvaddr=0,gpaddr=0,gfilesz=0,gmemsz=0,gflags=0,galign=0}
sizeofSymTabEnt  = castn $ length $ serialize $ ELFSymTabEnt  {symname=0,symvalue=0,symsize=0,syminfo=0,symother=0,symshndx=0}
sizeofRelEnt     = castn $ length $ serialize $ ELFRelEnt     {reoffset=0,reinfo=0}
sizeofRelaEnt    = castn $ length $ serialize $ ELFRelaEnt    {raoffset=0,rainfo=0,raaddend=0}

instance Serializable Int where
    serialize x = serialize ((castn x) :: Word32)

instance Serializable ELFFileType where
    serialize FTNone   = cw16 $ 0      -- ET_NONE
    serialize FTRel    = cw16 $ 1      -- ET_REL
    serialize FTExec   = cw16 $ 2      -- ET_EXEC
    serialize FTDyn    = cw16 $ 3      -- ET_DYN
    serialize FTCore   = cw16 $ 4      -- ET_CORE
    serialize FTLoos   = cw16 $ 0xFE00 -- ET_LOOS
    serialize FTHios   = cw16 $ 0xFEFF -- ET_HIOS
    serialize FTLoproc = cw16 $ 0xFF00 -- ET_LOPROC
    serialize FTHiproc = cw16 $ 0xFFFF -- ET_HIPROC
    
instance Serializable ELFMachArch where
    serialize MANone         = cw16 $  0 -- No machine
    serialize MAM32          = cw16 $  1 -- AT&T WE 32100
    serialize MASparc        = cw16 $  2 -- SPARC
    serialize MA386          = cw16 $  3 -- Intel 80386
    serialize MA68K          = cw16 $  4 -- Motorola 68000
    serialize MA88K          = cw16 $  5 -- Motorola 88000
    serialize MA860          = cw16 $  7 -- Intel 80860
    serialize MAMIPS         = cw16 $  8 -- MIPS I Architecture
    serialize MAS370         = cw16 $  9 -- IBM System/370 Processor
    serialize MAMIPS_RS3_LE  = cw16 $ 10 -- MIPS RS3000 Little-endian
    serialize MAPARISC       = cw16 $ 15 -- Hewlett-Packard PA-RISC
    serialize MAVPP500       = cw16 $ 17 -- Fujitsu VPP500
    serialize MASPARC32PLUS  = cw16 $ 18 -- Enhanced instruction set SPARC
    serialize MA960          = cw16 $ 19 -- Intel 80960
    serialize MAPPC          = cw16 $ 20 -- PowerPC
    serialize MAPPC64        = cw16 $ 21 -- 64-bit PowerPC
    serialize MAS390         = cw16 $ 22 -- IBM System/390 Processor
    serialize MAV800         = cw16 $ 36 -- NEC V800
    serialize MAFR20         = cw16 $ 37 -- Fujitsu FR20
    serialize MARH32         = cw16 $ 38 -- TRW RH-32
    serialize MARCE          = cw16 $ 39 -- Motorola RCE
    serialize MAARM          = cw16 $ 40 -- Advanced RISC Machines ARM
    serialize MAALPHA        = cw16 $ 41 -- Digital Alpha
    serialize MASH           = cw16 $ 42 -- Hitachi SH
    serialize MASPARCV9      = cw16 $ 43 -- SPARC Version 9
    serialize MATRICORE      = cw16 $ 44 -- Siemens TriCore embedded processor
    serialize MAARC          = cw16 $ 45 -- Argonaut RISC Core, Argonaut Technologies Inc.
    serialize MAH8_300       = cw16 $ 46 -- Hitachi H8/300
    serialize MAH8_300H      = cw16 $ 47 -- Hitachi H8/300H
    serialize MAH8S          = cw16 $ 48 -- Hitachi H8S
    serialize MAH8_500       = cw16 $ 49 -- Hitachi H8/500
    serialize MAIA_64        = cw16 $ 50 -- Intel IA-64 processor architecture
    serialize MAMIPS_X       = cw16 $ 51 -- Stanford MIPS-X
    serialize MACOLDFIRE     = cw16 $ 52 -- Motorola ColdFire
    serialize MA68HC12       = cw16 $ 53 -- Motorola M68HC12
    serialize MAMMA          = cw16 $ 54 -- Fujitsu MMA Multimedia Accelerator
    serialize MAPCP          = cw16 $ 55 -- Siemens PCP
    serialize MANCPU         = cw16 $ 56 -- Sony nCPU embedded RISC processor
    serialize MANDR1         = cw16 $ 57 -- Denso NDR1 microprocessor
    serialize MASTARCORE     = cw16 $ 58 -- Motorola Star*Core processor
    serialize MAME16         = cw16 $ 59 -- Toyota ME16 processor
    serialize MAST100        = cw16 $ 60 -- STMicroelectronics ST100 processor
    serialize MATINYJ        = cw16 $ 61 -- Advanced Logic Corp. TinyJ embedded processor family
    serialize MAX86_64       = cw16 $ 62 -- AMD x86-64 architecture
    serialize MAPDSP         = cw16 $ 63 -- Sony DSP Processor
    serialize MAFX66         = cw16 $ 66 -- Siemens FX66 microcontroller
    serialize MAST9PLUS      = cw16 $ 67 -- STMicroelectronics ST9+ 8/16 bit microcontroller
    serialize MAST7          = cw16 $ 68 -- STMicroelectronics ST7 8-bit microcontroller
    serialize MA68HC16       = cw16 $ 69 -- Motorola MC68HC16 Microcontroller
    serialize MA68HC11       = cw16 $ 70 -- Motorola MC68HC11 Microcontroller
    serialize MA68HC08       = cw16 $ 71 -- Motorola MC68HC08 Microcontroller
    serialize MA68HC05       = cw16 $ 72 -- Motorola MC68HC05 Microcontroller
    serialize MASVX          = cw16 $ 73 -- Silicon Graphics SVx
    serialize MAST19         = cw16 $ 74 -- STMicroelectronics ST19 8-bit microcontroller
    serialize MAVAX          = cw16 $ 75 -- Digital VAX
    serialize MACRIS         = cw16 $ 76 -- Axis Communications 32-bit embedded processor
    serialize MAJAVELIN      = cw16 $ 77 -- Infineon Technologies 32-bit embedded processor
    serialize MAFIREPATH     = cw16 $ 78 -- Element 14 64-bit DSP Processor
    serialize MAZSP          = cw16 $ 79 -- LSI Logic 16-bit DSP Processor
    serialize MAMMIX         = cw16 $ 80 -- Donald Knuth's educational 64-bit processor
    serialize MAHUANY        = cw16 $ 81 -- Harvard University machine-independent object files
    serialize MAPRISM        = cw16 $ 82 -- SiTera Prism
    serialize MAAVR          = cw16 $ 83 -- Atmel AVR 8-bit microcontroller
    serialize MAFR30         = cw16 $ 84 -- Fujitsu FR30
    serialize MAD10V         = cw16 $ 85 -- Mitsubishi D10V
    serialize MAD30V         = cw16 $ 86 -- Mitsubishi D30V
    serialize MAV850         = cw16 $ 87 -- NEC v850
    serialize MAM32R         = cw16 $ 88 -- Mitsubishi M32R
    serialize MAMN10300      = cw16 $ 89 -- Matsushita MN10300
    serialize MAMN10200      = cw16 $ 90 -- Matsushita MN10200
    serialize MAPJ           = cw16 $ 91 -- picoJava
    serialize MAOPENRISC     = cw16 $ 92 -- OpenRISC 32-bit embedded processor
    serialize MAARC_A5       = cw16 $ 93 -- ARC Cores Tangent-A5
    serialize MAXTENSA       = cw16 $ 94 -- Tensilica Xtensa Architecture

instance Serializable ELFFileVersion where
    serialize FVNone    = cw32 0
    serialize FVCurrent = cw32 1

instance Serializable ELFIDIndex where
    serialize MAG0       = cw8  0
    serialize MAG1       = cw8  1
    serialize MAG2       = cw8  2
    serialize MAG3       = cw8  3
    serialize CLASS      = cw8  4
    serialize DATA       = cw8  5
    serialize VERSION    = cw8  6
    serialize OSABI      = cw8  7
    serialize ABIVERSION = cw8  8
    serialize PAD        = cw8  9
    serialize NIDENT     = cw8 16

instance Serializable ELFFileClass where
    serialize FCNone = cw8 0
    serialize FC32   = cw8 1
    serialize FC64   = cw8 2

instance Serializable ELFByteOrder where
    serialize BONone         = cw8 0
    serialize BOLittleEndian = cw8 1
    serialize BOBigEndian    = cw8 2

instance Serializable ELFOS where
    serialize OSNone    = cw8  0 -- No extensions or unspecified
    serialize OSHPUX    = cw8  1 -- Hewlett-Packard HP-UX
    serialize OSNetBSD  = cw8  2 -- NetBSD
    serialize OSLinux   = cw8  3 -- Linux
    serialize OSSolaris = cw8  6 -- Sun Solaris
    serialize OSAIX     = cw8  7 -- AIX
    serialize OSIRIX    = cw8  8 -- IRIX
    serialize OSFreeBSD = cw8  9 -- FreeBSD
    serialize OSTRU64   = cw8 10 -- Compaq TRU64 UNIX
    serialize OSModesto = cw8 11 -- Novell Modesto
    serialize OSOpenBSD = cw8 12 -- Open BSD

instance Serializable ELFSectIndex where
    serialize SINone      = cw16 0
    serialize SILOReserve = cw16 0xFF00
    serialize SILOProc    = cw16 0xFF00
    serialize SIHIProc    = cw16 0xFF1F
    serialize SILOOS      = cw16 0xFF20
    serialize SIHIOS      = cw16 0xFF3F
    serialize SIAbs       = cw16 0xFFF1
    serialize SICommon    = cw16 0xFFF2
    serialize SIXIndex    = cw16 0xFFFF
    serialize SIHIReserve = cw16 0xFFFF

instance Serializable ELFSectType where
    serialize STNull         = cw32          0
    serialize STProgBits     = cw32          1
    serialize STSymTab       = cw32          2
    serialize STStrTab       = cw32          3
    serialize STRela         = cw32          4
    serialize STHash         = cw32          5
    serialize STDynamic      = cw32          6
    serialize STNote         = cw32          7
    serialize STNoBits       = cw32          8
    serialize STRel          = cw32          9
    serialize STSHLIB        = cw32         10
    serialize STDynSym       = cw32         11
    serialize STInitArray    = cw32         14
    serialize STFinArray     = cw32         15
    serialize STPreInitArray = cw32         16
    serialize STGroup        = cw32         17
    serialize STSymTabSHNdx  = cw32         18
    serialize STLOOS         = cw32 0x60000000
    serialize STHIOS         = cw32 0x6fffffff
    serialize STLOProc       = cw32 0x70000000
    serialize STHIProc       = cw32 0x7FFFFFFF
    serialize STLOUser       = cw32 0x80000000
    serialize STHIUser       = cw32 0xFFFFFFFF

instance Serializable ELFSectFlag where
    serialize SFWrite           = cw32        0x1
    serialize SFAlloc           = cw32        0x2
    serialize SFExecInstr       = cw32        0x4
    serialize SFMerge           = cw32       0x10
    serialize SFStrings         = cw32       0x20
    serialize SFInfoLink        = cw32       0x40
    serialize SFLinkOrder       = cw32       0x80
    serialize SFOSNonConforming = cw32      0x100
    serialize SFGroup           = cw32      0x200
    serialize SFTLS             = cw32      0x400
    serialize SFMaskOS          = cw32 0x0ff00000
    serialize SFMaskProc        = cw32 0xF0000000

instance Serializable ELFGroupFlag where
    serialize GFComDat   = cw32 0x1
    serialize GFMaskOS   = cw32 0x0ff00000
    serialize GFMaskProc = cw32 0xf0000000

instance Serializable ELFSymBinding where
    serialize SBLocal  = cw8  0
    serialize SBGlobal = cw8  1
    serialize SBWeak   = cw8  2
    serialize SBLoOS   = cw8 10
    serialize SBHiOS   = cw8 12
    serialize SBLoProc = cw8 13
    serialize SBHiProc = cw8 15

instance Serializable ELFSymType where
    serialize SYTNone    = cw8  0
    serialize SYTObject  = cw8  1
    serialize SYTFunc    = cw8  2
    serialize SYTSection = cw8  3
    serialize SYTFile    = cw8  4
    serialize SYTCommon  = cw8  5
    serialize SYTTLS     = cw8  6
    serialize SYTLoOS    = cw8 10
    serialize SYTHiOS    = cw8 12
    serialize SYTLoProc  = cw8 13
    serialize SYTHiProc  = cw8 15

instance Serializable ELFSymVisibility where
    serialize SYVDefault   = cw8 0
    serialize SYVInternal  = cw8 1
    serialize SYVHidden    = cw8 2
    serialize SYVProtected = cw8 3

-- Undefined name
-- define STN_UNDEF 0

instance Serializable ELFRelocationType where
    serialize RT386None     = cw8  0
    serialize RT38632       = cw8  1
    serialize RT386PC32     = cw8  2
    serialize RT386GOT32    = cw8  3
    serialize RT386PLT32    = cw8  4
    serialize RT386COPY     = cw8  5
    serialize RT386GLOB_DAT = cw8  6
    serialize RT386JMP_SLOT = cw8  7
    serialize RT386RELATIVE = cw8  8
    serialize RT386GOTOFF   = cw8  9
    serialize RT386GOTPC    = cw8 10

instance Serializable ELFSegmentType where
    serialize SegTNull    = cw32          0
    serialize SegTLoad    = cw32          1
    serialize SegTDynamic = cw32          2
    serialize SegTInterp  = cw32          3
    serialize SegTNote    = cw32          4
    serialize SegTSHLib   = cw32          5
    serialize SegTPHDR    = cw32          6
    serialize SegTTLS     = cw32          7
    serialize SegTLoOS    = cw32 0x60000000
    serialize SegTHiOS    = cw32 0x6fffffff
    serialize SegTLoProc  = cw32 0x70000000    
    serialize SegTHiProc  = cw32 0x7FFFFFFF    

instance Serializable ELFSegFlag where
    serialize SegFX        = cw32          1
    serialize SegFW        = cw32          2
    serialize SegFR        = cw32          4
    serialize SegFMaskOS   = cw32 0x0ff00000
    serialize SegFMaskProc = cw32 0xf0000000

instance Serializable ELFDynArray where
    serialize DTNull           = cw32  0
    serialize DTNeeded         = cw32  1
    serialize DTPLTRelsz       = cw32  2
    serialize DTPLTGot         = cw32  3
    serialize DTHash           = cw32  4
    serialize DTStrTab         = cw32  5
    serialize DTSymTab         = cw32  6
    serialize DTRela           = cw32  7
    serialize DTRelaSZ         = cw32  8
    serialize DTRelaNT         = cw32  9
    serialize DTStrSZ          = cw32 10
    serialize DTSymENT         = cw32 11
    serialize DTInit           = cw32 12
    serialize DTFini           = cw32 13
    serialize DTSOName         = cw32 14
    serialize DTRPath          = cw32 15
    serialize DTSymbolic       = cw32 16
    serialize DTRel            = cw32 17
    serialize DTRelSZ          = cw32 18
    serialize DTRelENT         = cw32 19
    serialize DTPLTRel         = cw32 20
    serialize DTDebug          = cw32 21
    serialize DTTExtRel        = cw32 22
    serialize DTJMPRel         = cw32 23
    serialize DTBindNow        = cw32 24
    serialize DTInitArray      = cw32 25
    serialize DTFiniArray      = cw32 26
    serialize DTInitArraySZ    = cw32 27
    serialize DTFiniArraySZ    = cw32 28
    serialize DTRunPath        = cw32 29
    serialize DTFlags          = cw32 30
    serialize DTEncoding       = cw32 32
    serialize DTPreInitArray   = cw32 32
    serialize DTPreInitArraySZ = cw32 33
    serialize DTLoOS           = cw32 0x6000000D
    serialize DTHiOS           = cw32 0x6ffff000
    serialize DTLoProc         = cw32 0x70000000
    serialize DTHiProc         = cw32 0x7FFFFFFF
    
instance Serializable ELFDTFlag where
    serialize DFOrigin    = cw8 0x1
    serialize DFSymbolic  = cw8 0x2
    serialize DFTextRel   = cw8 0x4
    serialize DFBindNow   = cw8 0x8
    serialize DFStaticTLS = cw8 0x10

instance Serializable ELFHeader where
    serialize (ELFHeader { htype=a, hmachine=b, hversion=c, hentry=d, hphoff=e, hshoff=f, hflags=g, hehsize=h, hphentsize=i, hphnum=j, hshentsize=k, hshnum=l, hshstrndx=m }) = n where
        n        = elfMagic ++ serialize a ++ serialize b ++ serialize c ++ serialize d ++ serialize e ++ serialize f ++ serialize g ++ serialize h ++ serialize i ++ serialize j ++ serialize k ++ serialize l ++ serialize m
        elfMagic = 0x7F : map (castn . ord) "ELF" ++ [0x1,0x1,0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0]

instance Serializable ELFSectHeader where
    serialize (ELFSectHeader { shname=a, shtype=b, shflags=c, shaddr=d, shoffset=e, shsize=f, shlink=g, shinfo=h, shaddralign=i, shentsize=j }) = k where
        k = serialize a ++ serialize b ++ serialize c ++ serialize d ++ serialize e ++ serialize f ++ serialize g ++ serialize h ++ serialize i ++ serialize j

instance Serializable ELFSegHeader where
    serialize (ELFSegHeader { gtype=a, goffset=b, gvaddr=c, gpaddr=d, gfilesz=e, gmemsz=f, gflags=g, galign=h }) = i where
        i = serialize a ++ serialize b ++ serialize c ++ serialize d ++ serialize e ++ serialize f ++ serialize g ++ serialize h

instance Serializable ELFSymTabEnt where
    serialize (ELFSymTabEnt { symname=a, symvalue=b, symsize=c, syminfo=d, symother=e, symshndx=f }) = g where
        g = serialize a ++ serialize b ++ serialize c ++ serialize d ++ serialize e ++ serialize f

elfSTBind       i   = i `shiftR` 4
elfSTType       i   = i .&. 0xf
elfSTInfo       b t = (b `shiftL` 4) + (t .&. 0xf)
elfSTVisibility o   = o .&. 0x3

instance Serializable ELFRelEnt where
    serialize (ELFRelEnt { reoffset=a, reinfo=b }) = c where
        c = serialize a ++ serialize b

instance Serializable ELFRelaEnt where
    serialize (ELFRelaEnt { raoffset=a, rainfo=b, raaddend=c }) = d where
        d = serialize a ++ serialize b ++ serialize c

elfRSym  i   = i `shiftR` 8
elfRType i   = (castn i) :: Word8
elfRInfo s t = (s `shiftL` 8) + ((castn t) :: Word8)

{-
// Dynamic structure
struct Elf32_Dyn {
    Elf32_Sword d_tag;
    union {
        Elf32_Word d_val;
        Elf32_Addr d_ptr;
    } d_un;
};
-}
