{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.X86_32.Assembler where
import ASM.Term
import ASM.Type
import Link.ObjFile

import Util.Annotated
import Util.FileBuf
import Util.Num
import Util.String
import Util.Sequence

import qualified Data.Map as Map
import Data.Bits
import Data.Char
import GHC.Word

-- an element of machine translation (can be interpreted as a patch to apply to a compile object)
data AsmData = RawData [Word8] | LabelDef String | DLabelDef String [Word8] | LabelRef String FixupType deriving (Eq, Show)
code       d   = RawData     d
def_label  s   = LabelDef  s
def_dlabel s d = DLabelDef s d
lbl        s   = LabelRef  s Absolute
rlbl       s   = LabelRef  s Relative

-- assemble a sequence of primitive instructions into a linkable object
assembleUnit :: Annotation a => [Inst a] -> CObj
assembleUnit insts = foldl applyAsmData (CObj [] []) (concat (map (encodeInstruction instructionTable) insts))

-- apply assembly data to a linkable object
applyAsmData :: CObj -> AsmData -> CObj
applyAsmData obj d = apply d (objectFindOrInsertSection obj defsect) where
    apply (RawData dt')     (obj', Sect nm dt fixes flags, sidx) = objectInsertSection obj'  $ Sect nm (dt ++ dt') fixes flags
    apply (LabelDef s)      (obj', Sect _  dt _     _,     sidx) = objectInsertSymbol  obj'  $ Symbol s (castn $ length dt) (castn sidx) Export Internal
    apply (DLabelDef s dt') (obj', Sect nm dt fixes flags, sidx) = objectInsertSection obj'' $ Sect nm (dt ++ dt') fixes flags where
        obj'' = objectInsertSymbol obj' $ Symbol s (castn $ length dt) (castn sidx) Export Internal
    apply (LabelRef s fty)  (obj', Sect nm dt fixes flags, sidx) = objectInsertSection obj'' $ Sect nm (dt ++ w32 0) fixes' flags where
        (obj'', _, symidx) = objectFindOrInsertSymbol obj' (Symbol s 0 0 Import Internal)
        fixes'             = (Fixup (castn $ length dt) (castn symidx) fty):fixes
    defsect = case d of
        DLabelDef _ _ -> Sect ".data" [] [] [Data, Read, Write]
        _             -> Sect ".text" [] [] [Code]

-- determine whether an abstract assembly call belongs to a particular op-call pattern
-- if so, encode the instruction accordingly
type OpDefs       = Map.Map String [(OpCall, OpEncoding)]
data OpCall       = OpCall [OCArg]
data OCArg        = CReg Int String | RM Int | R Int | Imm Int | RelAddr Int
data OpEncoding   = OpEncoding [Word8] RegAdded ModRMEnc
data RegAdded     = NoRegAdded | RegAdded
data ModRMEnc     = NoModRM | EncReg | EncID Int
data OpArgMatch a = OAMReg (OpArg a) | OAMRM (OpArg a) | OAMImm (OpArg a) | OAMRelAddr (OpArg a)

instance Show OpCall where
    show (OpCall args) = "'" ++ cdelim (map show args) " " ++ "'"
    
instance Show OCArg where
    show (CReg n r)  = r ++ "::b(" ++ show (n * 8) ++ ")"
    show (RM n)      = "r/m" ++ show (n * 8)
    show (R n)       = "r" ++ show (n * 8)
    show (Imm n)     = "imm" ++ show (n * 8)
    show (RelAddr n) = "rel" ++ show (n * 8)
    
instance Show OpEncoding where
    show (OpEncoding bs ra m) = "0x" ++ concat (map hexByte bs) ++ " " ++ sra ra ++ sm m where
        sra NoRegAdded = ""
        sra RegAdded   = "+r "
        sm  NoModRM    = ""
        sm  EncReg     = "/r"
        sm  (EncID x)  = "/" ++ show x
    
encodeInstruction :: Annotation a => OpDefs -> Inst a -> [AsmData]
encodeInstruction _ (LblDef _ s)          = [def_label s]
encodeInstruction _ (DataDef _ s d)       = [def_dlabel s d]
encodeInstruction m (Op _ op d s af lbls) = select (Map.lookup op m) where
    args              = af d s lbls
    
    select (Just ces) = firstMatch [encodeOp enc (argMatches $ zip args' args) | (OpCall args', enc) <- ces, validPairing args' args]
    select Nothing    = error ("Invalid op-code: " ++ op)
    
    firstMatch (r:_) = r
    firstMatch []    = error ("Unable to find a matching encoding for the op '" ++ op ++ "' with the args: " ++ showOpArgs args)
    
    validPairing args' args = length args' == length args && all (uncurry argIsMatch) (zip args' args)

showOpArgs :: Annotation a => [OpArg a] -> String
showOpArgs args = cdelim (map showOpArg args) ", " where
    showOpArg (OAReg _ ty rn)      = rn ++ "::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg (OARMemOff _ ty r 0) = "[" ++ r ++ "]::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg (OARMemOff _ ty r n) = "[" ++ r ++ "+" ++ show n ++ "]::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg (OACMemOff _ ty n)   = "[" ++ show n ++ "]::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg (OACCMemOff _ ty s)  = "[" ++ s ++ "]::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg (OAConst _ c)        = show c ++ "::byte(" ++ show (sizeof (primTy c)) ++ ")"
    showOpArg (OACConst _ ty s)    = s ++ "::byte(" ++ show (sizeof ty) ++ ")"
    showOpArg _                    = "<unknown op argument type>"

argIsMatch :: Annotation a => OCArg -> OpArg a -> Bool
argIsMatch (CReg n rname) (OAReg _ ty rn)      = n == sizeof ty && rname == rn
argIsMatch (CReg _ _)     _                    = False
argIsMatch (RM n)         (OAReg _ ty _)       = n == sizeof ty
argIsMatch (RM n)         (OARMemOff _ ty _ _) = n == sizeof ty
argIsMatch (RM n)         (OACMemOff _ ty _)   = n == sizeof ty
argIsMatch (RM n)         (OACCMemOff _ ty _)  = n == sizeof ty
argIsMatch (RM _)         _                    = False
argIsMatch (R n)          (OAReg _ ty _)       = n == sizeof ty
argIsMatch (R _)          _                    = False
argIsMatch (Imm n)        (OAConst _ c)        = n == sizeof (primTy c)
argIsMatch (Imm n)        (OACConst _ ty _)    = n == sizeof ty
argIsMatch (Imm _)        _                    = False
argIsMatch (RelAddr n)    (OACConst _ ty _)    = n == sizeof ty
argIsMatch (RelAddr _)    _                    = False

argMatches :: [(OCArg, OpArg a)] -> [OpArgMatch a]
argMatches ((CReg _ _, _):ms)    = argMatches ms
argMatches ((RM _, oar):ms)      = (OAMRM oar):(argMatches ms)
argMatches ((R _, oar):ms)       = (OAMReg oar):(argMatches ms)
argMatches ((Imm _, oar):ms)     = (OAMImm oar):(argMatches ms)
argMatches ((RelAddr _, oar):ms) = (OAMRelAddr oar):(argMatches ms)
argMatches []                    = []

encodeOp :: Annotation a => OpEncoding -> [OpArgMatch a] -> [AsmData]
encodeOp (OpEncoding opd ra mrm) args = encodeOpHead opd ra args ++ encodeModRM mrm args ++ encodeImm args

encodeOpHead :: [Word8] -> RegAdded -> [OpArgMatch a] -> [AsmData]
encodeOpHead opd NoRegAdded _    = [code opd]
encodeOpHead opd RegAdded   args = [code (rtail opd ++ [flast opd + reg_code (regArgName args)])] where
    flast [] = error "encodeOpHead last failure"
    flast xs = last xs

encodeModRM :: ModRMEnc -> [OpArgMatch a] -> [AsmData]
encodeModRM NoModRM   _    = []
encodeModRM EncReg    args = encodeModRMWithR (reg_code (regArgName args)) (rmArg args)
encodeModRM (EncID x) args = encodeModRMWithR (castn x)                    (rmArg args)

encodeModRMWithR :: Word8 -> OpArg a -> [AsmData]
encodeModRMWithR r (OAReg _ _ x) = [code $ [mod_rm 3 r (reg_code x)]]

encodeModRMWithR r (OARMemOff _ _ "bp" 0) = [code [mod_rm 1 r (reg_code "bp"), 0]]
encodeModRMWithR r (OARMemOff _ _ "sp" 0) = [code [mod_rm 1 r 4, sib 0 4 4, 0]]
encodeModRMWithR r (OARMemOff _ _ reg  0) = [code [mod_rm 0 r (reg_code reg)]]

encodeModRMWithR r (OARMemOff _ _ "sp" n) | abs n <= 127 = [code [mod_rm 1 r 4, sib 0 4 4,   castn n]]
encodeModRMWithR r (OARMemOff _ _ reg  n) | abs n <= 127 = [code [mod_rm 1 r (reg_code reg), castn n]]

encodeModRMWithR r (OARMemOff _ _ "sp" n) = [code $ [mod_rm 2 r 4, sib 0 4 4]   ++ cw32 n]
encodeModRMWithR r (OARMemOff _ _ reg  n) = [code $ [mod_rm 2 r (reg_code reg)] ++ cw32 n]

encodeModRMWithR r (OACMemOff _ _ x) = [code $ [mod_rm 0 r 5] ++ cw32 x]

encodeModRMWithR r (OACCMemOff _ _ s) = [code [mod_rm 0 r 5], lbl s]

encodeModRMWithR _ _ = error "Internal compiler error (invalid mod-r/m argument in assembly op invocation)."

regArgName :: [OpArgMatch a] -> String
regArgName args = select [rname | OAMReg (OAReg _ _ rname) <- args] where
    select (n:_) = n
    select []    = error "Internal compiler error (expected named register in assembly op invocation)."

rmArg :: [OpArgMatch a] -> OpArg a
rmArg args = select [arg | OAMRM arg <- args] where
    select (h:_) = h
    select []    = error "Internal compiler error (expected mod-r/m argument in assembly op invocation)."

encodeImm :: Annotation a => [OpArgMatch a] -> [AsmData]
encodeImm args = concat [r | Just r <- map translate args] where
    translate (OAMImm x)     = Just (translateImm x)
    translate (OAMRelAddr x) = Just (translateRelAddr x)
    translate _              = Nothing
    
    translateImm (OAConst  _ (CChar  _ x)) = [code (cw8 (ord x))]
    translateImm (OAConst  _ (CByte  _ x)) = [code (cw8 x)]
    translateImm (OAConst  _ (CShort _ x)) = [code (cw16 x)]
    translateImm (OAConst  _ (CInt   _ x)) = [code (cw32 x)]
    translateImm (OACConst _ _ s)          = [lbl s]
    translateImm e                         = error $ "Internal compiler error (unable to translate immediate argument: " ++ show e ++ ")."
    
    translateRelAddr (OACConst _ _ s) = [rlbl s]
    translateRelAddr e                = error $ "Internal compiler error (unable to translate immediate argument as address: " ++ show e ++ ")."

-- the table of instruction encodings
instructionTable :: OpDefs
instructionTable = Map.fromList
    [
        {-
            Instruction table entries come from the call-pattern and op-encoding types
            (the use of these types is cleaned up a little so that table entries aren't so large)
            
            data OpCall     = OpCall [OCArg]
            data OCArg      = CReg Int String | RM Int | R Int | Imm Int | RelAddr Int
            
            data OpEncoding = OpEncoding [Word8] RegAdded ModRMEnc
            data RegAdded   = NoRegAdded | RegAdded
            data ModRMEnc   = NoModRM | EncReg | EncID Int
        -}
        
        -- ascii adjust after addition
        ("aaa", [op0 [0x37]]),
        
        -- ascii adjust ax before division
        ("aad", [op0 [0xd5, 0x0a], op1 imm1 [0xd5]]),
        
        -- ascii adjust ax after multiply
        ("aam", [op0 [0xd4, 0x0a], op1 imm1 [0xd4]]),
        
        -- ascii adjust ax after subtraction
        ("aas", [op0 [0x3f]]),
        
        -- add with carry
        ("adc",
            [
                op2     (cr1 "ax") imm1 [0x14],         op2     (cr2 "ax") imm2 [0x66, 0x15],   op2     (cr4 "ax") imm4 [0x15],
                op2CMRM rm1        imm1 [0x80] 2,       op2CMRM rm2        imm2 [0x66, 0x81] 2, op2CMRM rm4        imm4 [0x81] 2,
                op2CMRM rm2        imm1 [0x66, 0x83] 2, op2CMRM rm4        imm1 [0x83] 2,
                op2MRM  rm1        r1   [0x10],         op2MRM  rm2        r2   [0x66, 0x11],   op2MRM  rm4        r4 [0x11],
                op2MRM  r1         rm1  [0x12],         op2MRM  r2         rm2  [0x66, 0x13],   op2MRM  r4         rm4 [0x13]
            ]
        ),
        
        -- add
        ("add",
            [
                op2     (cr1 "ax") imm1 [0x04],         op2     (cr2 "ax") imm2 [0x66, 0x05],   op2     (cr4 "ax") imm4 [0x05],
                op2CMRM rm1        imm1 [0x80] 0,       op2CMRM rm2        imm2 [0x66, 0x81] 0, op2CMRM rm4        imm4 [0x81] 0,
                op2CMRM rm2        imm1 [0x66, 0x83] 0, op2CMRM rm4        imm1 [0x83] 0,       op2MRM  rm1        r1   [0x00],
                op2MRM  rm2        r2   [0x66, 0x01],   op2MRM  rm4        r4   [0x01],         op2MRM  r1         rm1  [0x02],
                op2MRM  r2         rm2  [0x66, 0x03],   op2MRM  r4         rm4  [0x03]
            ]
        ),
        
        -- logical and
        ("and",
            [
                op2     (cr1 "ax") imm1 [0x24],         op2     (cr2 "ax") imm2 [0x66, 0x25],   op2 (cr4 "ax") imm4 [0x25],
                op2CMRM rm1        imm1 [0x80] 4,       op2CMRM rm2        imm2 [0x66, 0x81] 4, op2CMRM rm4 imm4 [0x81] 4,
                op2CMRM rm2        imm1 [0x66, 0x83] 4, op2CMRM rm4        imm1 [0x83] 4,
                op2MRM  rm1        r1   [0x20],         op2MRM  rm2        r2   [0x66, 0x21],   op2MRM  rm4 r4   [0x21],
                op2MRM  r1         rm1  [0x22],         op2MRM  r2         rm2  [0x66, 0x23],   op2MRM  r4  rm4  [0x23]
            ]
        ),
        
        -- byte swap
        ("bswap", [op1A r4 [0x0f, 0xc8]]),
        
        -- bit test
        ("bt",
            [
                op2MRM  rm2 r2   [0x66, 0x0f, 0xa3],   op2MRM  rm4 r4   [0x0f, 0xa3],
                op2CMRM rm2 imm1 [0x66, 0x0f, 0xba] 4, op2CMRM rm4 imm1 [0x0f, 0xba] 4
            ]
        ),
        
        -- call procedure
        ("call",
            [
                op1     ra2 [0x66, 0xe8],   op1     ra4 [0xe8],
                op1CMRM rm2 [0x66, 0xff] 2, op1CMRM rm4 [0xff] 2
            ]
        ),
        
        -- compare two operands
        ("cmp",
            [
                op2     (cr1 "ax") imm1 [0x3c],         op2     (cr2 "ax") imm2 [0x66, 0x3d],   op2     (cr4 "ax") imm4 [0x3d],
                op2CMRM rm1        imm1 [0x80] 7,       op2CMRM rm2        imm2 [0x66, 0x81] 7, op2CMRM rm4        imm4 [0x81] 7,
                op2CMRM rm2        imm1 [0x66, 0x83] 7, op2CMRM rm4        imm1 [0x83] 7,
                op2MRM  rm1        r1   [0x38],         op2MRM  rm2        r2   [0x66, 0x39],   op2MRM  rm4        r4   [0x39],
                op2MRM  r1         rm1  [0x3a],         op2MRM  r2         rm2  [0x66, 0x3b],   op2MRM  r4         rm4  [0x3b]
            ]
        ),
        
        -- convert word to double-word
        ("cwd", [op0 [0x99]]),
        
        -- decrement by 1
        ("dec",
            [
                op1CMRM rm1 [0xfe] 1,     op1CMRM rm2 [0x66, 0xff] 1, op1CMRM rm4 [0xff] 1,
                op1A    r2  [0x66, 0x48], op1A    r4  [0x48]
            ]
        ),
        
        -- unsigned divide
        ("div", [op1CMRM rm1 [0xf6] 6, op1CMRM rm2 [0x66, 0xf7] 6, op1CMRM rm4 [0xf7] 6]),
        
        -- empty MMX state
        ("emms", [op0 [0xff, 0x77]]),
        
        -- absolute value
        ("fabs", [op0 [0xd9, 0xe1]]),
        
        -- change sign of ST(0)
        ("fchs", [op0 [0xd9, 0xe0]]),
        
        -- cosine of ST(0)
        ("fcos", [op0 [0xd9, 0xff]]),
        
        -- decrement FPU stack-top
        ("fdecstp", [op0 [0xd9, 0xf6]]),
        
        -- increment FPU stack-top
        ("fincstp", [op0 [0xd9, 0xf7]]),
        
        -- initialize floating-point unit
        ("finit", [op0 [0x9b, 0xdb, 0xe3]]),
        ("fninit", [op0 [0xdb, 0xe3]]),
        
        -- round ST(0) to integral value
        ("frndint", [op0 [0xd9, 0xfc]]),
        
        -- sine of ST(0)
        ("fsin", [op0 [0xd9, 0xfe]]),
        
        -- sine of ST(0) into ST(0), cosine of ST(0) into ST(TOP)
        ("fsincos", [op0 [0xd9, 0xfb]]),
        
        -- square root
        ("fsqrt", [op0 [0xd9, 0xfa]]),
        
        -- signed divide
        ("idiv", [op1CMRM rm1 [0xf6] 7, op1CMRM rm2 [0x66, 0xf7] 7, op1CMRM rm4 [0xf7] 7]),
        
        -- signed multiply
        ("imul",
            [
                op1CMRM rm1          [0xf6] 5,           op1CMRM rm2         [0x66, 0xf7] 5, op1CMRM rm4         [0xf7] 5,
                op2MRM  r2 rm2       [0x66, 0x0f, 0xaf], op2MRM  r4 rm4      [0x0f, 0xaf],   op3MRM  r2 rm2 imm1 [0x66, 0x6b],
                op3MRM  r4 rm4  imm1 [0x6b],             op2MRM  r2 imm1     [0x66, 0x6b],   op2MRM  r4 imm1     [0x6b],
                op3MRM  r2 rm2  imm2 [0x66, 0x69],       op3MRM  r4 rm4 imm4 [0x69],         op2MRM  r2 imm2     [0x69],
                op2MRM  r4 imm4      [0x69]
            ]
        ),
        
        -- increment by one
        ("inc",
            [
                op1CMRM rm1 [0xfe] 0,     op1CMRM rm2 [0x66, 0xff] 0, op1CMRM rm4 [0xff] 0,
                op1A    r2  [0x66, 0x40], op1A    r4  [0x40]
            ]
        ),
        
        -- call interrupt
        ("int", [op1 imm1 [0xcd]]),
        
        -- jCC (conditional jumps)
        ("ja",    [op1 ra1 [0x77], op1 ra2 [0x66, 0x0f, 0x87], op1 ra4 [0x0f, 0x87]]),
        ("jae",   [op1 ra1 [0x73], op1 ra2 [0x66, 0x0f, 0x83], op1 ra4 [0x0f, 0x83]]),
        ("jb",    [op1 ra1 [0x72], op1 ra2 [0x66, 0x0f, 0x82], op1 ra4 [0x0f, 0x82]]),
        ("jbe",   [op1 ra1 [0x76], op1 ra2 [0x66, 0x0f, 0x86], op1 ra4 [0x0f, 0x86]]),
        ("jc",    [op1 ra1 [0x72], op1 ra2 [0x66, 0x0f, 0x82], op1 ra4 [0x0f, 0x82]]),
        ("jcxz",  [op1 ra1 [0xe3]]),
        ("jecxz", [op1 ra1 [0xe3]]),
        ("je",    [op1 ra1 [0x74], op1 ra2 [0x66, 0x0f, 0x84], op1 ra4 [0x0f, 0x84]]),
        ("jg",    [op1 ra1 [0x7f], op1 ra2 [0x66, 0x0f, 0x8f], op1 ra4 [0x0f, 0x8f]]),
        ("jge",   [op1 ra1 [0x7d], op1 ra2 [0x66, 0x0f, 0x8d], op1 ra4 [0x0f, 0x8d]]),
        ("jl",    [op1 ra1 [0x7c], op1 ra2 [0x66, 0x0f, 0x8c], op1 ra4 [0x0f, 0x8c]]),
        ("jle",   [op1 ra1 [0x7e], op1 ra2 [0x66, 0x0f, 0x8e], op1 ra4 [0x0f, 0x8e]]),
        ("jna",   [op1 ra1 [0x76], op1 ra2 [0x66, 0x0f, 0x86], op1 ra4 [0x0f, 0x86]]),
        ("jnae",  [op1 ra1 [0x72], op1 ra2 [0x66, 0x0f, 0x82], op1 ra4 [0x0f, 0x82]]),
        ("jnb",   [op1 ra1 [0x73], op1 ra2 [0x66, 0x0f, 0x83], op1 ra4 [0x0f, 0x83]]),
        ("jnbe",  [op1 ra1 [0x77], op1 ra2 [0x66, 0x0f, 0x87], op1 ra4 [0x0f, 0x87]]),
        ("jnc",   [op1 ra1 [0x73], op1 ra2 [0x66, 0x0f, 0x83], op1 ra4 [0x0f, 0x83]]),
        ("jne",   [op1 ra1 [0x75], op1 ra2 [0x66, 0x0f, 0x85], op1 ra4 [0x0f, 0x85]]),
        ("jng",   [op1 ra1 [0x7e], op1 ra2 [0x66, 0x0f, 0x8e], op1 ra4 [0x0f, 0x8e]]),
        ("jnge",  [op1 ra1 [0x7c], op1 ra2 [0x66, 0x0f, 0x8c], op1 ra4 [0x0f, 0x8c]]),
        ("jnl",   [op1 ra1 [0x7d], op1 ra2 [0x66, 0x0f, 0x8d], op1 ra4 [0x0f, 0x8d]]),
        ("jnle",  [op1 ra1 [0x7f], op1 ra2 [0x66, 0x0f, 0x8f], op1 ra4 [0x0f, 0x8f]]),
        ("jno",   [op1 ra1 [0x71], op1 ra2 [0x66, 0x0f, 0x81], op1 ra4 [0x0f, 0x81]]),
        ("jnp",   [op1 ra1 [0x7b], op1 ra2 [0x66, 0x0f, 0x8b], op1 ra4 [0x0f, 0x8b]]),
        ("jns",   [op1 ra1 [0x79], op1 ra2 [0x66, 0x0f, 0x89], op1 ra4 [0x0f, 0x89]]),
        ("jnz",   [op1 ra1 [0x75], op1 ra2 [0x66, 0x0f, 0x85], op1 ra4 [0x0f, 0x85]]),
        ("jo",    [op1 ra1 [0x70], op1 ra2 [0x66, 0x0f, 0x80], op1 ra4 [0x0f, 0x80]]),
        ("jp",    [op1 ra1 [0x7a], op1 ra2 [0x66, 0x0f, 0x8a], op1 ra4 [0x0f, 0x8a]]),
        ("jpe",   [op1 ra1 [0x7a], op1 ra2 [0x66, 0x0f, 0x8a], op1 ra4 [0x0f, 0x8a]]),
        ("jpo",   [op1 ra1 [0x7b], op1 ra2 [0x66, 0x0f, 0x8b], op1 ra4 [0x0f, 0x8b]]),
        ("js",    [op1 ra1 [0x78], op1 ra2 [0x66, 0x0f, 0x88], op1 ra4 [0x0f, 0x88]]),
        ("jz",    [op1 ra1 [0x74], op1 ra2 [0x66, 0x0f, 0x84], op1 ra4 [0x0f, 0x84]]),
        
        -- unconditional jump
        ("jmp",
            [
                op1     ra1 [0xeb],         op1     ra2 [0x66, 0xe9], op1 ra4 [0xe9],
                op1CMRM rm2 [0x66, 0xff] 4, op1CMRM rm4 [0xff] 4
            ]
        ),

        -- load fence
        ("lfence", [op0CMRM [0x0f, 0xae] 5]),
        
        -- memory fence
        ("mfence", [op0CMRM [0x0f, 0xae] 6]),
        
        -- move an rvalue to an lvalue
        ("mov",
            [
                op2MRM  rm1 r1   [0x88],   op2MRM  rm2 r2   [0x66, 0x89],   op2MRM  rm4 r4   [0x89],
                op2MRM  r1  rm1  [0x8a],   op2MRM  r2  rm2  [0x66, 0x8b],   op2MRM  r4  rm4  [0x8b],
                op2A    r1  imm1 [0xb0],   op2A    r2  imm2 [0x66, 0xb8],   op2A    r4  imm4 [0xb8],
                op2CMRM rm1 imm1 [0xc6] 0, op2CMRM rm2 imm2 [0x66, 0xc7] 0, op2CMRM rm4 imm4 [0xc7] 0
            ]
        ),
        
        -- move with sign-extension
        ("movsx", [op2MRM r2 rm1 [0x66, 0x0f, 0xbe], op2MRM r4 rm1 [0x0f, 0xbe], op2MRM r4 rm2 [0x0f, 0xbf]]),
        
        -- move with zero-extend
        ("movzx", [op2MRM r2 rm1 [0x66, 0x0f, 0xb6], op2MRM r4 rm1 [0x0f, 0xb6], op2MRM r4 rm2 [0x0f, 0xb7]]),

        -- unsigned multiply
        ("mul", [op1CMRM rm1 [0xf6] 4, op1CMRM rm2 [0x66, 0xf7] 4, op1CMRM rm4 [0xf7] 4]),
        
        -- two's complement negation
        ("neg", [op1CMRM rm1 [0xf6] 3, op1CMRM rm2 [0x66, 0xf7] 3, op1CMRM rm4 [0xf7] 3]),
        
        -- no-op
        ("nop", [op0 [0x90]]),
        
        -- one's complement negation
        ("not", [op1CMRM rm1 [0xf6] 2, op1CMRM rm2 [0x66, 0xf7] 2, op1CMRM rm4 [0xf7] 2]),

        -- or (logical inclusive or)
        ("or",
            [
                op2 (cr1 "ax") imm1 [0x0c],
                op2 (cr2 "ax") imm2 [0x66, 0x0d],
                op2 (cr4 "ax") imm4 [0x0d],
                op2CMRM rm1 imm1 [0x80] 1,
                op2CMRM rm2 imm2 [0x66, 0x81] 1,
                op2CMRM rm4 imm4 [0x81] 1,
                op2CMRM rm2 imm1 [0x66, 0x83] 1,
                op2CMRM rm4 imm1 [0x83] 1,
                op2MRM  rm1 r1 [0x08],
                op2MRM  rm2 r2 [0x66, 0x09],
                op2MRM  rm4 r4 [0x09],
                op2MRM  r1 rm1 [0x0a],
                op2MRM  r2 rm2 [0x66, 0x0b],
                op2MRM  r4 rm4 [0x0b]
            ]
        ),
        
        -- pop a value from the stack
        ("pop",
            [
                op1A r2 [0x66, 0x58],
                op1A r4 [0x58],
                op1CMRM rm2 [0x66, 0x8f] 0,
                op1CMRM rm4 [0x8f] 0
            ]
        ),
        
        -- push (word or doubleword)
        ("push",
            [
                op1A r2 [0x66, 0x50],
                op1A r4 [0x50],
                op1CMRM rm2 [0x66, 0xff] 6,
                op1CMRM rm4 [0xff] 6,
                op1 imm1 [0x6a],
                op1 imm2 [0x66, 0x68],
                op1 imm4 [0x68]
            ]
        ),
        
        -- rotate (n*8)+1 bits left
        ("rcl",
            [
                op2CMRM rm1 imm1 [0xc0] 2,
                op2CMRM rm2 imm1 [0x66, 0xc1] 2,
                op2CMRM rm4 imm1 [0xc1] 2
            ]
        ),
        
        -- rotate (n*8)+1 bits right
        ("rcr",
            [
                op2CMRM rm1 imm1 [0xc0] 3,
                op2CMRM rm2 imm1 [0x66, 0xc1] 3,
                op2CMRM rm4 imm1 [0xc1] 3
            ]
        ),
        
        -- rotate n*8 bits left
        ("rol",
            [
                op2CMRM rm1 imm1 [0xc0] 0,
                op2CMRM rm2 imm1 [0x66, 0xc1] 0,
                op2CMRM rm4 imm1 [0xc1] 0
            ]
        ),
        
        -- rotate n*8 bits right
        ("ror",
            [
                op2CMRM rm1 imm1 [0xc0] 1,
                op2CMRM rm2 imm1 [0x66, 0xc1] 1,
                op2CMRM rm4 imm1 [0xc1] 1
            ]
        ),
        
        -- read performance-monitoring counter
        ("rdpmc", [op0 [0x0f, 0x33]]),

        -- read time-stamp counter
        ("rdtsc", [op0 [0x0f, 0x31]]),
        
        -- return from procedure
        ("ret", [op0 [0xc3], op1 imm2 [0xc2]]),

        -- l * 2**i
        ("sal",
            [
                op2CMRM rm1 imm1 [0xc0] 4,
                op2CMRM rm2 imm1 [0x66, 0xc1] 4,
                op2CMRM rm4 imm1 [0xc1] 4
            ]
        ),
        
        -- l / 2**i
        ("sar",
            [
                op2CMRM rm1 imm1 [0xc0] 7,
                op2CMRM rm2 imm1 [0x66, 0xc1] 7,
                op2CMRM rm4 imm1 [0xc1] 7
            ]
        ),
        
        -- l * 2**i
        ("shl",
            [
                op2CMRM rm1 imm1 [0xc0] 4,
                op2CMRM rm2 imm1 [0x66, 0xc1] 4,
                op2CMRM rm4 imm1 [0xc1] 4
            ]
        ),
        
        -- l / 2**i
        ("shr",
            [
                op2CMRM rm1 imm1 [0xc0] 5,
                op2CMRM rm2 imm1 [0x66, 0xc1] 5,
                op2CMRM rm4 imm1 [0xc1] 5
            ]
        ),
        
        -- integer subtract with borrow
        ("sbb",
            [
                op2 (cr1 "ax") imm1 [0x1c],
                op2 (cr2 "ax") imm2 [0x66, 0x1d],
                op2 (cr4 "ax") imm4 [0x1d],
                op2CMRM rm1 imm1 [0x80] 3,
                op2CMRM rm2 imm2 [0x66, 0x81] 3,
                op2CMRM rm4 imm4 [0x81] 3,
                op2CMRM rm2 imm1 [0x66, 0x83] 3,
                op2CMRM rm4 imm1 [0x83] 3,
                op2MRM r1 rm1 [0x1a],
                op2MRM r2 rm2 [0x66, 0x1b],
                op2MRM r4 rm4 [0x1b],
                op2MRM rm1 r1 [0x18],
                op2MRM rm2 r2 [0x66, 0x19],
                op2MRM rm4 r4 [0x19]
            ]
        ),
        
        -- sub
        ("sub",
            [
                op2     (cr1 "ax") imm1 [0x2c],         op2     (cr2 "ax") imm2 [0x66, 0x2d],   op2     (cr4 "ax") imm4 [0x2d],
                op2CMRM rm1        imm1 [0x80] 5,       op2CMRM rm2        imm2 [0x66, 0x81] 5, op2CMRM rm4        imm4 [0x81] 5,
                op2CMRM rm2        imm1 [0x66, 0x83] 5, op2CMRM rm4        imm1 [0x83] 5,
                op2MRM  rm1        r1   [0x28],         op2MRM  rm2        r2   [0x66, 0x29],   op2MRM  rm4        r4   [0x29]
            ]
        )
    ]

-- instruction encoding utilities
cr1 :: String -> OCArg
cr1 x = CReg 1 x

cr2 :: String -> OCArg
cr2 x = CReg 2 x

cr4 :: String -> OCArg
cr4 x = CReg 4 x

imm1 :: OCArg
imm1 = Imm 1

imm2 :: OCArg
imm2 = Imm 2

imm4 :: OCArg
imm4 = Imm 4

rm1 :: OCArg
rm1 = RM 1

rm2 :: OCArg
rm2 = RM 2

rm4 :: OCArg
rm4 = RM 4

r1 :: OCArg
r1 = R 1

r2 :: OCArg
r2 = R 2

r4 :: OCArg
r4 = R 4

ra1 :: OCArg
ra1 = RelAddr 1

ra2 :: OCArg
ra2 = RelAddr 2

ra4 :: OCArg
ra4 = RelAddr 4

op0 :: [Word8] -> (OpCall, OpEncoding)
op0 opcode = (OpCall [], OpEncoding opcode NoRegAdded NoModRM)

op0CMRM :: [Word8] -> Int -> (OpCall, OpEncoding)
op0CMRM opcode n = (OpCall [], OpEncoding opcode NoRegAdded (EncID n))

op0MRM :: [Word8] -> (OpCall, OpEncoding)
op0MRM opcode = (OpCall [], OpEncoding opcode NoRegAdded EncReg)

op1 :: OCArg -> [Word8] -> (OpCall, OpEncoding)
op1 a opcode = (OpCall [a], OpEncoding opcode NoRegAdded NoModRM)

op1A :: OCArg -> [Word8] -> (OpCall, OpEncoding)
op1A a opcode = (OpCall [a], OpEncoding opcode RegAdded NoModRM)

op1CMRM :: OCArg -> [Word8] -> Int -> (OpCall, OpEncoding)
op1CMRM a opcode n = (OpCall [a], OpEncoding opcode NoRegAdded (EncID n))

op1MRM :: OCArg -> [Word8] -> (OpCall, OpEncoding)
op1MRM a opcode = (OpCall [a], OpEncoding opcode NoRegAdded EncReg)

op2 :: OCArg -> OCArg -> [Word8] -> (OpCall, OpEncoding)
op2 a0 a1 opcode = (OpCall [a0, a1], OpEncoding opcode NoRegAdded NoModRM)

op2A :: OCArg -> OCArg -> [Word8] -> (OpCall, OpEncoding)
op2A a0 a1 opcode = (OpCall [a0, a1], OpEncoding opcode RegAdded NoModRM)

op2CMRM :: OCArg -> OCArg -> [Word8] -> Int -> (OpCall, OpEncoding)
op2CMRM a0 a1 opcode n = (OpCall [a0, a1], OpEncoding opcode NoRegAdded (EncID n))

op2MRM :: OCArg -> OCArg -> [Word8] -> (OpCall, OpEncoding)
op2MRM a0 a1 opcode = (OpCall [a0, a1], OpEncoding opcode NoRegAdded EncReg)

op3 :: OCArg -> OCArg -> OCArg -> [Word8] -> (OpCall, OpEncoding)
op3 a0 a1 a2 opcode = (OpCall [a0, a1, a2], OpEncoding opcode NoRegAdded NoModRM)

op3CMRM :: OCArg -> OCArg -> OCArg -> [Word8] -> Int -> (OpCall, OpEncoding)
op3CMRM a0 a1 a2 opcode n = (OpCall [a0, a1, a2], OpEncoding opcode NoRegAdded (EncID n))

op3MRM :: OCArg -> OCArg -> OCArg -> [Word8] -> (OpCall, OpEncoding)
op3MRM a0 a1 a2 opcode = (OpCall [a0, a1, a2], OpEncoding opcode NoRegAdded EncReg)

mod_rm :: Bits b => b -> b -> b -> b
mod_rm f r rm = (shiftL f 6) .|. (shiftL r 3) .|. (shiftL rm 0)

sib :: Bits b => b -> b -> b -> b
sib ss i r = (shiftL ss 6) .|. (shiftL i 3) .|. (shiftL r 0)

reg_code :: Num n => String -> n
reg_code "ax" = 0
reg_code "bx" = 3
reg_code "cx" = 1
reg_code "dx" = 2
reg_code "sp" = 4
reg_code "bp" = 5
reg_code "si" = 6
reg_code "di" = 7
reg_code x    = error ("Invalid register: " ++ x)

r32_code :: Num n => String -> n
r32_code x = reg_code x

r16_code :: Num n => String -> n
r16_code x = reg_code x

r8_code :: Num n => String -> n
r8_code x = reg_code x
