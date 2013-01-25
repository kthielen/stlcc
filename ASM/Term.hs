{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.Term where
import ASM.Type

import Util.Annotated
import Util.CFG
import Util.Num
import Util.String

import GHC.Word
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State

{-
    Primitive assembly instructions
-}
type InferArglFn a = [OpArg a] -> [OpArg a] -> [String] -> [OpArg a]
data Inst a = Op a String [OpArg a] [OpArg a] (InferArglFn a) [String] -- dst[0..N] <- OP(src[0..M]); jumpto lbl[0..L]
            | LblDef a String                                          -- lbl:
            | DataDef a String [Word8]                                 -- data { s = DATA }
            | RMov a (Ty a) String String                              -- dst <- src

data OpArg a = OAReg       a (Ty a) String          -- rN
             | OARMemOff   a (Ty a) String Int      -- [rN+i]
             | OAFrameSlot a (Ty a) Int FSTy        -- [sp+frameOffset(i)]
             | OAConst     a (CPrim a)              -- C
             | OACConst    a (Ty a) String          -- C (label reference)
             | OACMemOff   a (Ty a) Int             -- [C]
             | OACCMemOff  a (Ty a) String          -- [C] (label reference)
             deriving (Eq, Ord)

data FSTy  = Param | Local | RetSlot | TailParam Int deriving (Eq, Ord, Show)
data Reg a = Reg (Ty a) String deriving Show

instance Annotated Inst where
    annotation (Op x _ _ _ _ _) = x
    annotation (LblDef x _)     = x
    annotation (DataDef x _ _)  = x
    annotation (RMov x _ _ _)   = x
    
    annotations (Op x _ dst src _ _) = concatMap annotations (dst++src) ++ [x]
    annotations (LblDef x _)         = [x]
    annotations (DataDef x _ _)      = [x]
    annotations (RMov x _ _ _)       = [x]

instance Annotated OpArg where
    annotation (OAReg       x _ _)   = x
    annotation (OARMemOff   x _ _ _) = x
    annotation (OAFrameSlot x _ _ _) = x
    annotation (OAConst     x _)     = x
    annotation (OACConst    x _ _)   = x
    annotation (OACMemOff   x _ _)   = x
    annotation (OACCMemOff  x _ _)   = x
    
    annotations (OAReg       x t _)   = annotations t ++ [x]
    annotations (OARMemOff   x t _ _) = annotations t ++ [x]
    annotations (OAFrameSlot x t _ _) = annotations t ++ [x]
    annotations (OAConst     x _)     = [x]
    annotations (OACConst    x t _)   = annotations t ++ [x]
    annotations (OACMemOff   x t _)   = annotations t ++ [x]
    annotations (OACCMemOff  x t _)   = annotations t ++ [x]

instance Eq a => Eq (Inst a) where
    (Op _ s dst src f lbls) == (Op _ s' dst' src' f' lbls') = s == s' && (f dst src lbls) == (f' dst' src' lbls')
    (LblDef _ s)            == (LblDef _ s')                = s == s'
    (DataDef _ s d)         == (DataDef _ s' d')            = s == s' && d == d'
    (RMov _ ty d s)         == (RMov _ ty' d' s')           = ty == ty' && d == d' && s == s'
    _                       == _                            = False
    
instance Ord a => Ord (Inst a) where
    compare (Op _ s dst src f lbls) (Op _ s' dst' src' f' lbls') = compare (s, f dst src lbls) (s', f' dst' src' lbls')
    compare (LblDef _ s)            (LblDef _ s')                = compare s s'
    compare (DataDef _ s d)         (DataDef _ s' d')            = compare (s, d) (s', d')
    compare (RMov _ ty d s)         (RMov _ ty' d' s')           = compare (ty, d, s) (ty', d', s')
    compare (Op _ _ _ _ _ _)        _                            = GT
    compare (RMov _ _ _ _)          _                            = LT
    compare _                       (Op _ _ _ _ _ _)             = LT
    compare _                       (RMov _ _ _ _)               = GT
    compare (LblDef _ _)            _                            = GT
    compare (DataDef _ _ _)         _                            = LT

argTy :: OpArg a -> Ty a
argTy (OAReg       _ ty _)   = ty
argTy (OARMemOff   _ ty _ _) = ty
argTy (OAFrameSlot _ ty _ _) = ty
argTy (OAConst     _ x)      = primTy x
argTy (OACConst    _ ty _)   = ty
argTy (OACMemOff   _ ty _)   = ty
argTy (OACCMemOff  _ ty _)   = ty

data UOp  = UNeg | ULoByte | UHiByte | ULoShort | UHiShort | UCastByteToShort | UCastShortToByte deriving Eq
data BOp  = BAdd | BSub | BMul | BDiv | BMod | BShiftL | BShiftR | BOr | BAnd | BXor deriving Eq
data ROp  = REq | RNeq | RLt | RLte | RGt | RGte deriving Eq
    
instance (Annotation a, Eq a) => Instruction (Inst a) where
    inst_type (LblDef _ s)              = LabelDef s
    inst_type (DataDef _ _ _)           = NoCFlow
    inst_type (Op _ "ret" _ _ _ _)      = JumpSrc []
    inst_type (Op _ "tailcall" _ _ _ _) = JumpSrc []
    inst_type (Op _ _ _ _ _ [])         = NoCFlow
    inst_type (Op _ _ _ _ _ lbls)       = JumpSrc lbls
    inst_type (RMov _ _ _ _)            = NoCFlow
    label s                             = LblDef nullAnnotation s
    jump                                = jmp nullAnnotation
    invert (Op p o d s f [tl,fl])       = Op p (rop_jump $ corop $ jump_rop o) d s f [fl,tl]

instance Show UOp where
    show UNeg = "~"
    
instance Show BOp where
    show BAdd = "+"
    show BSub = "-"
    show BMul = "*"
    show BDiv = "/"
    show BMod = "%"

instance Show ROp where
    show REq  = "="
    show RNeq  = "="
    show RLt  = "<"
    show RLte = "<="
    show RGt  = ">"
    show RGte = ">="

corop :: ROp -> ROp
corop REq  = RNeq
corop RNeq = REq
corop RLt  = RGte
corop RGte = RLt
corop RLte = RGt
corop RGt  = RLte

{-
    Basic machine info
-}
mregs       = ["ax", "cx", "dx", "bx", "si", "di", "bp"] -- all general-purpose registers
mregset     = Set.fromList mregs
cregs       = ["ax", "cx", "dx"]                         -- registers trashed by 'call'
retreg      = "ax"                                       -- the register where function returns are stored
tailcontreg = "cx"                                       -- the register used for tail calls (where necessary)

isMachineReg :: Reg a -> Bool
isMachineReg (Reg _ rn) = isMachineRegName rn

isMachineRegName :: String -> Bool
isMachineRegName "sp" = True
isMachineRegName s    = Set.member s mregset

mov :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
mov p (OAReg _ ty x) (OAReg _ ty' y) | sizeof ty == sizeof ty' = RMov p ty x y
mov p x              y                                         = realmov p x y

realmov :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
realmov p dst src = Op p "mov" [dst] [src] (\[dst] [src] _ -> [dst, src]) []

movsx :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
movsx p dst src = Op p "movsx" [dst] [src] (\[dst] [src] _ -> [dst, src]) []

movzx :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
movzx p dst src = Op p "movzx" [dst] [src] (\[dst] [src] _ -> [dst, src]) []

push :: Annotation a => a -> OpArg a -> Inst a
push p src = Op p "push" [] [src] (\[] [src] _ -> [src]) []

pop :: Annotation a => a -> OpArg a -> Inst a
pop p dst = Op p "pop" [dst] [] (\[dst] _ _ -> [dst]) []

cmp :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
cmp p v1 v2 = Op p "cmp" [] [v1, v2] (\[] [v1, v2] _ -> [v1, v2]) []

jmp :: Annotation a => a -> String -> Inst a
jmp p s = Op p "jmp" [] [] (\_ _ [s] -> [OACConst p (TPrim p "int") s]) [s]

jmpArg :: Annotation a => a -> OpArg a -> Inst a
jmpArg p x = Op p "jmp" [] [x] (\_ [x] _ -> [x]) []

jcc :: Annotation a => a -> String -> String -> String -> Inst a
jcc p rop tlbl flbl = Op p rop [] [] (\_ _ [tlbl, flbl] -> [OACConst p (TPrim p "int") tlbl]) [tlbl, flbl]

ret :: Annotation a => a -> Int -> Inst a
ret p 0 = Op p "ret" [] [] (\_ _ _ -> []) []
ret p x = Op p "ret" [] [OAConst p $ CShort p (castn x)] (\_ _ _ -> [OAConst p $ CShort p (castn x)]) []

iuop :: Annotation a => a -> String -> OpArg a -> OpArg a -> Inst a
iuop p o dst src = Op p o [dst] [src] (\[dst] [src] _ -> [dst, src]) []

ibop :: Annotation a => a -> String -> OpArg a -> OpArg a -> Inst a
ibop p o dst src = Op p o [dst] [dst, src] (\_ [dst, src] _ -> [dst, src]) []

sub :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
sub p dst src = ibop p "sub" dst src

add :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
add p dst src = ibop p "add" dst src

call :: Annotation a => a -> Ty a -> OpArg a -> Inst a
call p ty src = Op p "call" (map (OAReg p ty) cregs) [src] (\_ [src] _ -> [src]) []

idiv :: Annotation a => a -> Ty a -> OpArg a -> Inst a
idiv p ty src = Op p "idiv" [OAReg p ty "ax", OAReg p ty "dx"] [src] (\_ [src] _ -> [src]) []

tailcall :: Annotation a => a -> OpArg a -> OpArg a -> Inst a
tailcall p cid x = Op p "tailcall" [] [cid, x] (\_ args _ -> args) []

rop_jump :: ROp -> String
rop_jump REq  = "je"
rop_jump RNeq = "jne"
rop_jump RLt  = "jl"
rop_jump RLte = "jle"
rop_jump RGt  = "jg"
rop_jump RGte = "jge"

jump_rop :: String -> ROp
jump_rop "je"  = REq
jump_rop "jne" = RNeq
jump_rop "jl"  = RLt
jump_rop "jle" = RLte
jump_rop "jg"  = RGt
jump_rop "jge" = RGte

instArgs :: Inst a -> [OpArg a]
instArgs (Op _ _ dsts srcs f lbls) = f dsts srcs lbls
instArgs _ = []

inferOpDef :: Annotation a => a -> String -> [OpArg a] -> Inst a
inferOpDef p "mov"   [dst, src]          = Op p "mov"   [dst] [src] (\[dst] [src] _ -> [dst, src]) []
inferOpDef p "movsx" [dst, src]          = Op p "movsx" [dst] [src] (\[dst] [src] _ -> [dst, src]) []
inferOpDef p "movzx" [dst, src]          = Op p "movzx" [dst] [src] (\[dst] [src] _ -> [dst, src]) []
inferOpDef p "push"  [src]               = Op p "push"  [] [src] (\[] [src] _ -> [src]) []
inferOpDef p "pop"   [dst]               = Op p "pop"   [dst] [] (\[dst] _ _ -> [dst]) []
inferOpDef p "cmp"   [v1, v2]            = Op p "cmp"   [] [v1, v2] (\[] [v1, v2] _ -> [v1, v2]) []
inferOpDef p "jmp"   [OACConst _ _ s]    = Op p "jmp"   [] [] (\_ _ [s] -> [OACConst p (TPrim p "int") s]) [s]
inferOpDef p "je"    [OACConst _ _ tlbl] = Op p "je"    [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "jne"   [OACConst _ _ tlbl] = Op p "jne"   [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "jl"    [OACConst _ _ tlbl] = Op p "jl"    [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "jle"   [OACConst _ _ tlbl] = Op p "jle"   [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "jg"    [OACConst _ _ tlbl] = Op p "jg"    [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "jge"   [OACConst _ _ tlbl] = Op p "jge"   [] [] (\_ _ (tlbl:_) -> [OACConst p (TPrim p "int") tlbl]) [tlbl]
inferOpDef p "ret"   src                 = Op p "ret"   [] src (\_ _ _ -> src) []
inferOpDef p "neg"   [dst, src]          = Op p "neg"   [dst] [src] (\[dst] [src] _ -> [dst, src]) []
inferOpDef p "sub"   [dst, src]          = Op p "sub"   [dst] [dst, src] (\_ [dst, src] _ -> [dst, src]) []
inferOpDef p "add"   [dst, src]          = Op p "add"   [dst] [dst, src] (\_ [dst, src] _ -> [dst, src]) []
inferOpDef p "call"  [src]               = Op p "call"  (map (OAReg p (TPrim p "int")) cregs) [src] (\_ [src] _ -> [src]) []
inferOpDef p "idiv"  [src]               = Op p "idiv"  [OAReg p (TPrim p "int") "ax", OAReg p (TPrim p "int") "dx"] [src] (\_ [src] _ -> [src]) []
inferOpDef p "inc"   [src]               = Op p "inc"   [src] [src] (\_ x _ -> x) []
inferOpDef p o       argl                = error ("Unsupported assembly instruction: " ++ show (Op p o [] [] (\_ _ _ -> argl) []))

instance Annotation a => Show (Inst a) where
    show (Op _ "mov" [d] [s]     _ _) = show d ++ " <- " ++ show s
    show (Op _ "add" [d] [s1,s2] _ _) = show d ++ " <- " ++ show s1 ++ " + " ++ show s2
    show (Op _ "sub" [d] [s1,s2] _ _) = show d ++ " <- " ++ show s1 ++ " - " ++ show s2
    show (Op _ "mul" [d] [s1,s2] _ _) = show d ++ " <- " ++ show s1 ++ " * " ++ show s2
    show (Op _ "div" [d] [s1,s2] _ _) = show d ++ " <- " ++ show s1 ++ " / " ++ show s2
    
    show (Op _ o [] s _ [])       = o ++ " " ++ cdelim (map show s) ", "
    show (Op _ o d s _ [])        = cdelim (map show d) ", " ++ " <- " ++ o ++ " " ++ cdelim (map show s) ", "
    show (Op _ o [] [] _ [lbl])   = o ++ " " ++ lbl
    show (Op _ o [] [] _ lbls)    = o ++ " " ++ cdelim lbls ", "
    show (LblDef _ lbl)           = lbl ++ ":"
    show (DataDef _ lbl d)        = "data " ++ lbl ++ " = 0x" ++ concat (map hexByte d)
    show (RMov _ ty dst src)      = dst ++ " <- " ++ src ++ "::" ++ show (sizeof ty)

instance Annotation a => Show (OpArg a) where
    show (OAReg _ ty r)        = r ++ "::" ++ show (sizeof ty)
    show (OARMemOff _ ty r 0)  = "[" ++ r ++ "::" ++ show (sizeof ty) ++ "]"
    show (OARMemOff _ ty r i)  = "[" ++ r ++  "::" ++ show (sizeof ty) ++ "+" ++ show i ++ "]"
    show (OAFrameSlot _ _ i _) = "[sp+frameOffset[" ++ show i ++ "]]"
    show (OAConst _ c)         = show c
    show (OACConst _ _ lbl)    = lbl
    show (OACMemOff _ _ c)     = "[" ++ show c ++ "]"
    show (OACCMemOff _ _ lbl)  = "[" ++ lbl ++ "]"
    
instance Eq (Reg a) where
    (Reg _ s0) == (Reg _ s1) = s0 == s1
    
instance Ord (Reg a) where
    compare (Reg _ s0) (Reg _ s1) = compare s0 s1
    
inst_reg_uses :: Inst a -> [Reg a]
inst_reg_uses (LblDef _ _)           = []
inst_reg_uses (DataDef _ _ _)        = []
inst_reg_uses (RMov _ ty _ src)      = [Reg ty src]
inst_reg_uses (Op _ _ dsts srcs _ _) = (dst_refs dsts) ++ (src_refs srcs) where
    dst_refs ((OARMemOff _ ty rn _):ds) = (Reg ty rn):(dst_refs ds)
    dst_refs (_:ds)                     = dst_refs ds
    dst_refs []                         = []
    src_refs ((OAReg _ ty rn):ss)       = (Reg ty rn):(src_refs ss)
    src_refs ((OARMemOff _ ty rn _):ss) = (Reg ty rn):(src_refs ss)
    src_refs (_:ss)                     = src_refs ss
    src_refs []                         = []

mapRegUses :: (Reg a -> Reg a) -> Inst a -> Inst a
mapRegUses _ (LblDef p x)              = LblDef p x
mapRegUses _ (DataDef p x y)           = DataDef p x y
mapRegUses f (RMov p ty dst src)       = RMov p ty dst src' where Reg _ src' = f (Reg ty src)
mapRegUses f (Op p o dsts srcs g jmps) = Op p o (map tfnDst dsts) (map tfnSrc srcs) g jmps where
    tfnDst (OARMemOff p ty rn x) = OARMemOff p ty rn' x where Reg _ rn' = f (Reg ty rn)
    tfnDst a                     = a
    
    tfnSrc (OAReg p ty rn)       = OAReg p ty rn' where Reg _ rn' = f (Reg ty rn)
    tfnSrc (OARMemOff p ty rn n) = OARMemOff p ty rn' n where Reg _ rn' = f (Reg ty rn)
    tfnSrc a                     = a

inst_reg_defs :: Inst a -> [Reg a]
inst_reg_defs (LblDef _ _)        = []
inst_reg_defs (DataDef _ _ _)     = []
inst_reg_defs (RMov _ ty dst _)   = [Reg ty dst]
inst_reg_defs (Op _ _ dsts _ _ _) = reg_defs dsts where
    reg_defs ((OAReg _ ty rn):ds) = (Reg ty rn):(reg_defs ds)
    reg_defs (_:ds)               = reg_defs ds
    reg_defs []                   = []

mapRegDefs :: (Reg a -> Reg a) -> Inst a -> Inst a
mapRegDefs _ (LblDef p x)              = LblDef p x
mapRegDefs _ (DataDef p x y)           = DataDef p x y
mapRegDefs f (RMov p ty dst src)       = RMov p ty dst' src where Reg _ dst' = f (Reg ty dst)
mapRegDefs f (Op p o dsts srcs g jmps) = Op p o dsts' srcs g jmps where
    dsts' = map tfnReg dsts
    tfnReg (OAReg p ty rn) = OAReg p ty rn' where Reg _ rn' = f (Reg ty rn)
    tfnReg a               = a

inst_reg_refs :: Inst a -> [Reg a]
inst_reg_refs i = inst_reg_uses i ++ inst_reg_defs i

is_reg_move :: Inst a -> Bool
is_reg_move (RMov _ _ _ _) = True
is_reg_move _              = False

mapRegs :: (Reg a -> Reg a) -> Inst a -> Inst a
mapRegs _ (LblDef p x)              = LblDef p x
mapRegs _ (DataDef p x y)           = DataDef p x y
mapRegs f (RMov p ty dst src)       = RMov p ty dst' src' where
    Reg _ dst' = f (Reg ty dst)
    Reg _ src' = f (Reg ty src)
mapRegs f (Op p o dsts srcs g jmps) = Op p o (map tfn dsts) (map tfn srcs) g jmps where
    tfn (OAReg p ty rn)       = OAReg p ty rn' where Reg _ rn' = f (Reg ty rn)
    tfn (OARMemOff p ty rn n) = OARMemOff p ty rn' n where Reg _ rn' = f (Reg ty rn)
    tfn a                     = a
