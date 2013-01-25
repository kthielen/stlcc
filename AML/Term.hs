{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module AML.Term where
import AML.Type

import Util.Annotated
import Util.CFG
import Util.String
import Util.Sequence
import Util.State

import Control.Monad.State

{-
	Explicit memory and control flow
-}
data Expr a = Malloc a ALoc (Ty a) Int          -- allocate sizeof(Ty)*i and return its address
            | LName a String                    -- a label reference
            | Const a (CPrim a)                 -- a constant value (int, float, char, bool, ...)
            | Mem a (Ty a) (Expr a)             -- *((Ty*)e)
            | TReg a (Ty a) String              -- r
            | UOp a UOp (Expr a)                -- o(e)
            | BOp a BOp (Expr a) (Expr a)       -- o(e0, e1)
            | Call a (Ty a) (Expr a) [Expr a]   -- e0(e1,e2,...,eN) as Ty
            | ESeq a (Stmt a) (Expr a)          -- (s0, e0)
            deriving Eq

data Stmt a = Move a (Expr a) (Expr a)                    -- e0 <- e1
            | EStmt a (Expr a)                            -- evaluate expr and discard its result
            | Arg a (Ty a) String                         -- pop a       (or otherwise read a function argument)
            | Label a String                              -- lbl:
            | Jump a String                               -- jump lbl
            | CJump a ROp (Expr a) (Expr a) String String -- if r(e0,e1) then jump lbl0 else jump lbl1
            | Return a (Expr a)                           -- return e0
            | SSeq a (Stmt a) (Stmt a)                    -- s0; s1
            deriving Eq

data ALoc = Heap deriving Eq
data UOp  = UNeg | ULoByte | UHiByte | ULoShort | UHiShort | UCastByteToShort | UCastShortToByte deriving Eq
data BOp  = BAdd | BSub | BMul | BDiv | BMod | BShiftL | BShiftR | BOr | BAnd | BXor deriving Eq
data ROp  = REq | RNeq | RLt | RLte | RGt | RGte deriving Eq

instance Eq a => Instruction (Stmt a) where
    inst_type (Label _ s)           = LabelDef s
    inst_type (CJump _ _ _ _ tl fl) = JumpSrc [tl, fl]
    inst_type (Jump _ lbl)          = JumpSrc [lbl]
    inst_type (Return _ _)          = JumpSrc []
    inst_type _                     = NoCFlow
    label s                         = Label undefined s
    jump                            = Jump undefined
    invert (CJump p o e0 e1 tl fl)  = CJump p (corop o) e0 e1 fl tl

instance Annotated Expr where
    annotation (Malloc a _ _ _) = a
    annotation (LName a _)      = a
    annotation (Const a _)      = a
    annotation (Mem a _ _)      = a
    annotation (TReg a _ _)     = a
    annotation (UOp a _ _)      = a
    annotation (BOp a _ _ _)    = a
    annotation (Call a _ _ _)   = a
    annotation (ESeq a _ _)     = a
    annotations (Malloc a _ ty _)  = a : annotations ty
    annotations (LName a _)        = [a]
    annotations (Const a _)        = [a]
    annotations (Mem a t e)        = a : annotations t ++ annotations e
    annotations (TReg a ty _)      = a : annotations ty
    annotations (UOp a _ e)        = a : annotations e
    annotations (BOp a _ l r)      = a : annotations l ++ annotations r
    annotations (Call a ty fe aes) = a : annotations ty ++ annotations fe ++ concatMap annotations aes
    annotations (ESeq a s e)       = a : annotations s ++ annotations e

instance Annotated Stmt where
    annotation (Move a _ _)        = a
    annotation (EStmt a _)         = a
    annotation (Arg a _ _)         = a
    annotation (Label a _)         = a
    annotation (Jump a _)          = a
    annotation (CJump a _ _ _ _ _) = a
    annotation (Return a _)        = a
    annotation (SSeq a _ _)        = a
    annotations (Move a l r)        = a : annotations l ++ annotations r
    annotations (EStmt a e)         = a : annotations e
    annotations (Arg a ty _)        = a : annotations ty
    annotations (Label a _)         = [a]
    annotations (Jump a _)          = [a]
    annotations (CJump a _ l r _ _) = a : annotations l ++ annotations r
    annotations (Return a e)        = a : annotations e
    annotations (SSeq a l r)        = a : annotations l ++ annotations r

instance Show (Expr a) where
    show (Malloc _ Heap  t 0) = "new " ++ show t
    show (Malloc _ Heap  t n) = "new " ++ show t ++ "[" ++ show n ++ "]"
    show (LName _ lbl)        = lbl
    show (Const _ x)          = show x
    show (Mem _ t e)          = "*(" ++ show e ++ ")"
    show (TReg _ t r)         = r
    show (UOp _ o e)          = show o ++ "(" ++ show e ++ ")"
    show (BOp _ o l r)        = show l ++ " " ++ show o ++ " " ++ show r
    show (Call _ t fe aes)    = "call [" ++ show fe ++ "](" ++ cdelim (map show aes) ", " ++ ")"
    show (ESeq _ s e)         = "do { " ++ show s ++ "; return " ++ show e ++ " }"
    
instance Show (Stmt a) where
    show (Move _ l e)            = show l ++ " <- " ++ show e
    show (EStmt _ e)             = show e
    show (Arg _ ty vn)           = "arg " ++ vn
    show (Label _ lbl)           = lbl ++ ":"
    show (Jump _ lbl)            = "jump " ++ lbl
    show (CJump _ o e0 e1 tl fl) = "if (" ++ show e0 ++ " " ++ show o ++ " " ++ show e1 ++ ") then jump " ++ tl ++ " else jump " ++ fl
    show (Return _ e)            = "return " ++ show e
    show (SSeq _ s0 s1)          = show s0 ++ "; " ++ show s1

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

sseq :: [Stmt a] -> Stmt a
sseq [s]    = s
sseq (s:ss) = SSeq (annotation s) s (sseq ss)

exprTy :: Expr a -> Ty a
exprTy (Malloc a _ t _) = TPrim a "int"
exprTy (LName a lbl)    = TPrim a "int"
exprTy (Const _ x)      = primTy x
exprTy (Mem _ t _)      = t
exprTy (TReg _ t _)     = t
exprTy (UOp _ o e)      = uopTy o e
exprTy (BOp _ o e0 e1)  = bopTy o e0 e1
exprTy (Call _ t _ _)   = t
exprTy (ESeq _ _ e)     = exprTy e

uopTy :: UOp -> Expr a -> Ty a
uopTy UNeg             e = exprTy e
uopTy ULoByte          e = TPrim (annotation e) "byte"
uopTy UHiByte          e = TPrim (annotation e) "byte"
uopTy UCastShortToByte e = TPrim (annotation e) "byte"
uopTy ULoShort         e = TPrim (annotation e) "short"
uopTy UHiShort         e = TPrim (annotation e) "short"
uopTy UCastByteToShort e = TPrim (annotation e) "short"

bopTy :: BOp -> Expr a -> Expr a -> Ty a
bopTy _ e0 _ = exprTy e0

{-
    Statement/expression linearization
-}
nullStmt' :: Annotation a => a -> Stmt a
nullStmt' a = EStmt a (Const a (CUnit a))

nullStmt :: Annotation a => Stmt a
nullStmt = nullStmt' nullAnnotation

unseqStmt :: Annotation a => Stmt a -> State Int (Stmt a)
unseqStmt (Move a (LName a' s)  se)              = reorderStmt [se]  $ \[se'] -> Move a (LName a' s) se'
unseqStmt (Move a (TReg a' t n) se)              = reorderStmt [se]  $ \[se'] -> Move a (TReg a' t n) se'
unseqStmt (Move a (Mem a' t e)  se) | constLoc e = reorderStmt [se]  $ \[se'] -> Move a (Mem a' t e) se' where
    constLoc (Const _ _)                                    = True
    constLoc (LName _ _)                                    = True
    constLoc (TReg _ _ _)                                   = True
    constLoc (BOp _ BAdd (Const _ (CInt _ _)) (TReg _ _ _)) = True
    constLoc (BOp _ BAdd (TReg _ _ _) (Const _ (CInt _ _))) = True
    constLoc _                                              = False
unseqStmt (Move a (Mem a' t e)  se)    = reorderStmt [e, se]  $ \[e',  se'] -> Move a (Mem a' t e') se'
unseqStmt (EStmt a e)                  = reorderStmt [e]      $ \[e']       -> EStmt a e'
unseqStmt (CJump a ro fe se tlbl flbl) = reorderStmt [fe, se] $ \[fe', se'] -> CJump a ro fe' se' tlbl flbl
unseqStmt (Return a e)                 = reorderStmt [e]      $ \[e']       -> Return a e'
unseqStmt (Arg a ty vn)                = return (Arg a ty vn)
unseqStmt (Label a lbl)                = return (Label a lbl)
unseqStmt (Jump a lbl)                 = return (Jump a lbl)
unseqStmt (SSeq a fs ss) = do
    fs' <- unseqStmt fs;
    ss' <- unseqStmt ss;
    return (unjunkStmt fs' ss')

unseqExpr :: Annotation a => Expr a -> State Int (Stmt a, Expr a)
unseqExpr (Mem a t e)       = reorderExpr [e] $ \[e'] -> Mem a t e'
unseqExpr (UOp a o e)       = reorderExpr [e] $ \[e'] -> UOp a o e'
unseqExpr (BOp a o le re)   = reorderExpr [le, re] $ \[le', re'] -> BOp a o le' re'
unseqExpr (Call a t fe aes) = reorderExpr (fe:aes) $ \(fe':aes') -> Call a t fe' aes'
unseqExpr (Malloc a l t n)  = return (nullStmt' a, Malloc a l t n)
unseqExpr (LName a lbl)     = return (nullStmt' a, LName a lbl)
unseqExpr (Const a x)       = return (nullStmt' a, Const a x)
unseqExpr (TReg a t n)      = return (nullStmt' a, TReg a t n)
unseqExpr (ESeq a s e)      = do
    s'        <- unseqStmt s;
    (s'', e') <- unseqExpr e;
    return (unjunkStmt s' s'', e')

reorderStmt :: Annotation a => [Expr a] -> ([Expr a] -> Stmt a) -> State Int (Stmt a)
reorderStmt es f = do
    ses'     <- mapM unseqExpr es;
    (s, ees) <- collapseStmts ses';
    return (sseq [s, f ees])

reorderExpr :: Annotation a => [Expr a] -> ([Expr a] -> Expr a) -> State Int (Stmt a, Expr a)
reorderExpr es f = do
    ses'     <- mapM unseqExpr es;
    (s, ees) <- collapseStmts ses';
    return (s, f ees)

collapseStmts :: Annotation a => [(Stmt a, Expr a)] -> State Int (Stmt a, [Expr a])
collapseStmts ses = foldrM csf (nullStmt, []) ses where
    csf (s0, le) (s1, res) | commute s1 le = return (sseq [s0,s1], le:res)
    csf (s0, le) (s1, res)                 = do
        t <- fresh ((TReg nullAnnotation (exprTy le)) . prefix "t");
        return (sseq [s0, Move nullAnnotation t le, s1], t:res)

commute :: Stmt a -> Expr a -> Bool
commute (EStmt _ (Const _ _)) _             = True
commute _                     (LName _ _)   = True
commute _                     (TReg  _ _ _) = True
commute _                     (Const _ _)   = True
commute _ _                                 = False

unjunkStmt :: Stmt a -> Stmt a -> Stmt a
unjunkStmt (EStmt _ (Const _ _)) s  = s
unjunkStmt s (EStmt _ (Const _ _))  = s
unjunkStmt s0 s1                    = sseq [s0, s1]

unjunkStmts :: [Stmt a] -> [Stmt a]
unjunkStmts ss = filter (not . isJunk) ss where
    isJunk (EStmt _ (Const _ _)) = True
    isJunk _                     = False

flattenStmt :: Stmt a -> [Stmt a]
flattenStmt (SSeq _ s0 s1) = flattenStmt s0 ++ flattenStmt s1
flattenStmt s              = [s]

showStmts :: [Stmt a] -> String
showStmts ss = concat [show s ++ "\n" | s <- ss]
