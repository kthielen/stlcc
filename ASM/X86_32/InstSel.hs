{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.X86_32.InstSel where
import ASM.Term
import ASM.Type
import qualified AML.Term as A
import qualified AML.Type as AT

import Util.Annotated
import Util.CFG
import Util.String
import Util.Sequence
import Util.Tuples
import Util.State

import GHC.Word
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import Control.Monad
import Control.Monad.State

{-
    Type translation
-}
translateTy :: AT.Ty a -> Ty a
translateTy (AT.TPrim    a x)         = TPrim a x
translateTy (AT.TVar     a x)         = TVar a x
translateTy (AT.TFn      a cc ats rt) = TFn a (translateCC cc) (map translateTy ats) (translateTy rt)
translateTy (AT.TVariant a fs)        = TVariant a [(s, translateTy ty) | (s, ty) <- fs]
translateTy (AT.TRecord  a fs)        = TRecord a [(s, translateTy ty) | (s, ty) <- fs]
translateTy (AT.TExists  a v t)       = TExists a v (translateTy t)
translateTy (AT.TArray   a t)         = TArray a (translateTy t)
translateTy (AT.TMu      a v t)       = TMu a v (translateTy t)
translateTy (AT.TPtr     a t)         = TPtr a (translateTy t)

translateCC :: AT.CallConv -> CallConv
translateCC AT.CFn = CFn

translateConst :: AT.CPrim a -> CPrim a
translateConst (AT.CByte a x)   = CByte a x
translateConst (AT.CChar a x)   = CChar a x
translateConst (AT.CShort a x)  = CShort a x
translateConst (AT.CInt a x)    = CInt a x
translateConst (AT.CFloat a x)  = CFloat a x
translateConst (AT.CString a x) = CString a x
translateConst (AT.CUnit a)     = CUnit a

translateROp :: A.ROp -> ROp
translateROp A.REq  = REq
translateROp A.RNeq = RNeq
translateROp A.RLt  = RLt
translateROp A.RLte = RLte
translateROp A.RGt  = RGt
translateROp A.RGte = RGte

{-
    Statement translation
-}
translateCFG :: (Annotation a, Eq a) => CFG (A.Stmt a) -> State Int (CFG (Inst a))
translateCFG g = do
    is <- translateStmts 0 (serialize_cfg g);
    build_cfg is
    
translateStmts :: Annotation a => Int -> [A.Stmt a] -> State Int [Inst a]
translateStmts argidx [] = return []
translateStmts argidx ((A.Arg p ty vn):ss) | AT.sizeof ty == 1 = do
    let aty  = TPrim p "short";
    let amov = mov p (OAReg p aty vn) (OAFrameSlot p aty argidx Param);
    ss' <- translateStmts (argidx+1) ss;
    return (amov : ss')
translateStmts argidx ((A.Arg p ty vn):ss) = do
    let ty' = translateTy ty;
    let amov = mov p (OAReg p ty' vn) (OAFrameSlot p ty' argidx Param);
    ss' <- translateStmts (argidx+1) ss;
    return (amov : ss')
translateStmts argidx (s:ss) = do
    s'  <- translateStmt s;
    ss' <- translateStmts argidx ss;
    return (s' ++ ss')

translateStmt :: Annotation a => A.Stmt a -> State Int [Inst a]
translateStmt (A.Move p x y) = tfn2OpMC x y select where
    select (OAReg _ ty x') (OAReg _ _ y') = RMov p ty x' y'
    select x'              y'             = mov p x' y'

translateStmt (A.EStmt _ e) = do
    (eis, _, _) <- translateExpr e;
    return eis

translateStmt (A.Label p lbl) = return [LblDef p lbl]
translateStmt (A.Jump p lbl)  = return [jmp p lbl]
translateStmt (A.CJump p o e0 e1 tlbl flbl) = do
    ris <- tfn2OpMC e0 e1 (cmp p);
    return (ris ++ [jcc p (rop_jump (translateROp o)) tlbl flbl])

translateStmt (A.Return p (A.Call cp rty fe aes)) = do
    aes' <- mapM translateExpr aes;
    r    <- fresh (prefix "rr");
    cid  <- fresh id;
    let tint     = TPrim p "int";
    let aeis     = concat [is | (is, _, _) <- aes'];
    let args     = [OAReg cp ty r | (_, ty, r) <- aes'];
    let argln    = length aes;
    let getret   = [mov cp (OAReg cp tint r) (OAFrameSlot cp tint (-1) RetSlot)];
    let setret   = [mov cp (OAFrameSlot cp tint (-1) (TailParam cid)) (OAReg cp tint r)];
    let movframe = [mov cp (OAFrameSlot cp ty i (TailParam cid)) (OAReg cp ty r) | ((_, ty, r), i) <- zip aes' [0..]];
    finsts <- tfn1OpMCL fe (\x -> [mov cp (OAReg cp tint tailcontreg) x, tailcall cp (OAConst cp (CInt cp cid)) (OAReg cp tint tailcontreg)]);
    return (aeis ++ [first finsts] ++ getret ++ movframe ++ setret ++ [second finsts])
translateStmt (A.Return p e) = do
    ris <- tfn1OpMC e select;
    return (ris ++ [ret p 0])
    where
        select (OAReg _ ty r) = RMov p ty retreg r
        select x              = mov p (OAReg p (argTy x) retreg) x

{-
    Expression translation
-}
translateExpr :: Annotation a => A.Expr a -> State Int ([Inst a], Ty a, String)
translateExpr (A.Malloc p A.Heap ty n) = translateExpr (A.Call p (AT.TPtr p ty) (A.LName p "malloc") [A.Const p $ AT.CInt p $ n * AT.sizeof ty])

translateExpr (A.LName p lbl) = do
    r <- fresh (prefix "tr");
    let ity = TPrim p "int";
    return ([mov p (OAReg p ity r) (OACConst p ity lbl)], ity, r)
    
translateExpr (A.Const p x) = do
    r <- fresh (prefix "tr");
    let cty = translateTy (AT.primTy x);
    return ([mov p (OAReg p cty r) (OAConst p (translateConst x))], cty, r)
    
translateExpr (A.Mem p ty e) = do
    (eis, ety, er) <- translateExpr e;
    r              <- fresh (prefix "tr");
    insts          <- tfn1OpMCM e (select r);
    return $ case insts of
        Just insts' -> (insts', ty', r)
        _           -> (eis ++ [mov p (OAReg p ty' r) (OARMemOff p ety er 0)], ty', r)
    where
        ty' = translateTy ty
        select r (OAReg _ _ r')         = Just $ mov p (OAReg p ty' r) (OARMemOff p ty' r' 0)
        select r (OAConst _ (CInt _ x)) = Just $ mov p (OAReg p ty' r) (OACMemOff p ty' x)
        select r (OACConst _ _ s)       = Just $ mov p (OAReg p ty' r) (OACCMemOff p ty' s)
        select _ _                      = Nothing

translateExpr (A.TReg _ ty r) = return ([], translateTy ty, r)

translateExpr (A.UOp p A.UNeg e) = do
    let uty        =  translateTy (A.uopTy A.UNeg e);
    r              <- fresh (prefix "tr");
    insts          <- tfn1OpMC e (iuop p "neg" (OAReg p uty r));
    return (insts, uty, r)
translateExpr (A.UOp p A.UCastByteToShort e) = do
    let rty = TPrim p "short";
    r     <- fresh (prefix "tr");
    insts <- tfn1OpMC e (movzx p (OAReg p rty r));
    return (insts, rty, r)
translateExpr (A.UOp p A.UCastShortToByte e) = do
    let rty  = TPrim p "short";
    let rty' = TPrim p "byte";
    r <- fresh (prefix "tr");
    insts <- tfn1OpMC e (mov p (OAReg p rty r));
    return (insts, rty', r)

translateExpr (A.BOp p o e0 e1) = do
    let rty  = translateTy (A.bopTy o e0 e1);
    r       <- fresh (prefix "tr");
    insts   <- tfn2OpMCL e0 e1 (bopInsts p o rty r);
    return (insts, rty, r)

translateExpr (A.Call p ty fe aes) = do
    r     <- fresh (prefix "tr");
    ais   <- concatM (mapM pushExp aes);
    insts <- tfn1OpMCL fe (\x -> [call p ty' x, RMov p ty' r retreg]);
    return (ais ++ insts, ty', r)
    where
        ty' = translateTy ty
        concatM = liftM concat
        pushExp e | AT.sizeof (A.exprTy e) == 1 = tfn1OpMC (A.UOp p A.UCastByteToShort e) (push p)
        pushExp e                               = tfn1OpMC e (push p)

bopInsts :: Annotation a => a -> A.BOp -> Ty a -> String -> OpArg a -> OpArg a -> [Inst a]
bopInsts p A.BDiv ty r a0 a1 =
    [
        mov p (OAReg p ty "ax") a0,
        mov p (OAReg p ty "dx") (OAConst p (CInt p 0)),
        idiv p ty a1,
        mov p (OAReg p ty r) (OAReg p ty "ax")
    ]

bopInsts p A.BMod ty r a0 a1 =
    [
        mov p (OAReg p ty "ax") a0,
        mov p (OAReg p ty "dx") (OAConst p (CInt p 0)),
        idiv p ty a1,
        mov p (OAReg p ty r) (OAReg p ty "dx")
    ]

bopInsts p o ty r a0 a1 = [mov p (OAReg p ty r) a0, ibop p (bopName o) (OAReg p ty r) a1]

bopName :: A.BOp -> String
bopName A.BAdd = "add"
bopName A.BSub = "sub"
bopName A.BMul = "imul"
bopName _      = undefined

{-
    Simple utilities to automate some of the optimization of instruction selection
-}
tfn1OpMC :: Annotation a => A.Expr a -> (OpArg a -> Inst a) -> State Int [Inst a]
tfn1OpMC e f = tfn1OpMCL e (return . f)

tfn1OpMCL :: Annotation a => A.Expr a -> (OpArg a -> [Inst a]) -> State Int [Inst a]
tfn1OpMCL (A.Const p x)                                                              f = return $ f (OAConst p (translateConst x))
tfn1OpMCL (A.LName p s)                                                              f = return $ f (OACConst p (TPrim p "int") s)
tfn1OpMCL (A.Mem p ty (A.Const _ (AT.CInt _ x)))                                     f = return $ f (OACMemOff p (translateTy ty) x)
tfn1OpMCL (A.Mem p ty (A.LName _ s))                                                 f = return $ f (OACCMemOff p (translateTy ty) s)
tfn1OpMCL (A.Mem p ty (A.BOp _ A.BAdd (A.TReg _ _ e0r) (A.Const _ (AT.CInt _ off)))) f = return $ f (OARMemOff p (translateTy ty) e0r off)
tfn1OpMCL (A.Mem p ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg _ _ e0r))) f = return $ f (OARMemOff p (translateTy ty) e0r off)
tfn1OpMCL (A.Mem p ty e0) f = do
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OARMemOff p (translateTy ty) e0r 0)
tfn1OpMCL e f = do
    (eis, ty, er) <- translateExpr e;
    return $ eis ++ f (OAReg (annotation e) ty er)

tfn1OpMCM :: Annotation a => A.Expr a -> (OpArg a -> Maybe (Inst a)) -> State Int (Maybe [Inst a])
tfn1OpMCM (A.Const p x)                                                              f = liftMCM [] $ f (OAConst p (translateConst x))
tfn1OpMCM (A.LName p s)                                                              f = liftMCM [] $ f (OACConst p (TPrim p "int") s)
tfn1OpMCM (A.Mem p ty (A.Const _ (AT.CInt _ x)))                                     f = liftMCM [] $ f (OACMemOff p (translateTy ty) x)
tfn1OpMCM (A.Mem p ty (A.LName _ s))                                                 f = liftMCM [] $ f (OACCMemOff p (translateTy ty) s)
tfn1OpMCM (A.Mem p ty (A.BOp _ A.BAdd (A.TReg _ _ e0r) (A.Const _ (AT.CInt _ off)))) f = liftMCM [] $ f (OARMemOff p (translateTy ty) e0r off)
tfn1OpMCM (A.Mem p ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg _ _ e0r))) f = liftMCM [] $ f (OARMemOff p (translateTy ty) e0r off)
tfn1OpMCM (A.Mem p ty e0) f = do
    (e0is, _, e0r) <- translateExpr e0;
    liftMCM e0is $ f (OARMemOff p (translateTy ty) e0r 0)
tfn1OpMCM e f = do
    (eis, ty, er) <- translateExpr e;
    liftMCM eis $ f (OAReg (annotation e) ty er)

liftMCM :: Annotation a => [Inst a] -> Maybe (Inst a) -> State Int (Maybe [Inst a])
liftMCM pfx x = return $ either (Just . (pfx++) . return) (const Nothing) $ choice x

tfn2OpMC :: Annotation a => A.Expr a -> A.Expr a -> (OpArg a -> OpArg a -> Inst a) -> State Int [Inst a]
tfn2OpMC e0 e1 f = tfn2OpMCL e0 e1 (\x y -> [f x y])

tfn2OpMCL :: Annotation a => A.Expr a -> A.Expr a -> (OpArg a -> OpArg a -> [Inst a]) -> State Int [Inst a]
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.TReg p _ e0r) (A.Const _ (AT.CInt _ off)))) (A.Const p' x) f = do
    return $ f (OARMemOff p (translateTy ty) e0r off) (OAConst p' (translateConst x))
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg p _ e0r))) (A.Const p' x) f = do
    return $ f (OARMemOff p (translateTy ty) e0r off) (OAConst p' (translateConst x))
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.TReg p _ e0r) (A.Const _ (AT.CInt _ off)))) (A.LName p' s) f = do
    return $ f (OARMemOff p (translateTy ty) e0r off) (OACConst p' (translateTy ty) s)
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg p _ e0r))) (A.LName p' s) f = do
    return $ f (OARMemOff p (translateTy ty) e0r off) (OACConst p' (translateTy ty) s)
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.TReg p _ e0r) (A.Const _ (AT.CInt _ off)))) e1 f = do
    let ty' = translateTy ty;
    (e1is, _, e1r) <- translateExpr e1;
    return $ e1is ++ f (OARMemOff p ty' e0r off) (OAReg (annotation e1) ty' e1r)
tfn2OpMCL (A.Mem _ ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg p _ e0r))) e1 f = do
    let ty' = translateTy ty;
    (e1is, _, e1r) <- translateExpr e1;
    return $ e1is ++ f (OARMemOff p ty' e0r off) (OAReg (annotation e1) ty' e1r)
tfn2OpMCL (A.Mem p ty (A.Const _ (AT.CInt _ a))) (A.Const p' x) f = do
    return $ f (OACMemOff p (translateTy ty) a) (OAConst p' (translateConst x))
tfn2OpMCL (A.Mem p ty (A.Const _ (AT.CInt _ a))) (A.LName p' s) f = do
    let ty' = translateTy ty;
    return $ f (OACMemOff p ty' a) (OACConst p' ty' s)
tfn2OpMCL (A.Mem p ty (A.LName _ s)) (A.Const p' x) f = do
    return $ f (OACCMemOff p (translateTy ty) s) (OAConst p' (translateConst x))
tfn2OpMCL (A.Mem p ty (A.LName _ s)) (A.LName p' s') f = do
    let ty' = translateTy ty;
    return $ f (OACCMemOff p ty' s) (OACConst p' ty' s')
tfn2OpMCL (A.Mem p ty (A.LName _ s)) e0 f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OACCMemOff p ty' s) (OAReg (annotation e0) ty' e0r)
tfn2OpMCL (A.Mem p ty e0) (A.Const p' x) f = do
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OARMemOff p (translateTy ty) e0r 0) (OAConst p' (translateConst x))
tfn2OpMCL (A.Mem p ty e0) (A.LName p' s) f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OARMemOff p ty' e0r 0) (OACConst p' ty' s)
tfn2OpMCL (A.Mem p ty e0) e1 f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    (e1is, _, e1r) <- translateExpr e1;
    return $ e0is ++ e1is ++ f (OARMemOff p ty' e0r 0) (OAReg (annotation e1) ty' e1r)
    
tfn2OpMCL e0 (A.Mem _ ty (A.BOp _ A.BAdd (A.TReg p _ e1r) (A.Const _ (AT.CInt _ off)))) f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OAReg (annotation e0) ty' e0r) (OARMemOff p ty' e1r off)
tfn2OpMCL e0 (A.Mem _ ty (A.BOp _ A.BAdd (A.Const _ (AT.CInt _ off)) (A.TReg p _ e1r))) f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OAReg (annotation e0) ty' e0r) (OARMemOff p ty' e1r off)
tfn2OpMCL e0 (A.Mem p ty (A.LName _ s)) f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    return $ e0is ++ f (OAReg (annotation e0) ty' e0r) (OACCMemOff p ty' s)
tfn2OpMCL e0 (A.Mem p ty e1) f = do
    let ty' = translateTy ty;
    (e0is, _, e0r) <- translateExpr e0;
    (e1is, _, e1r) <- translateExpr e1;
    return $ e0is ++ e1is ++ f (OAReg (annotation e0) ty' e0r) (OARMemOff p ty' e1r 0)
tfn2OpMCL e0 e1 f = do
    (e0is, ty,  e0r) <- translateExpr e0;
    (e1is, ty', e1r) <- translateExpr e1;
    return $ e0is ++ e1is ++ f (OAReg (annotation e0) ty e0r) (OAReg (annotation e1) ty' e1r)
