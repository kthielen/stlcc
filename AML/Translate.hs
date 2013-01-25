
module AML.Translate where
import AML.Term
import AML.Type
import AML.Variant
import STLC.Term
import qualified STLC.Type as ST

import Util.Annotated
import Util.String
import Util.Sequence
import Util.Tuples
import Util.State

import Control.Monad.State

{-
    Translate STLC terms to abstract machine statements
-}
functionMachine :: (Annotation a, Eq a, ST.TypeEnv tenv a) => tenv -> Definition a -> State Int [Stmt a]
functionMachine tenv (fname, argnames, fty, e) = do
    let fnpfx  = [Label (annotation e) ("_" ++ fname)];
    let argpfx = [Arg (annotation e) (machineTy ty) ("_" ++ vn) | (vn, ty) <- reverse (zip argnames (ST.fnArgTys fty))];
    let tenv'  = ST.pushtys tenv (zip argnames (ST.fnArgTys fty));
    s <- absMachine tenv' (Return (annotation e)) e;
    let fncode = sseq (fnpfx ++ argpfx ++ [s]);
    s' <- unseqStmt fncode;
    return (unjunkStmts (flattenStmt s'));

functionExpr :: Annotation a => Expr a -> State Int [Stmt a]
functionExpr e = do
    s' <- unseqStmt (Return (annotation e) e);
    return (unjunkStmts (flattenStmt s'))

absMachine :: (Annotation a, Eq a, ST.TypeEnv tenv a) => tenv -> (Expr a -> Stmt a) -> Term a -> State Int (Stmt a)
absMachine tenv tf (Prim a x) = return (tf (Const a (liftSTC x)))
absMachine tenv tf (Var a x)  = do
    let xty   = machineTy (typeof tenv (Var a x));
    let xname = "_" ++ x;
    return $ tf $ case (ST.isTopLevelVar tenv x, xty) of
        (True, TFn _ _ _ _) -> LName a xname             -- primitive functions should resolve to their address
        (True, _)           -> Mem a xty (LName a xname) -- global variables are memory locations
        _                   -> TReg a xty xname          -- otherwise, a variable is stored in a register
absMachine tenv tf (Roll _ _ e) = absMachine tenv tf e
absMachine tenv tf (Unroll _ _ e) = absMachine tenv tf e
absMachine tenv tf (App a f as) = do
    let rty = machineTy (typeof tenv (App a f as));
    fe <- absMachineToExp tenv f;
    aes <- mapM (absMachineToExp tenv) as;
    return $ tf (Call a rty fe (reverse aes))
absMachine tenv tf (Let a x r b) = do
    let rty  = typeof tenv r;
    let rty' = machineTy rty;
    setr <- absMachine tenv (Move a (TReg a rty' ("_" ++ x))) r;
    b'   <- absMachine (ST.pushty tenv x rty) tf b;
    return (SSeq a setr b')
absMachine tenv tf (Variant a lbl e cs) = do
    let ety  = typeof tenv e;
    let ety' = machineTy ety;
    let cs'  = machineTyFields cs;
    e' <- absMachineToExp tenv e;
    t  <- fresh ((TReg a (machineTy (ST.TVariant a cs))) . prefix "t");
    let alloc  = variantAlloc t (TVariant a cs');
    let settag = variantSetTag t (TVariant a cs') (vindex cs' lbl);
    let setval = variantSetData t (TVariant a cs') e' ety';
    return $ SSeq a (sseq [alloc, settag, setval]) (tf t)
absMachine tenv tf (VCase a e cs) = do
    translateVCase tenv e cs absMachineToExp (\tenv' b -> absMachine tenv' tf b)
absMachine tenv tf (Record a fs) = do
    let rty  = typeof tenv (Record a fs);
    let rty' = machineTy rty;
    fes' <- mapM (absMachineToExp tenv) (map second fs);
    let fs' = zip (map first fs) fes';
    rg <- fresh ((TReg a rty') . prefix "r");
    let init = Move a rg (Malloc a Heap (liftSTT rty) 1);
    let fseq (offset, ss) e = (offset + sizeof (exprTy e), (Move a (Mem a (exprTy e) (BOp a BAdd rg (Const a (CInt a offset)))) e):ss);
    let (_, setf) = foldl fseq (0, []) fes';
    return (tf (ESeq a (sseq $ [init] ++ reverse setf) rg));
absMachine tenv tf (RProj a e lbl) = do
    let rty = typeof tenv e;
    e' <- absMachineToExp tenv e;
    return $ tf $ Mem a (machineTy (ST.rtype rty lbl)) (BOp a BAdd e' (Const a (CInt a (recordOffset [(lbl', machineTy ty) | (lbl', ty) <- ST.rfields rty] lbl))))
absMachine tenv tf (Pack _ e ty) = absMachine tenv tf e
absMachine tenv tf (Unpack a e v tn b) = do
    let ety = typeof tenv e;
    let uty = ST.exname ety tn;
    sete <- absMachine tenv (Move a (TReg a (machineTy uty) ("_" ++ v))) e;
    b'   <- absMachine (ST.pushty tenv v uty) tf b;
    return (SSeq a sete b')
absMachine tenv tf (Array a []) = do
    return $ tf $ Const a (CInt a 0);
absMachine tenv tf (Array a es) = do
    let ety   = typeof tenv (first es);
    let ety'  = machineTy ety;
    let szety = sizeof ety';
    es' <- mapM (absMachineToExp tenv) es;
    ag  <- fresh ((TReg a ety') . prefix "ae");
    let ains = mapi (\i e' -> Move a (Mem a ety' (BOp a BAdd ag (Const a $ CInt a $ i * szety))) e') es';
    return $ SSeq a (sseq $ (Move a ag (Malloc a Heap (liftSTT ety) (length es))):ains) (tf ag)
absMachine tenv tf (ArrAlloc a ty e) = do
    let ety  = typeof tenv e;
    let ety' = machineTy ety;
    e' <- absMachineToExp tenv e;
    rg <- fresh ((TReg a ety') . prefix "rc");
    let mv = Move a (Mem a ety' rg) e';
    return (SSeq a mv (tf rg))
absMachine tenv tf (ArrElem a ae ie) = do
    let aty = typeof tenv ae;
    let ety = arrTy (liftSTT aty);
    ae' <- absMachineToExp tenv ae;
    ie' <- absMachineToExp tenv ie;
    return $ tf $ Mem a ety (BOp a BAdd ae' (BOp a BMul ie' (Const a $ CInt a $ sizeof ety)))

absMachineToExp :: (Annotation a, Eq a, ST.TypeEnv tenv a) => tenv -> Term a -> State Int (Expr a)
absMachineToExp tenv e = do
    let ety = typeof tenv e;
    rg <- fresh ((TReg a (machineTy ety)) . prefix "te");
    s  <- absMachine tenv (Move a rg) e;
    return (selectResult s rg)
    where
        a = annotation e;
        selectResult (Move _ rg' e') rg | rg' == rg = e'
        selectResult s               rg             = ESeq a s rg
