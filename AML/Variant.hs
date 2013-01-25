
module AML.Variant where
import STLC.Term
import AML.Term
import AML.Type
import qualified STLC.Type as ST

import Util.Annotated
import Util.Num
import Util.String
import Util.Sequence
import Util.State

import qualified Data.List as List
import Control.Monad.State

{-
    Translate STLC variants to abstract machine statements
-}
variantAlloc :: Annotation a => Expr a -> Ty a -> Stmt a
variantAlloc t ty | sizeof ty > 4 = Move a t (Malloc a Heap ty 1) where a = annotation t
variantAlloc _ _                  = nullStmt

variantSetTag :: Annotation a => Expr a -> Ty a -> Int -> Stmt a
variantSetTag _ ty n | variantIsNullablePtr ty && n == variantNullablePtrID ty = nullStmt
variantSetTag t ty n | variantIsNullablePtr ty                                 = Move a t (Const a (CInt a n)) where a = annotation t
variantSetTag t ty n | sizeof ty == 1                                          = Move a t (Const a (CByte a (castn n))) where a = annotation t
variantSetTag t ty n | sizeof ty == 2 && sizeofVariantTag (vtags ty) == 2      = Move a t (Const a (CShort a n)) where a = annotation t
variantSetTag t ty n | sizeof ty == 2                                          = Move a t (BOp a BShiftL (Const a (CShort a n)) (Const a (CShort a 8))) where a = annotation t
variantSetTag t ty n | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 1      = Move a t (BOp a BShiftL (Const a (CInt a n)) (Const a (CInt a 24))) where a = annotation t
variantSetTag t ty n | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 2      = Move a t (BOp a BShiftL (Const a (CInt a n)) (Const a (CInt a 16))) where a = annotation t
variantSetTag t ty n | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 4      = Move a t (Const a (CInt a n)) where a = annotation t
variantSetTag t ty n | sizeofVariantTag (vtags ty) == 1                        = Move a (Mem a (TPrim a "byte")  t) (Const a (CByte a (castn n))) where a = annotation t
variantSetTag t ty n | sizeofVariantTag (vtags ty) == 2                        = Move a (Mem a (TPrim a "short") t) (Const a (CShort a n)) where a = annotation t
variantSetTag t ty n | sizeofVariantTag (vtags ty) == 4                        = Move a (Mem a (TPrim a "int")   t) (Const a (CInt a n)) where a = annotation t

variantSetData :: Annotation a => Expr a -> Ty a -> Expr a -> Ty a -> Stmt a
variantSetData _ _  _ (TPrim _ "unit") = nullStmt
variantSetData t ty e _ | variantIsNullablePtr ty = Move (annotation t) t e
variantSetData t ty e ety | sizeof ty == 2 = Move a t (BOp a BOr t e) where a = annotation t
variantSetData t ty e ety | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 1 = Move a t (BOp a BOr t e) where a = annotation t
variantSetData t ty e ety | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 2 = Move a t (BOp a BOr t e) where a = annotation t
variantSetData t ty e ety | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 4 = Move a t (BOp a BOr t e) where a = annotation t
variantSetData t ty e ety = Move a (Mem a ety (BOp a BAdd t (Const a $ CInt a $ sizeofVariantTag (vtags ty)))) e where a = annotation t

variantTagID :: Annotation a => Ty a -> Int -> Expr a
variantTagID ty n | variantIsNullablePtr ty          = Const a (CInt a n) where a = annotation ty
variantTagID ty n | sizeofVariantTag (vtags ty) == 1 = Const a (CByte a (castn n)) where a = annotation ty
variantTagID ty n | sizeofVariantTag (vtags ty) == 2 = Const a (CShort a n) where a = annotation ty
variantTagID ty n | sizeofVariantTag (vtags ty) == 4 = Const a (CInt a n) where a = annotation ty

variantGetTag :: Annotation a => Expr a -> Ty a -> Expr a -> Stmt a
variantGetTag t ty e | variantIsNullablePtr ty                            = Move a t e where a = annotation t
variantGetTag t ty e | sizeof ty == 1                                     = Move a t e where a = annotation t
variantGetTag t ty e | sizeof ty == 2 && sizeofVariantTag (vtags ty) == 2 = Move a t e where a = annotation t
variantGetTag t ty e | sizeof ty == 2                                     = Move a t (UOp a UHiByte e) where a = annotation t
variantGetTag t ty e | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 1 = Move a t (UOp a UHiByte e) where a = annotation t
variantGetTag t ty e | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 2 = Move a t (UOp a UHiShort e) where a = annotation t
variantGetTag t ty e | sizeof ty == 4 && sizeofVariantTag (vtags ty) == 4 = Move a t e where a = annotation t
variantGetTag t ty e | sizeofVariantTag (vtags ty) == 1                   = Move a t (Mem a (TPrim a "byte")  e) where a = annotation t
variantGetTag t ty e | sizeofVariantTag (vtags ty) == 2                   = Move a t (Mem a (TPrim a "short") e) where a = annotation t
variantGetTag t ty e | sizeofVariantTag (vtags ty) == 4                   = Move a t (Mem a (TPrim a "int")   e) where a = annotation t

variantGetData :: Annotation a => Expr a -> Ty a -> Expr a -> Ty a -> Stmt a
variantGetData _ (TPrim _ "unit") _ _                             = nullStmt
variantGetData t ty e ety | variantIsNullablePtr ety              = Move a t e where a = annotation t
variantGetData t ty e ety | sizeofVariantPayload (vtags ety) == 1 = Move a t (UOp a ULoByte e) where a = annotation t
variantGetData t ty e ety | sizeofVariantPayload (vtags ety) == 2 = Move a t (UOp a ULoShort e) where a = annotation t
variantGetData t ty e ety                                         = Move a t (Mem a ty (BOp a BAdd e (Const a (CInt a (sizeofVariantTag (vtags ety)))))) where a = annotation t

translateVCase :: (Annotation a, ST.TypeEnv tenv a) => tenv -> Term a -> [(String, String, Term a)] -> (tenv -> Term a -> State Int (Expr a)) -> (tenv -> Term a -> State Int (Stmt a)) -> State Int (Stmt a)
translateVCase tenv e cs ef bf | variantIsNullablePtr (liftSTT (typeof tenv e)) = do
    let a = annotation e;
    e' <- ef tenv e;
    ln <- fresh (prefix "vl");
    pr <- fresh ((TReg a (exprTy e')) . prefix "tv");
    tg <- fresh ((TReg a vtty) . prefix "tg");
    let dlbl = ln ++ "done";
    let vchead = SSeq a (Move a pr e') (variantGetTag tg vty pr);
    let cfn i (lbl, v, b) = do
        let xty   = ST.vtype svty lbl;
        let xty'  = machineTy xty;
        let x     = TReg a xty' ("_" ++ v);
        let tenv' = ST.pushty tenv v xty;
        setr <- bf tenv' b;
        let clbl = ln ++ "_"      ++ show i;
        let tlbl = ln ++ "_succ_" ++ show i;
        let nlbl = ln ++ "_"      ++ show (i+1);
        let cmp  = if variantNullablePtrLbl vty == lbl then nullStmt else CJump a REq tg (variantTagID vty $ vindex (vtags vty) lbl) tlbl nlbl;
        let init = variantGetData x xty' pr vty;
        return $ sseq [Label a clbl, cmp, Label a tlbl, init, setr, Jump a dlbl]
    cs' <- mapiM cfn (List.sortBy cmpCase cs);
    return (sseq $ [vchead] ++ cs' ++ [Label a dlbl])
    where
        svty  = typeof tenv e
        vty   = liftSTT svty
        vtty  = variantTagType vty
        vty' = machineTy svty
        cmpCase (lbl0, _, _) (_,    _, _) | isPtr (machineTy (ST.vtype svty lbl0)) = GT
        cmpCase (_,    _, _) (lbl1, _, _) | isPtr (machineTy (ST.vtype svty lbl1)) = LT
        cmpCase _            _                                                     = EQ
        isPtr (TPtr _ _) = True
        isPtr _          = False

translateVCase tenv e cs ef bf = do
    let a = annotation e;
    let vty = typeof tenv e;
    let vty' = liftSTT vty;
    e' <- ef tenv e;
    ln <- fresh (prefix "vl");
    pr <- fresh ((TReg a (exprTy e')) . prefix "tv");
    tg <- fresh ((TReg a (variantTagType vty')) . prefix "tg");
    let dlbl = ln ++ "done";
    let vchead = SSeq a (Move a pr e') (variantGetTag tg vty' pr);
    let vctail = sseq [Label a $ ln ++ "_" ++ show (length cs), Return a (Call a (TPrim a "unit") (LName a "failed_match") [])];
    let cfn i (lbl, v, b) = do
        let xty   = ST.vtype vty lbl;
        let xty'  = machineTy xty;
        let x     = TReg a xty' ("_" ++ v);
        let tenv' = ST.pushty tenv v xty;
        setr <- bf tenv' b;
        let clbl = ln ++ "_"      ++ show i;
        let tlbl = ln ++ "_succ_" ++ show i;
        let nlbl = ln ++ "_"      ++ show (i+1);
        let cmp  = CJump a REq tg (variantTagID vty' $ vindex (vtags vty') lbl) tlbl nlbl;
        let init = variantGetData x xty' pr vty';
        return $ sseq [Label a clbl, cmp, Label a tlbl, init, setr, Jump a dlbl]
    cs' <- mapiM cfn cs;
    return (sseq $ [vchead] ++ cs' ++ [vctail, Label a dlbl])
