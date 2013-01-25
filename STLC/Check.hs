
module STLC.Check where
import STLC.Term
import STLC.Type
import Util.Annotated

checkDefinitions :: (Annotation a, TypeEnv tenv a) => tenv -> Definitions a -> b -> IO b
checkDefinitions tenv defs result = chooseErr (concatMap (checkDefinition tenv) defs) where
    chooseErr [] = return result
    chooseErr msgs = do
        printMsgs msgs
        error "Compile failure, cannot continue."
    printMsgs [] = return ()
    printMsgs (m:msgs) = do
        putStrLn m;
        printMsgs msgs

checkDefinition :: (Annotation a, TypeEnv tenv a) => tenv -> Definition a -> [String]
checkDefinition tenv (_, [],  ty,  e) = check tenv e ++ checkEqTy tenv e ty
checkDefinition tenv (_, ans, fty, e) = check tenv' e ++ checkEqTy tenv' e (fnRTy fty) where
    tenv' = pushtys tenv (zip ans (fnArgTys fty))

check :: (Annotation a, TypeEnv tenv a) => tenv -> Term a -> [String]
check tenv (Prim     _ _)          = []
check tenv (Var      _ _)          = []
check tenv (Roll     p t e)        = check tenv e ++ checkRoll p tenv t e
check tenv (Unroll   p t e)        = check tenv e ++ checkUnroll p tenv t e
check tenv (Let      _ x e b)      = check tenv e ++ check (pushty tenv x (typeof tenv e)) b
check tenv (Variant  _ _ e _)      = check tenv e
check tenv (VCase    _ e cs)       = check tenv e ++ concat [check (pushty tenv v (vtype vty ctor)) b | (ctor, v, b) <- cs] where vty = typeof tenv e
check tenv (Record   _ cs)         = concat [check tenv e | (_, e) <- cs]
check tenv (RProj    _ e lbl)      = check tenv e
check tenv (Pack     _ e _)        = check tenv e
check tenv (Array    _ es)         = concatMap (check tenv) es ++ checkEqTys tenv es
check tenv (ArrAlloc _ _ e)        = check tenv e ++ checkEqTy tenv e (TPrim nullAnnotation "int")
check tenv (ArrElem  _ ae ie)      = check tenv ae ++ check tenv ie ++ checkArrayTy tenv ae ++ checkEqTy tenv ie (TPrim nullAnnotation "int")
check tenv (App      p fe aes)     = check tenv fe ++ concatMap (check tenv) aes ++ checkFnApp p tenv fe aes
check tenv (Unpack   p pe v vtn b) = check tenv pe ++ check (pushty tenv v (exname (typeof tenv pe) vtn)) b ++ checkUnpack p tenv pe v vtn b

checkRoll :: (Annotation a, TypeEnv tenv a) => a -> tenv -> Ty a -> Term a -> [String]
checkRoll p tenv t e = []

checkUnroll :: (Annotation a, TypeEnv tenv a) => a -> tenv -> Ty a -> Term a -> [String]
checkUnroll p tenv t e = []

checkEqTys :: (Annotation a, TypeEnv tenv a) => tenv -> [Term a] -> [String]
checkEqTys _    []        = []
checkEqTys _    [_]       = []
checkEqTys tenv (e:e':es) = checkEqTy tenv e' (typeof tenv e) ++ checkEqTys tenv (e':es)

checkEqTy :: (Annotation a, TypeEnv tenv a) => tenv -> Term a -> Ty a -> [String]
checkEqTy tenv e ty = checkExprTyP tenv e ((==) ty) ("Expected type " ++ show ty)

checkArrayTy :: (Annotation a, TypeEnv tenv a) => tenv -> Term a -> [String]
checkArrayTy tenv e = checkExprTyP tenv e isArrayTy "Expected array type"

checkFnApp :: (Annotation a, TypeEnv tenv a) => a -> tenv -> Term a -> [Term a] -> [String]
checkFnApp p tenv fe aes = chooseErrPath (isFNTy fty) (length aes == length argTys) where
    fty    = typeof tenv fe
    argTys = fnArgTys fty
    chooseErrPath False _     = ["Expected function type\n   actual type: " ++ show fty ++ "\n   at: " ++ describe (annotation fe)]
    chooseErrPath True  False = ["Argument length mismatch\n   expected " ++ show (length argTys) ++ " arguments but received " ++ show (length aes)]
    chooseErrPath True  True  = concat [checkEqTy tenv e ety | (e, ety) <- zip aes argTys]

checkUnpack :: (Annotation a, TypeEnv tenv a) => a -> tenv -> Term a -> String -> String -> Term a -> [String]
checkUnpack p tenv pe v vtn b = chooseErrPath (isExType pety) (not (elem (TVar p vtn) (map (TVar p) (tyFV bty)))) where
    pety = typeof tenv pe
    bty = typeof (pushty tenv v (exname (typeof tenv pe) vtn)) b
    chooseErrPath False _     = ["Expected existential type to unpack\n   actual type: " ++ show pety ++ "\n   at: " ++ describe (annotation pe)]
    chooseErrPath True  False = ["Existential type variable cannot escape the scope of an unpack\n   with type: " ++ show bty ++ "\n   at: " ++ describe (annotation b)]
    chooseErrPath True  True  = []

checkExprTyP :: (Annotation a, TypeEnv tenv a) => tenv -> Term a -> (Ty a -> Bool) -> String -> [String]
checkExprTyP tenv e p msg | not (p ety) = [errMsg] where
    ety    = typeof tenv e
    errMsg = msg ++ "\n   actual type: " ++ show ety ++ "\n   at: " ++ describe (annotation e)
checkExprTyP _ _ _ _ = []
