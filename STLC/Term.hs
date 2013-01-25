
module STLC.Term where
import STLC.Type
import Util.Annotated
import Util.String
import Util.Sequence
import Util.Tuples

import Data.Set as Set hiding (map, filter)
import Data.List as List hiding (delete, union, (\\))
import qualified Data.Map as Map

{- simply-typed lambda calculus terms -}
data Term a = Prim     a (CPrim a)
            | Var      a String
            | Roll     a (Ty a) (Term a)
            | Unroll   a (Ty a) (Term a)
		    | App      a (Term a) [Term a]
		    | Let      a String (Term a) (Term a)
		    | Variant  a String (Term a) [(String, Ty a)]
		    | VCase    a (Term a) [(String, String, Term a)]
		    | Record   a [(String, Term a)]
		    | RProj    a (Term a) String
		    | Pack     a (Term a) (Ty a)
		    | Unpack   a (Term a) String String (Term a)
		    | Array    a [Term a]
		    | ArrAlloc a (Ty a) (Term a)
		    | ArrElem  a (Term a) (Term a)
            deriving (Eq, Ord)

type Definition a    = (String, [String], Ty a, Term a)
type Definitions a   = [Definition a]
type LabelledTys a   = [(String, Ty a)]
type LabelledTerms a = [(String, Term a)]
type ModDef a        = [(String, Term a)]

instance Annotated Term where
    annotation (Prim a _)         = a
    annotation (Var a _)          = a
    annotation (Roll a _ _)       = a
    annotation (Unroll a _ _)     = a
    annotation (App a _ _)        = a
    annotation (Let a _ _ _)      = a
    annotation (Variant a _ _ _)  = a
    annotation (VCase a _ _)      = a
    annotation (Record a _)       = a
    annotation (RProj a _ _)      = a
    annotation (Pack a _ _)       = a
    annotation (Unpack a _ _ _ _) = a
    annotation (Array a _)        = a
    annotation (ArrAlloc a _ _)   = a
    annotation (ArrElem a _ _)    = a

    annotations (Prim     a c)       = annotations c ++ [a]
    annotations (Var      a _)       = [a]
    annotations (Roll     a t b)     = annotations t ++ annotations b ++ [a]
    annotations (Unroll   a t b)     = annotations t ++ annotations b ++ [a]
    annotations (App      a f as)    = annotations f ++ concatMap annotations as ++ [a]
    annotations (Let      a _ e b)   = annotations e ++ annotations b ++ [a]
    annotations (Variant  a _ v _)   = annotations v ++ [a]
    annotations (VCase    a v cs)    = annotations v ++ concat [annotations e | (_, _, e) <- cs] ++ [a]
    annotations (Record   a cs)      = concat [annotations e | (_, e) <- cs] ++ [a]
    annotations (RProj    a r _)     = annotations r ++ [a]
    annotations (Pack     a e t)     = annotations e ++ annotations t ++ [a]
    annotations (Unpack   a e _ _ b) = annotations e ++ annotations b ++ [a]
    annotations (Array    a es)      = concatMap annotations es ++ [a]
    annotations (ArrAlloc a t e)     = annotations t ++ annotations e ++ [a]
    annotations (ArrElem  a e i)     = annotations e ++ annotations i ++ [a]

instance Show (Term a) where
    show (Prim _ x)                     = show x
    show (Var _ x)                      = x
    show (Roll _ ty x)                  = "roll [" ++ show ty ++ "] " ++ show x
    show (Unroll _ ty x)                = "unroll [" ++ show ty ++ "] " ++ show x
    show (App _ (Var _ f) as)           = f ++ "(" ++ cdelim (map show as) "," ++ ")"
    show (App _ (RProj _ (Var _ r) lbl) as) = r ++ "." ++ lbl ++ "(" ++ cdelim (map show as) "," ++ ")"
    show (App _ f as)                   = "(" ++ show f ++ "(" ++ cdelim (map show as) "," ++ "))"
    show (Let _ x r b)                  = "let " ++ x ++ " = " ++ show r ++ " in " ++ show b
    show (Variant a t v ty)             = "(" ++ t ++ "=" ++ show v ++ " as " ++ show (TVariant a ty) ++ ")"
    show (VCase _ x cs)                 = "case " ++ show x ++ " " ++ cdelim (map (\(t,v,b) -> "<" ++ t ++ "=" ++ v ++ ">: " ++ show b) cs) "| "
    show (Record _ fs)                  = "{" ++ showFields "=" fs ++ "}"
    show (RProj _ e f)                  = show e ++ "." ++ f
    show (Pack _ e ty)                  = "pack " ++ show e ++ " as " ++ show ty
    show (Unpack _ e v t b)             = "unpack " ++ show e ++ " as {" ++ v ++ "," ++ t ++ "} in " ++ show b
    show (Array _ es)                   = "[" ++ cdelim (map show es) "; " ++ "]"
    show (ArrAlloc _ ty e)              = "new " ++ show ty ++ "[" ++ show e ++ "]"
    show (ArrElem _ a i)                = show a ++ "[" ++ show i ++ "]"

showDef :: Definition a -> String
showDef (n, [], ty, e) = n ++ " :: " ++ show ty ++ " = " ++ show e ++ ";"
showDef (n, as, ty, e) = n ++ " :: " ++ show ty ++ "\n" ++ n ++ concat [" " ++ an | an <- as] ++ " = " ++ show e ++ ";"

defName :: Definition a -> String
defName (n, _, _, _) = n

fv :: Term a -> Set.Set String
fv (Prim _ _)         = empty
fv (Var _ x)          = singleton x
fv (Roll _ _ e)       = fv e
fv (Unroll _ _ e)     = fv e
fv (App _ f as)       = unions $ map fv (f:as)
fv (Let _ x r b)      = fv r `union` (delete x $ fv b)
fv (Variant _ _ e _)  = fv e
fv (VCase _ x cs)     = fv x `union` (unions cvs \\ Set.fromList vvs) where
    (vvs, cvs) = (map second cs, map (fv . third) cs)
fv (Record _ fs)      = unions $ map (fv . second) fs
fv (RProj _ e _)      = fv e
fv (Pack _ e _)       = fv e
fv (Unpack _ e v _ b)  = fv e `union` (delete v $ fv b)
fv (Array _ es)       = unions $ map fv es
fv (ArrAlloc _ _ e)   = fv e
fv (ArrElem _ a i)    = fv a `union` fv i

typeof :: (Annotation a, TypeEnv tenv a) => tenv -> Term a -> Ty a
typeof tenv (Prim a x)          = primTy x
typeof tenv (Var a x)           = suj (tydef tenv x) (describe a ++ ": Undefined variable, " ++ x)
typeof tenv (Roll a ty e)       = rollTy ty (typeof tenv e)
typeof tenv (Unroll a ty e)     = unrollTy ty (typeof tenv e)
typeof tenv (App a f as)        = fnRTy (typeof tenv f)
typeof tenv (Let a x r b)       = typeof (pushty tenv x (typeof tenv r)) b
typeof tenv (Variant a _ _ ty)  = TVariant a ty
typeof tenv (VCase a e cs)      = let (lbl, v, b) = first cs in typeof (pushty tenv v (vtype (typeof tenv e) lbl)) b
typeof tenv (Record a fs)       = TRecord a (map (\(f, e) -> (f, typeof tenv e)) fs)
typeof tenv (RProj a e f)       = rtype (typeof tenv e) f
typeof tenv (Pack _ _ ty)       = ty
typeof tenv (Unpack a e v tv b) = typeof (pushty tenv v (exname (typeof tenv e) tv)) b
typeof tenv (Array a (v:vs))    = TArray a (typeof tenv v)
typeof tenv (ArrAlloc a ty _)   = TArray a ty
typeof tenv (ArrElem a e i)     = arrTy (typeof tenv e)

replaceVar :: String -> Term a -> Term a -> Term a
replaceVar x (Var _ x') e | x == x' = e

replaceVar _ _ (Prim a x) = Prim a x
replaceVar x e (Var _ y) | x == y = e
replaceVar _ _ (Var a y)          = Var a y
replaceVar x e (Roll a ty e')     = Roll a ty (replaceVar x e e')
replaceVar x e (Unroll a ty e')   = Unroll a ty (replaceVar x e e')
replaceVar x e (App a f as) = App a (replaceVar x e f) (map (replaceVar x e) as)
replaceVar x e (Let a x' e' b) | x == x' = Let a x' (replaceVar x e e') b
replaceVar x e (Let a x' e' b)           = Let a x' (replaceVar x e e') (replaceVar x e b)
replaceVar x e (Variant a c b cs) = Variant a c (replaceVar x e b) cs
replaceVar x e (VCase a v cs) = VCase a (replaceVar x e v) (map replaceVCase cs) where
    replaceVCase (lbl, x', b) | x == x' = (lbl, x', b)
    replaceVCase (lbl, x', b)           = (lbl, x', replaceVar x e b)
replaceVar x e (Record a defs) = Record a [(f, replaceVar x e v) | (f, v) <- defs]
replaceVar x e (RProj a r lbl) = RProj a (replaceVar x e r) lbl
replaceVar x e (Pack a v ty)   = Pack a (replaceVar x e v) ty
replaceVar x e (Unpack a v vn ty b) | x == vn = Unpack a (replaceVar x e v) vn ty b
replaceVar x e (Unpack a v vn ty b)           = Unpack a (replaceVar x e v) vn ty (replaceVar x e b)
replaceVar x e (Array a es)                   = Array a [replaceVar x e e' | e' <- es]
replaceVar x e (ArrAlloc a ty e')             = ArrAlloc a ty (replaceVar x e e')
replaceVar x e (ArrElem a ae i)               = ArrElem a (replaceVar x e ae) (replaceVar x e i)

expandTermTy :: [(String, Ty a)] -> Term a -> Term a
expandTermTy subst (Prim a x)          = Prim a x
expandTermTy subst (Var a x)           = Var a x
expandTermTy subst (Roll a ty e)       = Roll a (expandTy subst ty) (expandTermTy subst e)
expandTermTy subst (Unroll a ty e)     = Unroll a (expandTy subst ty) (expandTermTy subst e)
expandTermTy subst (App a f as)        = App a (expandTermTy subst f) (map (expandTermTy subst) as)
expandTermTy subst (Let a v e b)       = Let a v (expandTermTy subst e) (expandTermTy subst b)
expandTermTy subst (Variant a v e cs)  = Variant a v (expandTermTy subst e) [(v, expandTy subst ty) | (v, ty) <- cs]
expandTermTy subst (VCase a e cs)      = VCase a (expandTermTy subst e) [(lbl, v, expandTermTy subst e') | (lbl, v, e') <- cs]
expandTermTy subst (Record a fs)       = Record a [(v, expandTermTy subst e) | (v, e) <- fs]
expandTermTy subst (RProj a e lbl)     = RProj a (expandTermTy subst e) lbl
expandTermTy subst (Pack a e ty)       = Pack a (expandTermTy subst e) (expandTy subst ty)
expandTermTy subst (Unpack a e v vn b) = Unpack a (expandTermTy subst e) v vn (expandTermTy subst b)
expandTermTy subst (Array a es)        = Array a (map (expandTermTy subst) es)
expandTermTy subst (ArrAlloc a ty e)   = ArrAlloc a (expandTy subst ty) (expandTermTy subst e)
expandTermTy subst (ArrElem a e i)     = ArrElem a (expandTermTy subst e) (expandTermTy subst i)

mapMTerm :: Monad m => (Term a -> Bool) -> (Term a -> m (Term a)) -> Term a -> m (Term a)
mapMTerm exitfn tfn e | exitfn e = tfn e
mapMTerm exitfn tfn (Prim a x) = return (Prim a x)
mapMTerm exitfn tfn (Var a x) = return (Var a x)
mapMTerm exitfn tfn (Roll a ty e) = do
    e' <- mapMTerm exitfn tfn e;
    return (Roll a ty e')
mapMTerm exitfn tfn (Unroll a ty e) = do
    e' <- mapMTerm exitfn tfn e;
    return (Unroll a ty e')
mapMTerm exitfn tfn (App a f as) = do
    f'  <- mapMTerm exitfn tfn f;
    as' <- mapM (mapMTerm exitfn tfn) as;
    return (App a f' as')
mapMTerm exitfn tfn (Let a x e b) = do
    e' <- mapMTerm exitfn tfn e;
    b' <- mapMTerm exitfn tfn b;
    return (Let a x e' b')
mapMTerm exitfn tfn (Variant a lbl e cs) = do
    e' <- mapMTerm exitfn tfn e;
    return (Variant a lbl e' cs)
mapMTerm exitfn tfn (VCase a e cs) = do
    e'  <- mapMTerm exitfn tfn e;
    cs' <- mapM (\(lbl,x,e) -> do { e' <- mapMTerm exitfn tfn e; return (lbl,x,e') }) cs;
    return (VCase a e' cs')
mapMTerm exitfn tfn (Record a fs) = do
    fs' <- mapM (\(lbl,e) -> do { e' <- mapMTerm exitfn tfn e; return (lbl,e') }) fs;
    return (Record a fs')
mapMTerm exitfn tfn (RProj a e lbl) = do
    e' <- mapMTerm exitfn tfn e;
    return (RProj a e' lbl)
mapMTerm exitfn tfn (Pack a e ty) = do
    e' <- mapMTerm exitfn tfn e;
    return (Pack a e' ty)
mapMTerm exitfn tfn (Unpack a e v vn b) = do
    e' <- mapMTerm exitfn tfn e;
    b' <- mapMTerm exitfn tfn b;
    return (Unpack a e' v vn b')
mapMTerm exitfn tfn (Array a es) = do
    es' <- mapM (mapMTerm exitfn tfn) es;
    return (Array a es')
mapMTerm exitfn tfn (ArrAlloc a ty e) = do
    e' <- mapMTerm exitfn tfn e;
    return (ArrAlloc a ty e')
mapMTerm exitfn tfn (ArrElem a ae i) = do
    ae' <- mapMTerm exitfn tfn ae;
    i'  <- mapMTerm exitfn tfn i;
    return (ArrElem a ae' i')

-- extend a type environment by term
--  (the term should be meaningful in the input type environment)
pushtermty :: (Annotation a, TypeEnv tenv a) => tenv -> String -> Term a -> tenv
pushtermty tenv v e = pushty tenv v (typeof tenv e)

-- determine the dependency graph between term definitions
type DefinitionDependencies = Map.Map String [String]
definitionDependencies :: Definitions a -> DefinitionDependencies
definitionDependencies defs = Map.fromList (map defDeps defs) where
    defDeps (vn, vs, _, e) = (vn, Set.toList (Set.difference (fv e) (Set.fromList vs)))
    vnames = [vn | (vn, _, _, _) <- defs]
    isTLDef vn = vn `elem` vnames

definitionDependencyGraph :: String -> String -> DefinitionDependencies -> String
definitionDependencyGraph name label defs =
    "   subgraph cluster_" ++ name ++ " {\n" ++
    "      label = \"" ++ label ++ "\";\n" ++
    concatMap node nodeNames ++
    concatMap edges defs' ++
    "   }\n"
    where
        defs'     = Map.toList defs
        nodeNames = [vn | (vn, _) <- defs']
        node vn = "     \"" ++ nodeName vn ++ "\" [label=\"" ++ nodeLabel vn ++ "\"];\n"
        edges (vn, vs) = concat [edge vn vn' | vn' <- vs, vn' `elem` nodeNames]
        edge vn vn' = "     \"" ++ nodeName vn ++ "\" -> \"" ++ nodeName vn' ++ "\";\n"
        
        nodeName vn = name ++ "_" ++ vn
        nodeLabel = id
