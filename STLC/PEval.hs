
module STLC.PEval where
import STLC.Term
import STLC.Type
import Util.Annotated
import Util.Sequence
import qualified Data.Map as Map
import Control.Monad.State

type VRef  = String
type Label = String

data Proj = PConst (CPrim ())
          | PVariant [(Label, Proj)]
          | PRecord [(Label, Proj)]
          | PBottom
          deriving (Eq, Ord, Show)

-- a specialization environment
type ProjEnv = Map.Map VRef Proj
type SpecEnv tenv a = (tenv, ProjEnv)

stypeof :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> Term a -> Ty a
stypeof (tenv, _) e = typeof tenv e

spushtys :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> [(String, Ty a)] -> [(String, Proj)] -> SpecEnv tenv a
spushtys (tenv, penv) tds pds = (pushtys tenv tds, insertBatch penv pds)

sproj :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> VRef -> Proj
sproj (_, penv) v = meither id (const PBottom) (lookup v penv)

-- the name of a definition and the static projection of its arguments
type Specialization = (VRef, [Proj])

-- the specialized definition name and the static projection of its result
type SpecResult = (VRef, Proj)

-- within this monad:
--   generate fresh names
--   map known specializations to their chosen names
--   keep track of which specializations still need to be realized
type NamedSpecs = Map.Map Specialization SpecResult
type PEM a b = State (Int, NamedSpecs, Definitions a) b

specializeDefs :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> Definitions a -> Definitions a
specializeDefs senv defs = select (runState (mapM initSpecDef defs) (0, Map.empty, defs)) where
    select (dps, (_, _, defs')) = (map fst dps) ++ defs'
    initSpecDef d@(_, argl, _, _) = specializeDef senv (map (const PBottom) argl) d

specializeDef :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> [Proj] -> Definition a -> PEM a (Definition a, Proj)
specializeDef senv [] (vn, [], _, e) = do
    (e', p) <- specializeExp senv e
    return ((vn, [], stypeof senv e', e'), p)
specializeDef senv ps (vn, ans, fty, e) | length ps == length (fnArgTys fty) = do
    let senv' = spushtys senv (zip ans (fnArgTys fty)) (zip ans ps);
    (e', p) <- specializeExp senv' e;
    let fty' = stypeof senv' e';
    return ((vn, ans, fty', e'), p)
specializeDef tenv ps d = error ("Incoherent specialization request: " ++ show d ++ " / " ++ show ps)

specializeExp :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> Term a -> PEM a (Term a, Proj)
specializeExp senv (Prim     p c)         = return (Prim p c, PConst () c)
specializeExp senv (Var      p vn)        = return (Var p vn, sproj senv vn)
specializeExp senv (Roll     p ty e)      = undefined
specializeExp senv (Unroll   p ty e)      = undefined
specializeExp senv (App      p f es)      = undefined
specializeExp senv (Let      p vn e b)    = do
    (e', ep) <- specializeExp senv e;
    (b', bp) <- specializeExp (spushtys senv [(vn, stypeof senv e'] [(vn, ep)]) b;
    return (Let p vn e' b', bp)
specializeExp senv (Variant  p cn e cts)  = do
    (e', ep) <- specializeExp senv e;
    return (Variant p cn e' cts, PVariant [(cn, ep)])
specializeExp senv (VCase    p e cs)      = undefined
specializeExp senv (Record   p fs)        = do
    (fs', fsp) <- specFields fs;
    return (Record p fs', PRecord fsp)
    where
        specFields fs =
            fsps <- mapM specField fs;
            return (map (\(lbl,e,_) -> (lbl, e)) fsps, map (\(lbl,_,ep) -> (lbl, ep)) fsps)
        specField (lbl, e) = do
            (e', ep) <- specializeExp senv e;
            return (lbl, e', ep)
specializeExp senv (RProj    p e lbl)     = undefined
specializeExp senv (Pack     p e ty)      = undefined
specializeExp senv (Unpack   p e vn tn b) = undefined
specializeExp senv (Array    p es)        = undefined
specializeExp senv (ArrAlloc p ty ie)     = undefined
specializeExp senv (ArrElem  p ae ie)     = undefined

specDef :: (Annotation a, TypeEnv tenv a) => SpecEnv tenv a -> Specialization -> PEM a SpecResult
specDef senv s@(dn, ps) = do
    meitherM (lookupSpec s) return $
        do def <- namedDef dn;
           (def', p) <- specializeDef senv ps def
           let r = (defName def', p);
           putSpec s r;
           putSpecDef def';
           return r

fresh :: (Int -> b) -> PEM a b
fresh f = do
    (id, m, ss) <- get;
    put (id+1, m, ss);
    return (f id)

lookupSpec :: Specialization -> PEM a (Maybe SpecResult)
lookupSpec s = do
    (_, m, _) <- get;
    return (Map.lookup s m)

putSpec :: Specialization -> SpecResult -> PEM a ()
putSpec s sr = do
    (id, m, ds) <- get;
    put (id, Map.insert s sr m, ds)

putSpecDef :: Definition a -> PEM a ()
putSpecDef d = do
    (id, m, ds) <- get;
    put (id, m, d:ds)

namedDef :: VRef -> PEM a (Definition a)
namedDef vn = do
    (_, _, ds) <- get;
    return (select [d | d@(vn', _, _, _) <- ds, vn' == vn]) where
        select [x] = x
        select _   = error ("Missing or ambiguous definition found during specialization: " ++ vn)
