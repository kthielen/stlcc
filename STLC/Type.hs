{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module STLC.Type where
import Util.Annotated
import Util.Sequence
import Util.Tuples
import Util.String
import Util.Num
import Data.Set as Set hiding (map, filter, (\\))
import Data.List as List hiding (delete, union)
import GHC.Word
import Data.Maybe

{- Types -}
data Ty a = TPrim    a String
          | TVar     a String
          | TFn      a CallConv [Ty a] (Ty a)
          | TVariant a [(String, Ty a)]
          | TRecord  a [(String, Ty a)]
          | TExists  a String (Ty a)
          | TArray   a (Ty a)
          | TMu      a String (Ty a)
          deriving (Ord)

instance Annotated Ty where
    annotation (TPrim    x _)     = x
    annotation (TVar     x _)     = x
    annotation (TFn      x _ _ _) = x
    annotation (TVariant x _)     = x
    annotation (TRecord  x _)     = x
    annotation (TExists  x _ _)   = x
    annotation (TArray   x _)     = x
    annotation (TMu      x _ _)   = x
    
    annotations (TPrim    a _)      = [a]
    annotations (TVar     a _)      = [a]
    annotations (TFn      a _ as r) = concatMap annotations as ++ annotations r ++ [a]
    annotations (TVariant a cs)     = concat [annotations t | (_, t) <- cs] ++ [a]
    annotations (TRecord  a cs)     = concat [annotations t | (_, t) <- cs] ++ [a]
    annotations (TExists  a _ t)    = annotations t ++ [a]
    annotations (TArray   a t)      = annotations t ++ [a]
    annotations (TMu      a _ t)    = annotations t ++ [a]

data CallConv = CFn deriving (Eq, Ord, Show)

data CPrim a = CByte a Word8
             | CChar a Char
             | CShort a Int
             | CInt a Int
             | CFloat a Float
             | CString a String
		     | CUnit a
             deriving (Eq, Ord)

instance Functor Ty where
    fmap f (TPrim x c0)        = TPrim (f x) c0
    fmap f (TVar x c0)         = TVar (f x) c0
    fmap f (TFn x cc args rty) = TFn (f x) cc (map (fmap f) args) (fmap f rty)
    fmap f (TVariant x defs)   = TVariant (f x) [(lbl, fmap f ty) | (lbl, ty) <- defs]
    fmap f (TRecord  x defs)   = TRecord (f x) [(lbl, fmap f ty) | (lbl, ty) <- defs]
    fmap f (TExists x vn ety)  = TExists (f x) vn (fmap f ety)
    fmap f (TArray x aty)      = TArray (f x) (fmap f aty)
    fmap f (TMu x vn rty)      = TMu (f x) vn (fmap f rty)

instance Eq (Ty a) where
    (TPrim _ x)         == (TPrim _ x')                 = x == x'
    (TVar _ x)          == (TVar _ x')                  = x == x'
    (TFn _ cc atys rty) == (TFn _ cc' atys' rty')       = cc == cc' && atys == atys' && rty == rty'
    (TVariant _ fs)     == (TVariant _ fs')             = fs == fs'
    (TRecord _ fs)      == (TRecord _ fs')              = fs == fs'
    (TExists _ x ty)    == (TExists _ x' ty') | x == x' = ty == ty'
    (TExists _ x ty)    == (TExists _ x' ty')           = tyRename ty x x' == ty' && ty == tyRename ty' x' x
    (TArray _ ty)       == (TArray _ ty')               = ty == ty'
    (TMu _ x ty)        == (TMu _ x' ty') | x == x'     = ty == ty'
    (TMu _ x ty)        == (TMu _ x' ty')               = tyRename ty x x' == ty' && ty == tyRename ty' x' x
    _                   == _                            = False

instance Show (Ty a) where
    show (TPrim _ s)      = s
    show (TVar _ s)       = s
    show (TFn _ cc as r)  = "(" ++ cdelim (map show as) ", " ++ ") -> " ++ show r
    show (TVariant _ fs)  = "<" ++ showFields ":" fs ++ ">"
    show (TRecord _ fs)   = "{" ++ showFields ":" fs ++ "}"
    show (TExists _ v ty) = "exists " ++ v ++ "." ++ show ty
    show (TArray _ e)     = "[" ++ show e ++ "]"
    show (TMu _ x ty)     = "mu " ++ x ++ "." ++ show ty

instance Functor CPrim where
    fmap f (CByte   x c0) = CByte   (f x) c0
    fmap f (CChar   x c0) = CChar   (f x) c0
    fmap f (CShort  x c0) = CShort  (f x) c0
    fmap f (CInt    x c0) = CInt    (f x) c0
    fmap f (CFloat  x c0) = CFloat  (f x) c0
    fmap f (CString x c0) = CString (f x) c0
    fmap f (CUnit   x)    = CUnit   (f x)

instance Annotated CPrim where
    annotation (CByte   a _) = a
    annotation (CChar   a _) = a
    annotation (CShort  a _) = a
    annotation (CInt    a _) = a
    annotation (CFloat  a _) = a
    annotation (CString a _) = a
    annotation (CUnit   a)   = a
    annotations c = [annotation c]

instance Show (CPrim a) where
    show (CByte   _ x) = show x
    show (CChar   _ x) = show x
    show (CShort  _ x) = show x
    show (CInt    _ x) = show x
    show (CFloat  _ x) = show x
    show (CString _ x) = show x
    show (CUnit _)     = "()"

showFields :: Show t => String -> [(String, t)] -> String
showFields eq fs = cdelim [lbl ++ eq ++ show e | (lbl, e) <- fs] "; "

isArrayTy :: Ty a -> Bool
isArrayTy (TArray _ _) = True
isArrayTy _            = False

isExType :: Ty a -> Bool
isExType (TExists _ _ _) = True
isExType _               = False

isFNTy :: Ty a -> Bool
isFNTy (TFn _ _ _ _) = True
isFNTy _             = False

fnArgTys :: Ty a -> [Ty a]
fnArgTys (TFn _ _ atys _) = atys
fnArgTys fty              = error ("Cannot apply a value of type '" ++ show fty ++ "'.")

fnRTy :: Ty a -> Ty a
fnRTy (TFn _ _  _ r) = r
fnRTy fty            = error ("Cannot apply a value of type '" ++ show fty ++ "'.")

vtype :: Annotation a => Ty a -> String -> Ty a
vtype (TVariant a vs) v = suj (lookup v vs) (describe a ++ ": Unable to lookup '" ++ v ++ "' in the variant " ++ show (TVariant a vs))
vtype ty lbl = error (describe (annotation ty) ++ ": Not a variant type: " ++ show ty ++ ", can't read " ++ lbl ++ ".")

vlookup :: Annotation a => Ty a -> String -> Ty a
vlookup t@(TVariant a vs) v = suj (lookup v vs) (describe a ++ ": Unable to lookup '" ++ v ++ "' in the variant " ++ show t)
vlookup t lbl = error (describe (annotation t) ++ ": Not a variant type: " ++ show t ++ ", can't read " ++ lbl ++ ".")

vtags :: Ty a -> [(String, Ty a)]
vtags (TVariant _ vs) = vs

vindex :: [(String, Ty a)] -> String -> Int
vindex ts t = at t (map first ts)

rlookup :: Annotation a => Ty a -> String -> Ty a
rlookup t@(TRecord a fs) f = suj (lookup f fs) (describe a ++ ": Unable to lookup '" ++ f ++ "' in the record " ++ show t)
rlookup t lbl = error (describe (annotation t) ++ ": Unable to lookup the label '" ++ lbl ++ "' in a non-record type: " ++ show t)

rtype :: Annotation a => Ty a -> String -> Ty a
rtype (TRecord a fs) f = suj (lookup f fs) (describe a ++ ": Unable to lookup '" ++ f ++ "' in the record " ++ show (TRecord a fs))
rtype ty lbl = error (describe (annotation ty) ++ ": Unable to lookup the label '" ++ lbl ++ "' in a non-record type: " ++ show ty)

rfields :: Ty a -> [(String, Ty a)]
rfields (TRecord _ fs) = fs

arrTy :: Ty a -> Ty a
arrTy (TArray _ x) = x

exname :: Ty a -> String -> Ty a
exname (TExists _ x ty) tv = tyRename ty x tv
exname e _                 = error ("Not an existential type: " ++ show e)

tyRename :: Ty a -> String -> String -> Ty a
tyRename (TPrim d x) _ _     = TPrim d x
tyRename (TVar d x) o n | x == o = TVar d n
tyRename (TVar d x) o n          = TVar d x
tyRename (TFn d cc as r) o n = TFn d cc [tyRename a o n | a <- as] (tyRename r o n)
tyRename (TVariant d fs) o n = TVariant d [(f, tyRename t o n) | (f, t) <- fs]
tyRename (TRecord d fs) o n  = TRecord d [(f, tyRename t o n) | (f, t) <- fs]
tyRename (TExists d x t) o _ | x == o = TExists d x t
tyRename (TExists d x t) o n          = TExists d x (tyRename t o n)
tyRename (TArray d t) o n    = TArray d (tyRename t o n)
tyRename (TMu d x ty) o _ | x == o = TMu d x ty
tyRename (TMu d x ty) o n          = TMu d x (tyRename ty o n)

tyBinderNames :: Ty a -> [String]
tyBinderNames (TPrim _ _)        = []
tyBinderNames (TVar _ _)         = []
tyBinderNames (TFn _ _ args rty) = concatMap tyBinderNames (rty:args)
tyBinderNames (TVariant _ fs)    = concatMap (tyBinderNames . snd) fs
tyBinderNames (TRecord _ fs)     = concatMap (tyBinderNames . snd) fs
tyBinderNames (TExists _ v ty)   = v : tyBinderNames ty
tyBinderNames (TArray _ ty)      = tyBinderNames ty
tyBinderNames (TMu _ v ty)       = v : tyBinderNames ty

tyFreshBinder :: Ty a -> String
tyFreshBinder ty = first [tn | tn <- allNames, not (tn `elem` usedNames)] where
    usedNames = tyBinderNames ty
    allNames  = "a":"b":"c":"d":"e":"f":"g":"h":"i":"j":"k":"l":"m":"n":"o":"p":"q":"r":"s":"t":"u":"v":"w":"x":"y":"z":["t"++show i | i <- [0..]]

rollTy :: Ty a -> Ty a -> Ty a
rollTy rty ty = TMu a x (roll ty) where
    a = annotation ty
    x = tyFreshBinder ty
    roll ty' | rty == ty'    = TVar a x
    roll (TPrim a x)         = TPrim a x
    roll (TVar a x)          = TVar a x
    roll (TFn a cc args rty) = TFn a cc (map roll args) (roll rty)
    roll (TVariant a fs)     = TVariant a [(v, roll ty) | (v, ty) <- fs]
    roll (TRecord a fs)      = TRecord  a [(v, roll ty) | (v, ty) <- fs]
    roll (TExists a x ty)    = TExists a x (roll ty)
    roll (TArray a ty)       = TArray a (roll ty)
    roll (TMu a x ty)        = TMu a x (roll ty)

unrollTy :: Ty a -> Ty a -> Ty a
unrollTy rty ty = unroll ty where
    unroll (TPrim a x)                    = TPrim a x
    unroll (TVar a x)                     = TVar a x
    unroll (TFn a cc args rty)            = TFn a cc (map unroll args) (unroll rty)
    unroll (TVariant a fs)                = TVariant a [(v, unroll ty) | (v, ty) <- fs]
    unroll (TRecord  a fs)                = TRecord  a [(v, unroll ty) | (v, ty) <- fs]
    unroll (TExists a x ty)               = TExists a x (unroll ty)
    unroll (TArray a ty)                  = TArray a (unroll ty)
    unroll rty'@(TMu _ x b) | rty == rty' = rewriteTy rty x b
    unroll (TMu a y ty)                   = TMu a y (unroll ty)

rewriteTy :: Ty a -> String -> Ty a -> Ty a
rewriteTy rty _ (TPrim a x)                 = TPrim a x
rewriteTy rty x (TVar _ x') | x' == x       = rty
rewriteTy rty _ (TVar a x)                  = TVar a x
rewriteTy rty x (TFn a cc args rty')        = TFn a cc (map (rewriteTy rty x) args) (rewriteTy rty x rty')
rewriteTy rty x (TVariant a fs)             = TVariant a [(v, rewriteTy rty x ty) | (v, ty) <- fs]
rewriteTy rty x (TRecord  a fs)             = TRecord  a [(v, rewriteTy rty x ty) | (v, ty) <- fs]
rewriteTy rty x (TExists a x' ty) | x == x' = TExists a x' ty
rewriteTy rty x (TExists a y ty)            = TExists a y (rewriteTy rty x ty)
rewriteTy rty x (TArray a ty)               = TArray a (rewriteTy rty x ty)
rewriteTy rty x (TMu a x' ty) | x == x'     = TMu a x' ty
rewriteTy rty x (TMu a y ty)                = TMu a y (rewriteTy rty x ty)

isPrimTy :: String -> Bool
isPrimTy "unit"  = True
isPrimTy "byte"  = True
isPrimTy "char"  = True
isPrimTy "short" = True
isPrimTy "int"   = True
isPrimTy "float" = True
isPrimTy _       = False

expandTy :: [(String, Ty a)] -> Ty a -> Ty a
expandTy defs (TPrim a x) = TPrim a x
expandTy defs (TVar a v) = select (lookup v defs) where
    select (Just ty') = ty'
    select Nothing    = TVar a v
expandTy defs (TFn a cc args rty) = TFn a cc (map (expandTy defs) args) (expandTy defs rty)
expandTy defs (TVariant a fs) = TVariant a [(v, expandTy defs ty) | (v, ty) <- fs]
expandTy defs (TRecord a fs) = TRecord a [(v, expandTy defs ty) | (v, ty) <- fs]
expandTy defs (TExists a v ty) = TExists a v (expandTy defs ty)
expandTy defs (TArray a ty) = TArray a (expandTy defs ty)
expandTy defs (TMu a x ty) = TMu a x (expandTy defs ty)

variantCaseID :: Ty a -> String -> Int
variantCaseID (TVariant _ ts) lbl = atBy (\(x,_) -> x == lbl) ts

primTy :: CPrim a -> Ty a
primTy (CByte  a _)  = TPrim a "byte"
primTy (CChar  a _)  = TPrim a "char"
primTy (CShort a _)  = TPrim a "short"
primTy (CInt   a _)  = TPrim a "int"
primTy (CFloat a _)  = TPrim a "float"
primTy (CString a _) = TArray a (TPrim a "char")
primTy (CUnit a)     = TPrim a "unit"

tyFV :: Ty a -> [String]
tyFV (TPrim    _ _)          = []
tyFV (TVar     _ x)          = [x]
tyFV (TFn      _ _ atys rty) = concatMap tyFV atys ++ tyFV rty
tyFV (TVariant _ cs)         = concatMap (tyFV . snd) cs
tyFV (TRecord  _ cs)         = concatMap (tyFV . snd) cs
tyFV (TExists  _ vn ty)      = tyFV ty \\ [vn]
tyFV (TArray   _ ty)         = tyFV ty
tyFV (TMu      _ vn ty)      = tyFV ty \\ [vn]

{- Type environments and top-level bindings -}
class Show tenv => TypeEnv tenv a | tenv -> a where
    emptyTEnv     :: tenv
    isTopLevelVar :: tenv -> String -> Bool
    tydef         :: tenv -> String -> Maybe (Ty a)
    pushtys       :: tenv -> [(String, Ty a)] -> tenv
    pushty        :: tenv -> String -> Ty a -> tenv
    pushtysTop    :: tenv -> [(String, Ty a)] -> tenv
    pushtyTop     :: tenv -> String -> Ty a -> tenv
    varnames      :: tenv -> [String]
    mapTyFn       :: (String -> Ty a -> Ty a) -> tenv -> tenv
    updateVarTy   :: String -> (Ty a -> Ty a) -> tenv -> tenv
    
    pushty tenv v ty = pushtys tenv [(v, ty)]
    pushtysTop = pushtys
    pushtyTop tenv v ty = pushtysTop tenv [(v, ty)]
	
instance TypeEnv [[(String, Ty a)]] a where
    emptyTEnv = []
    
    isTopLevelVar []         v                           = False
    isTopLevelVar [frame]    v                           = isJust (lookup v frame)
    isTopLevelVar (frame:_)  v | isJust (lookup v frame) = False
    isTopLevelVar (_:frames) v                           = isTopLevelVar frames v
    
    tydef []             v = Nothing
    tydef (frame:frames) v = either Just (const (tydef frames v)) (choice $ lookup v frame)
    
    pushtys te vs      = vs : te
    pushtysTop te vs   = reverse (ins (reverse te)) where
        ins [] = [vs]
        ins (f:fs) = (vs++f):fs

    varnames te        = concatMap (map fst) te
    mapTyFn f tenv     = [[(x, f x ty) | (x, ty) <- frame] | frame <- tenv]
    
    updateVarTy v u []                           = []
    updateVarTy v u (f:fs) | isJust (lookup v f) = [if vn == v then (vn, u ty) else (vn, ty) | (vn, ty) <- f] : fs
    updateVarTy v u (f:fs)                       = f : updateVarTy v u fs

isdef :: TypeEnv tenv a => tenv -> String -> Bool
isdef tenv v = mb (tydef tenv v) where
    mb (Just _) = True
    mb _        = False
