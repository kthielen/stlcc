{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.Type where
import qualified STLC.Type as ST
import Util.Annotated
import Util.Sequence
import Util.Tuples
import Util.String
import Util.Num
import Data.Set as Set hiding (map, filter)
import Data.List as List hiding (delete, union, (\\))
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
          | TPtr     a (Ty a)
          deriving (Eq, Ord)

data CallConv = CFn deriving (Eq, Ord, Show)

data CPrim a = CByte a Word8
             | CChar a Char
             | CShort a Int
             | CInt a Int
             | CFloat a Float
             | CString a String
		     | CUnit a
             deriving (Eq, Ord)

instance Annotated Ty where
    annotation (TPrim    a _)     = a
    annotation (TVar     a _)     = a
    annotation (TFn      a _ _ _) = a
    annotation (TVariant a _)     = a
    annotation (TRecord  a _)     = a
    annotation (TExists  a _ _)   = a
    annotation (TArray   a _)     = a
    annotation (TMu      a _ _)   = a
    annotation (TPtr     a _)     = a
    annotations (TPrim    a _)          = [a]
    annotations (TVar     a _)          = [a]
    annotations (TFn      a _ atys rty) = a : concatMap annotations atys ++ annotations rty
    annotations (TVariant a ctys)       = a : concat [annotations ty | (_, ty) <- ctys]
    annotations (TRecord  a ctys)       = a : concat [annotations ty | (_, ty) <- ctys]
    annotations (TExists  a _ t)        = a : annotations t
    annotations (TArray   a t)          = a : annotations t
    annotations (TMu      a _ t)        = a : annotations t
    annotations (TPtr     a t)          = a : annotations t

instance Show (Ty a) where
    show (TPrim _ "unit")       = "()"
    show (TPrim _ s)            = s
    show (TVar _ s)             = s
    show (TFn _ cc as r)        = "(" ++ cdelim (map show as) ", " ++ ") -> " ++ show r
    show (TVariant _ fs)        = "<" ++ showFields ":" fs ++ ">"
    show (TRecord _ fs)         = "{" ++ showFields ":" fs ++ "}"
    show (TExists _ v ty)       = "exists " ++ v ++ "." ++ show ty
    show (TArray _ e)           = "[" ++ show e ++ "]"
    show (TMu _ x ty)           = "mu " ++ x ++ "." ++ show ty
    show (TPtr _ ty)            = "^" ++ show ty

showFields :: Show t => String -> [(String, t)] -> String
showFields eq fs = cdelim [lbl ++ eq ++ show e | (lbl, e) <- fs] "; "

instance Show (CPrim a) where
    show (CByte   _ x) = show x
    show (CChar   _ x) = show x
    show (CShort  _ x) = show x
    show (CInt    _ x) = show x
    show (CFloat  _ x) = show x
    show (CString _ x) = show x
    show (CUnit _)     = "()"

isPrimTy :: String -> Bool
isPrimTy "unit"  = True
isPrimTy "byte"  = True
isPrimTy "char"  = True
isPrimTy "short" = True
isPrimTy "int"   = True
isPrimTy "float" = True
isPrimTy _       = False

primTy :: CPrim a -> Ty a
primTy (CByte  a _)  = TPrim a "byte"
primTy (CChar  a _)  = TPrim a "char"
primTy (CShort a _)  = TPrim a "short"
primTy (CInt   a _)  = TPrim a "int"
primTy (CFloat a _)  = TPrim a "float"
primTy (CString a _) = TArray a (TPrim a "char")
primTy (CUnit a)     = TPrim a "unit"

liftSTC :: ST.CPrim a -> CPrim a
liftSTC (ST.CByte  a x)  = CByte a x
liftSTC (ST.CChar  a x)  = CChar a x
liftSTC (ST.CShort a x)  = CShort a x
liftSTC (ST.CInt   a x)  = CInt a x
liftSTC (ST.CFloat a x)  = CFloat a x
liftSTC (ST.CString a x) = CString a x
liftSTC (ST.CUnit a)     = CUnit a

liftSTT :: ST.Ty a -> Ty a
liftSTT = undefined

machineTy :: ST.Ty a -> Ty a
machineTy = undefined

machineTyFields :: [(b, ST.Ty a)] -> [(b, Ty a)]
machineTyFields = undefined

vtype :: Annotation a => Ty a -> String -> Ty a
vtype t v = suj (lookup v (vtags t)) (describe (annotation t) ++ ": Unable to lookup '" ++ v ++ "' in the variant " ++ show t)

vtags :: Annotation a => Ty a -> [(String, Ty a)]
vtags (TVariant _ vs) = vs
vtags ty = error (describe (annotation ty) ++ ": Not a variant type: " ++ show ty)

vindex :: [(String, Ty a)] -> String -> Int
vindex ts t = at t (map first ts)

arrTy :: Ty a -> Ty a
arrTy (TArray _ x) = x

sizeof :: Annotation a => Ty a -> Int
sizeof (TPrim _ "unit")  = 0
sizeof (TPrim _ "byte")  = 1
sizeof (TPrim _ "char")  = 1
sizeof (TPrim _ "short") = 2
sizeof (TPrim _ "int")   = 4
sizeof (TPrim _ "float") = 8
sizeof (TFn _ _ _ _)     = 4
sizeof (TVariant a ts) | variantIsNullablePtr (TVariant a ts) = 4
sizeof (TVariant _ ts) = selectSize (sizeofVariantTag ts + sizeofVariantPayload ts) where
    selectSize 3 = 4
    selectSize n = n
sizeof (TRecord _ fs)    = max 4 (sum (fSizes fs))
sizeof (TExists _ _ _)   = 4
sizeof (TArray _ _)      = 4
sizeof (TPtr _ _)        = 4
sizeof (TMu _ _ ty)      = sizeof ty
sizeof e                 = error ("Unable to compute size of: " ++ show e)

sizeofVariantTag :: Annotation a => [(String, Ty a)] -> Int
sizeofVariantTag ts | variantIsNullablePtr (TVariant nullAnnotation ts) = 4
sizeofVariantTag ts = bytes (length ts)

variantTagType :: Annotation a => Ty a -> Ty a
variantTagType (TVariant a ts) | sizeofVariantTag ts == 1 = TPrim a "byte"
variantTagType (TVariant a ts) | sizeofVariantTag ts == 2 = TPrim a "short"
variantTagType (TVariant a ts) | sizeofVariantTag ts == 4 = TPrim a "int"

sizeofVariantPayload :: Annotation a => [(b, Ty a)] -> Int
sizeofVariantPayload ts = seqMax 0 [sizeof ty | (_, ty) <- ts]
-- sizeofVariantPayload ts = seqMax 0 [sizeof (machineTy ty) | (_, ty) <- ts]

variantIsNullablePtr :: Annotation a => Ty a -> Bool
variantIsNullablePtr (TVariant _ ts) = ptc == 1 && etc == 0 && utc < 32 where
    (ptc, utc, etc) = foldl classifyVTy (0,0,0) [ty | (_, ty) <- ts]
    classifyVTy (ptc', utc', etc') (TPtr _ _)       = (ptc' + 1, utc',     etc')
    classifyVTy (ptc', utc', etc') (TPrim _ "unit") = (ptc',     utc' + 1, etc')
    classifyVTy (ptc', utc', etc') _                = (ptc',     utc',     etc' + 1)
variantIsNullablePtr e = error (describe (annotation e) ++ ": Not a variant: " ++ show e)

variantNullablePtrLbl :: Ty a -> String
variantNullablePtrLbl (TVariant _ ts) = head [lbl | (lbl, ty) <- ts, isPtrTy ty] where
    isPtrTy (TPtr _ _) = True
    isPtrTy _          = False
    
variantNullablePtrID :: Ty a -> Int
variantNullablePtrID ty = variantCaseID ty (variantNullablePtrLbl ty)

variantCaseID :: Ty a -> String -> Int
variantCaseID (TVariant _ ts) lbl = atBy (\(x,_) -> x == lbl) ts

tySizes :: Annotation a => [Ty a] -> [Int]
tySizes = map sizeof

fSizes :: Annotation a => [(b, Ty a)] -> [Int]
fSizes fs = tySizes [ty | (_, ty) <- fs]

recordOffset :: Annotation a => [(String, Ty a)] -> String -> Int
recordOffset fs lbl = sum $ map (sizeof . second) $ takeWhile (not . (== lbl) . first) fs
