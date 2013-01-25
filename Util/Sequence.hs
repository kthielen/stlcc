
module Util.Sequence where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

names :: [String]
names = shortNames ++ ["t" ++ show i | i <- [0..]] where
    shortNames = map box "abcdefghijklmnopqrstuvwxyz"
    box x = [x]

insertBatch :: Ord k => Map.Map k v -> [(k, v)] -> Map.Map k v
insertBatch = foldl (\m (k, v) -> Map.insert k v m)

deleteAll :: Ord a => [a] -> Set.Set a -> Set.Set a
deleteAll []     s = s
deleteAll (x:xs) s = deleteAll xs (Set.delete x s)

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

count :: (a -> Bool) -> [a] -> Int
count f []           = 0
count f (x:xs) | f x = 1 + count f xs
count f (_:xs)       = count f xs

uj :: Maybe a -> a
uj (Just x) = x

suj :: Maybe a -> String -> a
suj (Just x) _ = x
suj Nothing  s = error s

duj :: Maybe a -> a -> a
duj (Just x) _ = x
duj Nothing  x = x

shead :: [a] -> String -> a
shead []    x = error x
shead (x:_) _ = x

mhead :: [a] -> Maybe a
mhead []    = Nothing
mhead (x:_) = Just x

rtail :: [a] -> [a]
rtail xs = reverse (tail $ reverse xs)

choice :: Maybe a -> Either a ()
choice (Just x) = Left x
choice Nothing  = Right ()

meither :: (a -> b) -> b -> Maybe a -> b
meither f _ (Just x) = f x
meither _ x Nothing  = x

meitherM :: Monad m => m (Maybe a) -> (a -> m b) -> m b -> m b
meitherM x f d = do
    x' <- x;
    meither f d x'

chooseDefault :: a -> Maybe a -> a
chooseDefault _ (Just x) = x
chooseDefault x Nothing  = x

spani :: Num n => (a -> n -> Bool) -> [a] -> ([a], [a])
spani f xs = siter 0 [] xs where
    siter n pfx (x:xs) | f x n = siter (n+1) (x:pfx) xs
    siter _ pfx xs'            = (reverse pfx, xs')
    
splitTake :: [a] -> Int -> ([a], [a])
splitTake xs n = spani (const (< n)) xs

tryTake :: [a] -> Int -> Maybe [a]
tryTake xs n = fs 0 [] xs where
    fs i a _ | i == n = Just (reverse a)
    fs _ _ []         = Nothing
    fs i a (x:xs)     = fs (i+1) (x:a) xs

subseqs :: [a] -> Int -> [[a]]
subseqs xs n = select (tryTake xs n) where
    select Nothing = []
    select (Just xs') = xs' : subseqs (tail xs) n

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix pfx seq = pfx == (take (length pfx) seq)

findSubSeq :: Eq a => [a] -> [a] -> Int
findSubSeq s ss = fss 0 s where
    sslen = length ss
    fss i []                       = i
    fss i s' | ss == take sslen s' = i
    fss i (s:s')                   = fss (i+1) s'

group :: Ord a => [(a, b)] -> [(a, [b])]
group ps = Map.toList (buildMap Map.empty ps) where
    buildMap m ((k, v):rs) = buildMap (addMap m k v (Map.lookup k m)) rs
    buildMap m []          = m
    addMap m k v Nothing   = Map.insert k [v] m
    addMap m k v (Just vs) = Map.insert k (v:vs) m

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f xs = group [(f x, x) | x <- xs]

collapseEither :: Either a a -> a
collapseEither (Left x)  = x
collapseEither (Right x) = x

atBy :: (a -> Bool) -> [a] -> Int
atBy f xs = atb 0 xs where
    atb n []            = n
    atb n (x:xs') | f x = n
    atb n (_:xs')       = atb (n+1) xs'
    
at :: Eq a => a -> [a] -> Int
at x xs = atBy (== x) xs

mlist :: Maybe [a] -> [a]
mlist (Just xs) = xs
mlist Nothing   = []

(!!!) :: Show a => [a] -> Int -> a
x !!! n | n < length x = x !! n
x !!! n                = error ("Unable to lookup index " ++ show n ++ " of " ++ show x)

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f xs = mapii 0 f xs where
    mapii n f (x:xs') = (f n x):(mapii (n+1) f xs')
    mapii n f []      = []

foldli :: (Int -> a -> b -> a) -> a -> [b] -> a
foldli f s xs = iter s 0 xs where
    iter s' n [] = s'
    iter s' n (x:xs') = iter (f n s' x) (n+1) xs'
    
foldri :: (Int -> a -> b -> b) -> b -> [a] -> b
foldri f s xs = iter 0 xs where
    iter n [] = s
    iter n (x:xs) = f n x (iter (n+1) xs)
    
mapiM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
mapiM f xs = mapii 0 f xs where
    mapii n f (x:xs') = do { x' <- f n x; xs'' <- mapii (n+1) f xs'; return (x':xs'') }
    mapii n f []      = return []

unfold :: (a -> b) -> (a -> a) -> (a -> Bool) -> a -> [b]
unfold getfn nextfn stopfn x = uf [] x where
    uf vs x' | stopfn x' = (getfn x'):vs
    uf vs x'             = uf ((getfn x'):vs) (nextfn x')

unfoldM :: Monad m => (a -> b) -> (a -> m a) -> (a -> Bool) -> a -> m [b]
unfoldM getfn nextfn stopfn x = uf [] x where
    uf vs x' | stopfn x' = return $ (getfn x'):vs
    uf vs x'             = do { x'' <- nextfn x'; uf ((getfn x'):vs) x'' }
    
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f s []     = return s
foldrM f s (x:xs) = do { s' <- foldrM f s xs; f x s' }

foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM f s []     = return s
foldlM f s (x:xs) = do { s' <- f s x; foldlM f s' xs }

foldlM' :: Monad m => b -> [a] -> (b -> a -> m b) -> m b
foldlM' s xs f = foldlM f s xs

takeM :: Monad m => Int -> m a -> m [a]
takeM 0 a = return []
takeM n a = do { r  <- a; r' <- takeM (n-1) a; return (r:r') }

appendMapVal :: Ord k => Map.Map k [v] -> k -> v -> Map.Map k [v]
appendMapVal m k v = Map.insert k (v : (mlist (Map.lookup k m))) m

insertMapVal :: (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 v) -> k1 -> k2 -> v -> Map.Map k1 (Map.Map k2 v)
insertMapVal m k1 k2 v = Map.insert k1 (merge (Map.lookup k1 m)) m where
    merge (Just m') = Map.insert k2 v m'
    merge Nothing   = Map.insert k2 v Map.empty

map2Lookup :: (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 v) -> k1 -> k2 -> Maybe v
map2Lookup m k1 k2 = next (Map.lookup k1 m) where
    next (Just m') = Map.lookup k2 m'
    next Nothing   = Nothing

unify :: (Show a, Ord a) => (a -> Bool) -> [(a, a)] -> Map.Map a a -> Either (Map.Map a a) String
unify ground ((x, y):rs) m | x == y    = unify ground rs m
unify ground ((x, y):rs) m | ground x && ground y = Right $ "Unification failure: " ++ show x ++ " != " ++ show y
unify ground ((x, y):rs) m | ground x  = unify ground ((y, x):rs) m
unify ground ((x, y):rs) m | bound m x = unify ground ((binding m x, y):rs) m
unify ground ((x, y):rs) m | bound m y = unify ground ((binding m y, x):rs) m
unify ground ((x, y):rs) m             = unify ground rs (bind m x y)
unify ground []          m             = Left m

subst :: Ord a => Map.Map a a -> Map.Map a a
subst m = Map.fromList [(n, binding m n) | (n, _) <- Map.toList m]

bound :: Ord a => Map.Map a a -> a -> Bool
bound m x = isJust (Map.lookup x m)
    
binding :: Ord a => Map.Map a a -> a -> a
binding m x | bound m x = binding m (uj $ Map.lookup x m)
binding _ x             = x

bind :: Ord a => Map.Map a a -> a -> a -> Map.Map a a
bind m x y = Map.insert x y m
