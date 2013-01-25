
module Util.Recursion where
import Util.Sequence

concentricFixedPoint :: Eq a => [a -> a] -> a -> a
concentricFixedPoint = concentricFixedPointBy (==)

concentricFixedPointBy :: (a -> a -> Bool) -> [a -> a] -> a -> a
concentricFixedPointBy donefn fs x = (cfn id fs) x where
    cfn f' (f:fs) = cfn (fixedPointBy donefn $ f . f') fs
    cfn f' []     = f'

concentricFixedPointSeq :: Eq a => [a -> a] -> a -> [a]
concentricFixedPointSeq = concentricFixedPointBySeq (==)

concentricFixedPointBySeq :: (a -> a -> Bool) -> [a -> a] -> a -> [a]
concentricFixedPointBySeq donefn fs x = (cfn id fs) [x] where
    cfn f' (f:fs) = cfn (fix $ (liftFN f) . f') fs
    cfn f' []     = f'
    liftFN f xs = xs ++ [f (last xs)]
    fix f xs = if donefn x x' then rtail xs' else fix f xs' where
        x    = last xs
        x'   = last xs'
        xs'  = f xs

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = fixedPointBy (==)

fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixedPointBy donefn f x = fix (f x) where
    fix x' | donefn x x' = x'
    fix x'               = fixedPointBy donefn f x'

fixedPointSeq :: Eq a => (a -> a) -> a -> [a]
fixedPointSeq = fixedPointBySeq (==)

fixedPointBySeq :: (a -> a -> Bool) -> (a -> a) -> a -> [a]
fixedPointBySeq donefn f x = fix (f x) where
    fix x' | donefn x x' = [x']
    fix x'               = x : fixedPointBySeq donefn f x'
