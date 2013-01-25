
module Util.Num where
import Data.Bits

downto :: (Ord n, Num n) => n -> n -> [n]
downto t b | t < b = []
downto t b         = t : downto (t - 1) b

upto :: (Ord n, Num n) => n -> n -> [n]
upto b t | b > t = []
upto b t         = b : upto (b + 1) t

seqMax :: Ord a => a -> [a] -> a
seqMax m xs = foldl max m xs

seqMaxBy :: Ord b => (a -> b) -> a -> [a] -> a
seqMaxBy _ m []     = m
seqMaxBy f m (x:xs) = seqMaxBy f (fmax m x) xs where
    fmax x y | f x >= f y = x
    fmax _ y              = y

seqMin :: Ord a => a -> [a] -> a
seqMin m xs = foldl min m xs

seqMinBy :: Ord b => (a -> b) -> a -> [a] -> a
seqMinBy _ m []     = m
seqMinBy f m (x:xs) = seqMinBy f (fmin m x) xs where
    fmin x y | f x <= f y = x
    fmin _ y              = y

castn :: (Integral a, Num b) => a -> b
castn x = fromInteger (toInteger x)

align :: (Integral a, Integral b) => a -> b -> b
align v b = b * ceiling ((castn v) / (castn b))

intAlign :: (Integral a, Integral b) => a -> b -> b
intAlign v b = b * floor ((castn v) / (castn b))

lg :: Floating a => a -> a
lg x = log x / log 2

bytes :: Integral a => a -> a
bytes x = ceiling (lg (castn $ x + 1) / 8.0)

withinLen :: Int -> Int -> Int -> Bool
withinLen x lb len = x >= lb && x < (lb + len)

within :: Ord a => a -> a -> a -> Bool
within x l b = x >= l && x <= b
    
band :: Bits a => [a] -> a
band []     = 0
band (x:xs) = foldl (.&.) x xs

bor :: Bits a => [a] -> a
bor []     = 0
bor (x:xs) = foldl (.|.) x xs
