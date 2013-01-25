
module Util.String where
import Util.Sequence
import Util.Num
import Data.Char
import Data.Bits
import Text.Regex
import GHC.Word

cdelim :: [[a]] -> [a] -> [a]	
cdelim []     _ = []
cdelim [x]    _ = x
cdelim (x:xs) e = x ++ e ++ cdelim xs e

pad :: [a] -> Int -> a -> [a]
pad ws n w = ws ++ take (n - length ws) (repeat w)

replace :: String -> String -> String -> String
replace x y s = subRegex (mkRegex x) s y

upper :: String -> String
upper = map toUpper

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

csplit :: Eq a => [a] -> [a] -> [[a]]
csplit s ss = unfold (lsplit s ss) where
    unfold (x, []) = [x]
    unfold (x, s') = x : unfold (lsplit s' ss)

lsplit :: Eq a => [a] -> [a] -> ([a], [a])
lsplit s ss = (take n s, drop (n + length ss) s) where n = findSubSeq s ss

rsplit :: Eq a => [a] -> [a] -> ([a], [a])
rsplit s ss = (reverse $ drop (n + length ss') s', reverse $ take n s') where
    s'  = reverse s
    ss' = reverse ss
    n   = findSubSeq s' ss'

parseFilePath :: String -> (String, String, String)
parseFilePath s = splitStr (likelyDelim s) where
    likelyDelim s | count (== '/') s > 0 = "/"
    likelyDelim _                        = "\\"
    splitStr delim = (path, file, ext) where
        (path, filee) = rsplit s delim
        (file, ext)   = rsplit filee "."

mustEndWith :: Eq a => [a] -> [a] -> [a]
mustEndWith seq sfx = if endsWith then seq else seq ++ sfx where
    xfs      = take (length sfx) (reverse seq)
    endsWith = (reverse xfs) == sfx

prefix :: Show a => String -> a -> String
prefix pfx x = pfx ++ show x

showHex :: [Word8] -> String
showHex = concatMap hexByte

hexByte :: Word8 -> String
hexByte x = [nybByte hx, nybByte lx] where
    hx = x `shiftR` 4
    lx = x .&. 0x0f

nybByte :: Word8 -> Char
nybByte n | n < 10 = chr $ ord '0' + (castn n)
nybByte n          = chr $ ord 'a' + (castn $ n - 10)

readHex :: String -> [Word8]
readHex (h:l:rs) = ((charNyb h `shiftL` 4) + charNyb l) : readHex rs
readHex [c]      = [charNyb c]
readHex []       = []

charNyb :: Char -> Word8
charNyb c | within c '0' '9' = castn $  0 + ord c - ord '0'
charNyb c | within c 'a' 'f' = castn $ 10 + ord c - ord 'a'
charNyb c | within c 'A' 'F' = castn $ 10 + ord c - ord 'A'
