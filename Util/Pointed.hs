
module Util.Pointed where
import Util.Tuples

next :: ([a], a, [a]) -> ([a], a, [a])
next (pfx, x, s:sfx) = (x:pfx, s, sfx)

nextM :: Maybe ([a], a, [a]) -> Maybe ([a], a, [a])
nextM (Just (_,   _, []))    = Nothing
nextM (Just (pfx, x, s:sfx)) = Just (x:pfx, s, sfx)
nextM Nothing                = Nothing

prev :: ([a], a, [a]) -> ([a], a, [a])
prev (p:pfx, s, sfx) = (pfx, p, s:sfx)

prevM :: Maybe ([a], a, [a]) -> Maybe ([a], a, [a])
prevM (Just ([],    _, _))   = Nothing
prevM (Just (p:pfx, s, sfx)) = Just (pfx, p, s:sfx)
prevM Nothing                = Nothing

eos :: ([a], a, [a]) -> Bool
eos (_, _, []) = True
eos _          = False

eosM :: Maybe ([a], a, [a]) -> Bool
eosM (Just (_, _, [])) = True
eosM (Just _)          = False
eosM Nothing           = True

bos :: ([a], a, [a]) -> Bool
bos ([], _, _) = True
bos _          = False

bosM :: Maybe ([a], a, [a]) -> Bool
bosM (Just ([], _, _)) = True
bosM (Just _)          = False
bosM Nothing           = True

open :: [a] -> ([a], a, [a])
open (x:xs) = ([], x, xs)

openM :: [a] -> Maybe ([a], a, [a])
openM (x:xs) = Just ([], x, xs)
openM []     = Nothing

close :: ([a], a, [a]) -> [a]
close (pfx, x, sfx) = (reverse pfx) ++ (x:sfx)

closeM :: Maybe ([a], a, [a]) -> [a]
closeM (Just c) = close c
closeM Nothing  = []

update :: ([a], a, [a]) -> a -> ([a], a, [a])
update (pfx, _, sfx) x = (pfx, x, sfx)

updateM :: Maybe ([a], a, [a]) -> a -> Maybe ([a], a, [a])
updateM (Just c) x = Just (update c x)
updateM Nothing  _ = Nothing

cut :: ([a], a, [a]) -> ([a], a, [a])
cut (pfx, _, s:sfx) = (pfx, s, sfx)
cut (p:pfx, _, [])  = (pfx, p, [])

cutM :: Maybe ([a], a, [a]) -> Maybe ([a], a, [a])
cutM = fmap cut

pointed :: ([a], a, [a]) -> a
pointed (_, x, _) = x

pointedM :: Maybe ([a], a, [a]) -> Maybe a
pointedM (Just (_, x, _)) = Just x
pointedM Nothing          = Nothing

cutClose :: ([a], a, [a]) -> [a]
cutClose (pfx, _, sfx) = reverse pfx ++ sfx

closePointedPfx :: ([a], a, [a]) -> [a]
closePointedPfx (pfx, x, sfx) = x:(reverse pfx ++ sfx)

findNextBy :: (a -> Bool) -> ([a], a, [a]) -> ([a], a, [a])
findNextBy f p@(pfx, x, sfx) | f x = p
findNextBy f p                     = findNextBy f $ next p

findNextByM :: (a -> Bool) -> Maybe ([a], a, [a]) -> Maybe ([a], a, [a])
findNextByM f (Just p@(pfx, x, sfx)) | f x = Just p
findNextByM _ Nothing                      = Nothing
findNextByM f c                            = findNextByM f $ nextM c

findNext :: Eq a => a -> ([a], a, [a]) -> ([a], a, [a])
findNext x p = findNextBy ((==) x) p

findNextM :: Eq a => a -> Maybe ([a], a, [a]) -> Maybe ([a], a, [a])
findNextM x p = findNextByM ((==) x) p

findBy :: (a -> Bool) -> [a] -> ([a], a, [a])
findBy f xs = findNextBy f $ open xs

findByM :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findByM f xs = findNextByM f $ openM xs

find :: Eq a => a -> [a] -> ([a], a, [a])
find x xs = findBy ((==) x) xs

findM :: Eq a => a -> [a] -> Maybe ([a], a, [a])
findM x xs = findByM ((==) x) xs

upsertSeq :: [a] -> (a -> Bool) -> (Maybe a -> a) -> ([a], a, Int)
upsertSeq xs sfn tfn = choose siter where
    siter          = findByM (sfn . first) (zip xs [0..])
    choose Nothing = (xs ++ [x'], x', length xs) where
        x' = tfn Nothing
    choose (Just (pfx, (x, i), sfx)) = (xs', x', i) where
        x'  = tfn (Just x)
        xs' = map first $ close (pfx, (x', i), sfx)
