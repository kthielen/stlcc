{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ImpredicativeTypes #-}

module Util.ReadPosTracking where
import Util.Annotated

-- monadic lexer tracking file/line positions
type FilePos        = (Int, Int) -- line/col
type FileExtent     = (String, FilePos, FilePos) -- filename, start-pos, end-pos
type ParseState     = (String, String, [FileExtent], FilePos)
data ParseResult a  = Success ParseState a | Failed ParseState String deriving (Eq, Show)
data ReadPosTrack a = ReadPosTrack (ParseState -> ParseResult a)
type LexDiff a      = (String, Int, Int, Int, Int, a)

instance Annotation FileExtent where
    nullAnnotation = ("", (0,0), (0,0))
    describe (f, (l, c), (l', c')) | l == l' = f ++ "[" ++ show (l+1) ++ "," ++ show (c+1) ++ "-" ++ show (c'+1) ++ "]"
    describe (f, (l, c), (l', c'))           = f ++ "[" ++ show (l+1) ++ "," ++ show (c+1) ++ "]-[" ++ show (l'+1) ++ "," ++ show (c'+1) ++ "]"

instance Monad ReadPosTrack where
	return x = ReadPosTrack $ \state -> Success state x
	(ReadPosTrack f) >>= g = ReadPosTrack $ \state -> step (f state) where
		step (Success state' x)  = applyParseState (g x) state'
		step (Failed state' msg) = Failed state' msg

applyParseState :: ReadPosTrack a -> ParseState -> ParseResult a
applyParseState (ReadPosTrack f) x = f x

applyReadState :: ReadPosTrack a -> ParseState -> a
applyReadState f x = extract (applyParseState f x)

extract :: ParseResult a -> a
extract (Success _ x)  = x
extract (Failed p msg) = error ("Parse failure at: " ++ show p ++ "\n" ++ msg)

initialReadState :: String -> String -> ParseState
initialReadState filename text = (filename, text, [], (0, 0))

parseFailure :: String -> ReadPosTrack a
parseFailure msg = ReadPosTrack (\x -> Failed x msg)

pushTS :: FileExtent -> ReadPosTrack ()
pushTS te = ReadPosTrack step where
	step (f, d, s, p) = Success (f, d, te : s, p) ()

popTS :: ReadPosTrack FileExtent
popTS = ReadPosTrack step where
    step (f, d, te:s', p) = Success (f, d, s', p) te
    step x                = Failed x "internal parser error (stack underflow)"

peekTS :: ReadPosTrack FileExtent
peekTS = ReadPosTrack step where
    step (f, d, te:s, p) = Success (f, d, te:s, p) te
    step x               = Failed x "internal parser error (stack underflow in peek)"

popNTS :: Int -> ReadPosTrack [FileExtent]
popNTS 0 = return []
popNTS n = do
    t  <- popTS
    ts <- popNTS (n-1)
    return (t:ts)

globExtent :: [FileExtent] -> FileExtent
globExtent ts = glob (last ts) (head ts) where
    glob (f, b, _) (_, _, e) = (f, b, e)

reduce :: Int -> (FileExtent -> a) -> ReadPosTrack a
reduce 0 k = do
    ts <- peekTS
    pushTS ts
    return (k ts)
reduce n k = do
    ts <- popNTS (n+1)
    let t' = globExtent (tail ts)
    pushTS t'
    pushTS (head ts)
    return (k t')

getInput :: ReadPosTrack String
getInput = do
    (_, d, _, _) <- getState
    return d

getState :: ReadPosTrack ParseState
getState = ReadPosTrack (\x -> Success x x)

lexExtentTrack :: (String -> LexDiff a) -> (a -> ReadPosTrack b) -> ReadPosTrack b
lexExtentTrack step k = do
    inp <- getInput
    t   <- applyLexDiff (step inp)
    k t

applyLexDiff :: LexDiff a -> ReadPosTrack a
applyLexDiff (d, ns, cs, sn, sc, t) = ReadPosTrack step where
    step (f, _, s, (l, c)) = Success (f, d, e:s, p') t where
        e  = (f, (l+sn, c'), p')
        p' = if ns > 0 then (l+ns, cs) else (l, c+cs)
        c' = if ns > 0 then sc else c+sc

lexDiff :: String -> a -> LexDiff a
lexDiff cs t = (cs, 0, 0, 0, 0, t)

dLine :: Int -> LexDiff a -> LexDiff a
dLine n (cs', n', c, sn, sc, t) = (cs', n' + n, c, sn + n, sc, t)

dCol :: Int -> LexDiff a -> LexDiff a
dCol c (cs', 0, dc, sn, sc, t) = (cs', 0, c+dc, sn, sc, t)
dCol _ x = x

dSkipCol :: Int -> LexDiff a -> LexDiff a
dSkipCol dc (cs', 0, c, sn, sc, t) = (cs', 0, c+dc, sn, sc+dc, t)
dSkipCol _ x = x

-- pretty print utilities for working with file extents
pad :: a -> Int -> [a]
pad c n = take n (repeat c)

highlight :: String -> Int -> Int -> Int -> Int -> String
highlight text bl bc el ec | el == bl = (lines text !! bl) ++ "\n" ++ pad ' ' bc ++ pad '-' (ec - bc) ++ "\n"

highlight text bl bc el ec = hline bline bc (length bline) ++ concatMap hfline ilines ++ hline eline 0 ec where
    ls            = lines text
    bline         = ls !! bl
    ilines        = take (el - bl - 1) (drop (1+bl) ls)
    eline         = ls !! el
    hline l sc ec = l ++ "\n" ++ pad ' ' sc ++ pad '-' (ec - sc) ++ "\n"
    hfline l      = hline l 0 (length l)

highlight' :: String -> FileExtent -> String
highlight' txt (_, (l, c), (l', c')) = highlight txt l c l' c'

printPositions :: String -> [FileExtent] -> IO ()
printPositions text x = putStr (concatMap (highlight' text) x)
