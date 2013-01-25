
{-
    A module for assembling graphviz diagrams and rendering them (invoking dot)
-}
module Util.Graphviz where
import Util.String
import Util.Directory
import Data.Char
import System.Process
import Directory

-- remove invalid characters from a graphviz label (and convert some sequences to special symbols)
gvEsc :: String -> String
gvEsc x = subnum (foldl (\x' (s, s') -> replace s s' x') x reps) where
    reps = [("&", "&amp;"), ("<-", "&larr;"), ("<", "&lt;"), (">", "&gt;"), ("#\\|", "&"), ("\\|#", ";"), ("\\|", "&x7c;"), ("#", "")]
    subnum ('(':cs) = if isNum p then "<font point-size=\"8\">" ++ p ++ "</font>" ++ subnum cs' else '(' : subnum cs where (p,cs') = toEP cs
    subnum (c:cs)   = c : subnum cs
    subnum []       = []
    isNum s         = all isDigit s
    toEP s          = (p, drop (length p + 1) s) where p = takeWhile (not . (== ')')) s

-- wrap a digraph description
digraphDesc :: String -> String
digraphDesc g = "digraph G {\n  graph [fontsize=30 splines=true overlap=false];\n  ratio = auto;\n" ++ g ++ "}\n"

-- wrap a digraph description with a label
digraphDescWLabel :: String -> String -> String
digraphDescWLabel lbl g = "digraph G {\n   label = \"" ++ lbl ++ "\";\n  graph [fontsize=30 splines=true overlap=false];\n  ratio = auto;\n" ++ g ++ "}\n"

-- render a graph description to a GIF file using dot
dotFile :: String -> String -> IO ()
dotFile fileName graphDesc = do
    let fileName' = fileName ++ ".dot";
    makeFileDir fileName'
    writeFile fileName' graphDesc;
    h <- runProcess "dot" [fileName', "-Tgif", "-o" ++ fileName] Nothing Nothing Nothing Nothing Nothing;
    waitForProcess h;
    removeFile fileName';
    return ()
