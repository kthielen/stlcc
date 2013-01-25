
module Util.Directory where
import Util.String
import Util.Sequence
import Util.Tuples
import Directory
import Data.Char
import Data.Maybe
import qualified Data.List as List
import Control.Monad
import System.Process

makeDir :: String -> IO ()
makeDir ""  = return ()
makeDir "." = return ()
makeDir s = do
    makeDir (fst (rsplit s "/"));
    f <- doesDirectoryExist s;
    when (not f) (createDirectory s)

makeFileDir :: String -> IO ()
makeFileDir s = makeDir (fst (rsplit s "/"))

countLines :: String -> IO Int
countLines s = do
    txt <- readFile s;
    return (count (== (chr 0xA)) txt)

data DirTree a = DirTree a [DirTree a] | DTFile a deriving (Eq, Show)

instance Functor DirTree where
    fmap f (DirTree x ents) = DirTree (f x) (fmap (fmap f) ents)
    fmap f (DTFile x)       = DTFile (f x)

foldPathBy :: (a -> a -> Bool) -> (b -> DirTree a -> b) -> b -> DirTree a -> [a] -> [b]
foldPathBy eq f s n@(DirTree x _)  [x']    | x `eq` x' = [f s n]
foldPathBy eq f s n@(DTFile x)     [x']    | x `eq` x' = [f s n]
foldPathBy eq f s n@(DirTree x cs) (x':xs) | x `eq` x' = concatMap (\n' -> foldPathBy eq f (f s n) n' xs) cs
foldPathBy _  _ _ _                _                   = []

followPathBy :: (a -> a -> Bool) -> DirTree a -> [a] -> Maybe (DirTree a)
followPathBy eq n p = justFirst (foldPathBy eq (const id) undefined n p) where

followPath :: Eq a => DirTree a -> [a] -> Maybe (DirTree a)
followPath = followPathBy (==)

fileExists :: DirTree String -> String -> Bool
fileExists t fpath = isJust (followPathBy (\s s' -> upper s == upper s') t (csplit fpath "/"))

fileLiteral :: DirTree String -> String -> String -> Maybe String
fileLiteral t baseDir fpath = justFirst (foldPathBy eq tfn "" t (csplit (mustEndWith baseDir "/" ++ fpath) "/")) where
    eq n n' = fpart n == fpart n'
    fpart n = upper (fst (lsplit n "."))
    tfn pfx (DirTree x _) = accPfx pfx x
    tfn pfx (DTFile x)    = accPfx pfx x
    accPfx "" x = x
    accPfx x' x = x' ++ "/" ++ x

fileLiterals :: DirTree String -> [String]
fileLiterals t = accumPath "" t where
    accumPath pfx (DirTree x cs) = concatMap (accumPath (accumPfx pfx x)) cs
    accumPath pfx (DTFile x)     = [accumPfx pfx x]
    accumPfx "" x = x
    accumPfx x' x = x' ++ "/" ++ x

projectFiles :: String -> IO (DirTree String)
projectFiles sdir = do
    ents  <- getDirectoryContents sdir;
    ents' <- mapM (gatherFiles ents) ents;
    return (DirTree (snd (rsplit sdir "/")) [ent | ent <- concat ents', not (emptyEnt ent)])
    where
        emptyEnt (DirTree _ []) = True
        emptyEnt _              = False
        gatherFiles _ "."  = return []
        gatherFiles _ ".." = return []
        gatherFiles fs fname = do
            let path           = mustEndWith sdir "/" ++ fname;
            let (name, ext)    = rsplit fname ".";
            let ext'           = upper ext;
            let isHappyGenFile = (upper $ name ++ ".y") `elem` (map upper fs);
            
            isDir <- doesDirectoryExist path;
            case (isDir, upper ext) of
                (True,_)                      -> do { t <- projectFiles path; return [t] }
                (_,"Y")                       -> return [DTFile fname]
                (_,"HS") | not isHappyGenFile -> return [DTFile fname]
                _                             -> return []

projectFileDependencies :: DirTree String -> String -> String -> IO [String]
projectFileDependencies t dir fpath = do
    txt <- readFile fpath;
    let imports   = [readImport line | line <- lines txt, isImport line];
    let locipaths = [imp | imp <- imports, validImport imp];
    return locipaths
    where
        isImport    line = fst (lsplit line " ") == "import"
        readImport  line = modName (snd (lsplit line " ")) where
            modName m | isPrefix "qualified" m = (csplit m " ") !! 1
            modName m                          = m
        validImport imp  = isJust x && fileExists t x' where
            x       = importFile t dir imp
            Just x' = x

importFile :: DirTree String -> String -> String -> Maybe String
importFile t dir imp = fileLiteral t dir (cdelim (csplit imp ".") "/")

fileModule :: String -> String -> String
fileModule dir file = replace "/" "." (fst (rsplit (drop (length (mustEndWith dir "/")) file) "."))

fileModuleDef :: String -> String -> IO String
fileModuleDef dir file = do
    txt <- readFile file;
    let mdefs = [readModule line | line <- lines txt, isModDef line];
    return (unjust (justFirst mdefs))
    where
        isModDef   line = fst (lsplit line " ") == "module"
        readModule line = fst (lsplit (snd (lsplit line " ")) " ")
        unjust (Just x) = x
        unjust Nothing  = fileModule dir file

projectDependencies :: DirTree String -> String -> IO [(String, [String])]
projectDependencies t dir = do
    let files = fileLiterals t;
    mapM fileDeps files;
    where
        fileDeps f = do
            deps <- projectFileDependencies t dir f;
            fm   <- fileModuleDef dir f;
            return (fm, deps)

projectDepsGraphDef :: String -> [String] -> [(String, [String])] -> String
projectDepsGraphDef gname ignoreModules deps =
    "digraph G {\n" ++
    "   label = \"" ++ gname ++ "\";\n" ++
    concatMap subgraphDesc (groupBy (first . modPath) deps'') ++
    concatMap edgeDefs deps' ++
    "}\n"
    where
        moduleSeq m     = csplit m "."
        ignoredModule m = any (\p -> isPrefix (map upper p) (map upper (moduleSeq m))) (map moduleSeq ignoreModules)
        modPath (p, _, _) = p
        deps'  = [(m, filter (not . ignoredModule) es) | (m, es) <- deps, not (ignoredModule m)]
        deps'' = [(moduleSeq m, m, es) | (m, es) <- deps']
        subgraphDesc (mod, ns) =
            "   subgraph cluster_" ++ mod ++ "{\n" ++
            "      label = \"" ++ mod ++ "\";\n\n" ++
            concatMap subgraphDesc (nextSubgraphs ns) ++
            concatMap nodeDef tg ++
            "   }\n"
            where
                tg = finalGraph ns
        nextSubgraphs ns = groupBy (first . modPath) [(p':p'':ps', m, es) | (_:p':p'':ps', m, es) <- ns]
        finalGraph ns = [(m, es) | ([_,_], m, es) <- ns]
        nodeDef  (m, _)  = "      " ++ nodeName m ++ " [label=\"" ++ leastMod m ++ "\"];\n"
        nodeName m       = replace "\\." "_" m
        leastMod m       = snd (rsplit m ".")
        edgeDefs (m, es) = concatMap (edgeDef m) es
        edgeDef  m m'    = "   " ++ nodeName m ++ " -> " ++ nodeName m' ++ ";\n"

projectDependencyDiagram :: String -> [String] -> String -> IO ()
projectDependencyDiagram dir ignoreModules gname = do
    fileTree <- projectFiles dir;
    pdeps    <- projectDependencies fileTree dir;
    writeFile (gname ++ ".dot") (projectDepsGraphDef gname ignoreModules pdeps);
    return ()

projectDependencyDiagramImage :: String -> [String] -> String -> IO ()
projectDependencyDiagramImage dir ignoreModules gname = do
    let file = gname ++ ".dot";
    fileTree <- projectFiles dir;
    pdeps    <- projectDependencies fileTree dir;
    writeFile file (projectDepsGraphDef gname ignoreModules pdeps);
    h <- runProcess "dot" [file, "-Tgif", "-o" ++ first (rsplit file ".") ++ ".gif"] Nothing Nothing Nothing Nothing Nothing;
    waitForProcess h;
    removeFile file;

projectReport :: String -> IO ()
projectReport dir = do
    fileTree <- projectFiles dir;
    fileTree' <- computeLen dir fileTree;
    printCTree 0 fileTree'
    where
        computeLen pfx (DirTree s ents) = do
            let dir = pfx ++ "/" ++ s;
            ents' <- mapM (computeLen dir) ents;
            return (DirTree (s, dir, sum [s | (_, _, s) <- map nodeVal ents']) ents')
        computeLen pfx (DTFile s) = do
            let dir = pfx ++ "/" ++ s;
            lc <- countLines dir;
            return (DTFile (s, dir, lc))
        nodeVal (DirTree x _) = x
        nodeVal (DTFile x)    = x
        
        printCTree idt (DirTree (s, dir, lc) ents) = do
            putStrLn (indent idt ++ "+ " ++ s ++ " (total): " ++ show lc);
            let ents' = reverse $ List.sortBy (\n1 n2 -> compare (third $ nodeVal n1) (third $ nodeVal n2)) ents;
            forM_ ents' (printCTree (idt+1))
        printCTree idt (DTFile (s, dir, lc)) = do
            putStrLn (indent idt ++ "- " ++ s ++ ": " ++ show lc) 
            
        indent n = take n (repeat ' ')

justFirst :: [a] -> Maybe a
justFirst []    = Nothing
justFirst (x:_) = Just x
