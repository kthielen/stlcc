{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

{-
    Graphs suitable for representing program units
        From such graphs, we can derive dominator trees
-}
module Util.Graph where
import Util.Num
import Util.String
import Util.Sequence
import Util.Tuples

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import System.Process
import Directory

class (Show n, Ord n) => Node n where
    nodeName :: n -> String
    nodeDesc :: n -> String
    nodeDesc n = "[label=\"" ++ nodeName n ++ "\"]"

class Node n => Graph g n | g -> n where
    graphName    :: g -> String
    entry        :: g -> n
    nodes        :: g -> [n]
    successors   :: g -> n -> [n]
    predecessors :: g -> n -> [n]
    graphName = nodeName . entry

{-
    Find the depth-first spanning tree for a graph
-}
type NodeDepth   i = Map.Map i Int
type DepthNode   i = Map.Map Int i
type DFSChildren i = Map.Map i [i]
type DFSParent   i = Map.Map i i
type DFSData i = (NodeDepth i, DepthNode i, DFSChildren i, DFSParent i)

depthFirstSpanningTree :: Graph g n => g -> DFSData n
depthFirstSpanningTree g = snd $ dfs e 1 (successors g e) (initDFS e) where
    e   = entry g
    
    dfs p x [] d = (x, d)
    dfs p x (n:ns) d | dfsIsDefined d n = dfs p x ns d
    dfs p x (n:ns) d = dfs p x' ns d' where
        (x', d') = dfs n (x+1) (successors g n) (insertDFS d p n x)

initDFS :: Ord n => n -> DFSData n
initDFS i = (Map.insert i 0 Map.empty, Map.insert 0 i Map.empty, Map.empty, Map.empty)

insertDFS :: Ord n => DFSData n -> n -> n -> Int -> DFSData n
insertDFS (m, r, t, f) p c n = (Map.insert c n m, Map.insert n c r, appendMapVal t p c, Map.insert c p f)

dfsNum :: Ord n => DFSData n -> n -> Maybe Int
dfsNum (m, _, _, _) i = Map.lookup i m

dfsNode :: Ord n => DFSData n -> Int -> Maybe n
dfsNode (_, r, _, _) d = Map.lookup d r

dfsMaxDepth :: Ord n => DFSData n -> Int
dfsMaxDepth (_, r, _, _) = fst (Map.findMax r)

dfsParent :: Ord n => DFSData n -> n -> Maybe n
dfsParent (_, _, _, f) c = Map.lookup c f

dfsChildren :: Ord n => DFSData n -> n -> [n]
dfsChildren (_, _, t, _) p = mlist (Map.lookup p t)

dfsIsDefined :: Ord n => DFSData n -> n -> Bool
dfsIsDefined d i = isJust (dfsNum d i)

isDFSEdge :: Ord n => DFSData n -> n -> n -> Bool
isDFSEdge d p c = pmatch (dfsParent d c) where
    pmatch (Just p') | p == p' = True
    pmatch _                   = False

{-
    Find the dominator tree for a graph
-}
type SemiDom      i = Map.Map i i
type AncestorLink i = Map.Map i i
type IDom         i = Map.Map i i
type EquivDom     i = Map.Map i i
type RPathNodes   i = Map.Map i [i]
type DomState i = (SemiDom i, AncestorLink i, IDom i, EquivDom i, RPathNodes i)

type DomTree n = (IDom n, Map.Map n (Set.Set n))

dominators :: Graph g n => g -> DomTree n
dominators g = dominatorTree (substEquivDoms d dfs) where
    dfs = depthFirstSpanningTree g
    d   = domState g dfs

dominatorTree :: Ord n => IDom n -> DomTree n
dominatorTree d = (d, foldl insDomTreeEdge Map.empty (Map.toList d)) where
    insDomTreeEdge t (c, p) = Map.insert p (insDomSet c (Map.lookup p t)) t
    insDomSet c (Just s) = Set.insert c s
    insDomSet c Nothing  = Set.singleton c

substEquivDoms :: Ord n => DomState n -> DFSData n -> IDom n
substEquivDoms d dfs = doms where
    (_, _, doms, _, _) = foldl subst d [node n | n <- upto 1 (dfsMaxDepth dfs)]
    node n             = uj (dfsNode dfs n)
    
    subst d i | isJust (equivDom d i) = linkIDom d i di where
        Just li = equivDom d i
        di      = linkedIDom d li
    subst d _ = d

domState :: Graph g n => g -> DFSData n -> DomState n
domState g dfs = foldl idomStep initDomState [node n | n <- downto (dfsMaxDepth dfs) 1] where
    dfsn   i  = uj (dfsNum    dfs i)
    node   n  = uj (dfsNode   dfs n)
    parent i  = uj (dfsParent dfs i)
    
    idomStep d i = identifyIDoms (closeDomStateNode d' i p s) p where
        p          = parent i
        (d', c:cs) = foldl semiCandidate (d, []) (predecessors g i)
        s          = seqMinBy dfsn c cs
        semiCandidate (d', ss) p | dfsn p <= dfsn i = (d', p:ss)
        semiCandidate (d', ss) p                    = (d'', (semiDominator d'' p'):ss) where (d'', p') = ancestorWithLowestSemi d' p

initDomState :: Ord n => DomState n
initDomState = (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

identifyIDoms :: Ord n => DomState n -> n -> DomState n
identifyIDoms d i = clearRPathNodes d' i where
    d' = foldl classifyIDoms d (rpathNodes d i)
    classifyIDoms d v = chooseIDom v y where
        (d', y) = ancestorWithLowestSemi d v
        chooseIDom v y | semiDominator d' v == semiDominator d' y = linkIDom     d' v i
        chooseIDom v y                                            = linkEquivDom d' v y

closeDomStateNode :: Ord n => DomState n -> n -> n -> n -> DomState n
closeDomStateNode d i p s = d''' where
    d'   = linkSemiDominator d   i s
    d''  = appendRPathNodes  d'  s i
    d''' = linkAncestor      d'' p i

semiDominator :: Ord n => DomState n -> n -> n
semiDominator (semi, _, _, _, _) i = uj (Map.lookup i semi)

linkSemiDominator :: Ord n => DomState n -> n -> n -> DomState n
linkSemiDominator (s, a, d, e, r) i v = (Map.insert i v s, a, d, e, r)

rpathNodes :: Ord n => DomState n -> n -> [n]
rpathNodes (_, _, _, _, r) i = mlist (Map.lookup i r)

appendRPathNodes :: Ord n => DomState n -> n -> n -> DomState n
appendRPathNodes (s, a, d, e, r) i c = (s, a, d, e, appendMapVal r i c)

clearRPathNodes :: Ord n => DomState n -> n -> DomState n
clearRPathNodes (s, a, d, e, r) i = (s, a, d, e, Map.insert i [] r)

ancestorWithLowestSemi :: Ord n => DomState n -> n -> (DomState n, n)
ancestorWithLowestSemi d v = (d, seqMinBy (semiDominator d) v (ancestors d v))

ancestor :: Ord n => DomState n -> n -> Maybe n
ancestor (_, a, _, _, _) i = Map.lookup i a

ancestors :: Ord n => DomState n -> n -> [n]
ancestors d v = cont (ancestor d v) where
    cont (Just p) | isJust (ancestor d p) = p : ancestors d p
    cont _ = []

linkAncestor :: Ord n => DomState n -> n -> n -> DomState n
linkAncestor (s, a, d, e, r) p c = (s, Map.insert c p a, d, e, r)

linkIDom :: Ord n => DomState n -> n -> n -> DomState n
linkIDom (s, a, d, e, r) i dom = (s, a, Map.insert i dom d, e, r)

linkedIDom :: Ord n => DomState n -> n -> n
linkedIDom (_, _, d, _, _) n = uj (Map.lookup n d)

idom :: Ord n => DomTree n -> n -> n
idom (m, _) n = uj (Map.lookup n m)

idomChildren :: Ord n => DomTree n -> n -> Set.Set n
idomChildren (_, t) n = mset (Map.lookup n t) where
    mset (Just s) = s
    mset Nothing  = Set.empty

hasDominator :: Ord n => DomTree n -> n -> Bool
hasDominator (m, _) n = isJust (Map.lookup n m)

isDominatedBy :: Ord n => DomTree n -> n -> n -> Bool
isDominatedBy d c _ | not (hasDominator d c) = False
isDominatedBy d c p | idom d c == p          = True
isDominatedBy d c p                          = isDominatedBy d (idom d c) p

equivDom :: Ord n => DomState n -> n -> Maybe n
equivDom (_, _, _, e, _) i = Map.lookup i e

linkEquivDom :: Ord n => DomState n -> n -> n -> DomState n
linkEquivDom (s, a, d, e, r) v y = (s, a, d, Map.insert v y e, r)

{-
    Find the dominance frontier of each node in a graph
-}
type DominanceFrontier n = Map.Map n (Set.Set n)

dominanceFrontier :: Ord n => DominanceFrontier n -> n -> Set.Set n
dominanceFrontier d n = set (Map.lookup n d) where
    set (Just s) = s
    set Nothing  = Set.empty

dominanceFrontiers :: Graph g n => g -> DomTree n -> DominanceFrontier n
dominanceFrontiers g d = nodeFrontier g d (entry g)

nodeFrontier :: Graph g n => g -> DomTree n -> n -> DominanceFrontier n
nodeFrontier g d n = Map.insert n (Set.union s s') df where
    s  = Set.fromList [y | y <- successors g n, hasDominator d y, idom d y /= n]
    s' = Set.fromList [y | Just ys <- [Map.lookup c df | c <- cs], y <- Set.toList ys, not (isDominatedBy d y n)]
    df = Map.unions [nodeFrontier g d y | y <- cs]
    cs = Set.toList (idomChildren d n)

{-
    Graphviz serialization
-}
graphToDot :: Graph g n => g -> String
graphToDot g =
    "digraph G {\n" ++
    "   label = \"" ++ graphName g ++ "\";\n" ++
    concat (map defNode ns) ++ 
    concat (map defEdges ns) ++
    "}"
    where
        ns         = nodes g
        defNode  n = "   \"" ++ nodeName n ++ "\" " ++ nodeDesc n ++ ";\n"
        defEdges n = concat ["   \"" ++ nodeName n ++ "\" -> \"" ++ nodeName n' ++ "\";\n" | n' <- successors g n]

graphWithDFSToDot :: Graph g n => g -> String
graphWithDFSToDot g =
    "digraph G {\n" ++
    "   label = \"" ++ graphName g ++ "\";\n" ++
    concat (map defNode ns) ++ 
    concat (map defEdges ns) ++
    "}"
    where
        dfs        = depthFirstSpanningTree g
        ns         = nodes g
        defNode  n = "   \"" ++ nodeName n ++ "\" [label=\"" ++ nodeName n ++ " (" ++ show (uj $ dfsNum dfs n) ++ ")\"];\n"
        defEdges n = concat ["   \"" ++ nodeName n ++ "\" -> \"" ++ nodeName n' ++ "\" " ++ dfsEdgeDesc n n' ++ ";\n" | n' <- successors g n]
        dfsEdgeDesc n n' = choose (isDFSEdge dfs n n') where
            choose True  = "[style=bold]"
            choose False = "[style=dotted]"

graphWithDomToDot :: Graph g n => g -> String
graphWithDomToDot g =
    "digraph G {\n" ++
    domSubGraph (node 0) ++
    "}"
    where
        ns     = nodes g
        d      = dominators g
        dfs    = depthFirstSpanningTree g
        node n = uj (dfsNode dfs n)
        
        domSubGraph i =
            let children = Set.toList (idomChildren d i) in
            "      \"" ++ nodeName i ++ "\" " ++ nodeDesc i ++ ";\n" ++
            concat [domEdgeDesc i i' | i' <- children] ++
            concat (map domSubGraph children)
            
        domEdgeDesc i i' = "      \"" ++ nodeName i ++ "\" -> \"" ++ nodeName i' ++ "\""
{-
graphWithDomToDot g =
    "digraph G {\n" ++
    domSubGraph (node 0) ++
    "}"
    where
        ns     = nodes g
        d      = dominators g
        dfs    = depthFirstSpanningTree g
        node n = uj (dfsNode dfs n)
        
        domSubGraph i =
            let children = Set.toList (idomChildren d i) in
            "   subgraph cluster_" ++ nodeName i ++ " {\n" ++
            "      \"" ++ nodeName i ++ "\" " ++ nodeDesc i ++ ";\n" ++
            concat [domEdgeDesc i i' | i' <- children] ++
            concat (map domSubGraph children) ++
            "   }\n"
            
        domEdgeDesc i i' = "      \"" ++ nodeName i ++ "\" -> \"" ++ nodeName i' ++ "\""
-}

graphToDotImage :: Graph g n => (g -> String) -> g -> String -> IO ()
graphToDotImage g2d g dir = do
    let file = mustEndWith dir "/" ++ graphName g ++ ".dot";
    
    writeFile file (g2d g);
    h <- runProcess "dot" [file, "-Tgif", "-o" ++ first (rsplit file ".") ++ ".gif"] Nothing Nothing Nothing Nothing Nothing;
    waitForProcess h;
    removeFile file;

graphImage :: Graph g n => g -> String -> IO ()
graphImage = graphToDotImage graphToDot

graphDFSImage :: Graph g n => g -> String -> IO ()
graphDFSImage = graphToDotImage graphWithDFSToDot

graphDomImage :: Graph g n => g -> String -> IO ()
graphDomImage = graphToDotImage graphWithDomToDot
