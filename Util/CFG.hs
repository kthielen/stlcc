{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ImpredicativeTypes #-}

module Util.CFG where
import Util.String
import Util.Sequence
import Util.Pointed
import Util.State
import Util.Num
import Util.Graphviz

import Data.List
import Data.Char
import Control.Monad.State

{-
	Basic block construction for generic instruction sequences
        (plus definitions of assembly code statements and abstract machine statements as generic instructions)
-}
data CFlowTy = NoCFlow | LabelDef String | JumpSrc [String]

class (Eq i, Show i) => Instruction i where
    inst_type :: i -> CFlowTy
    label     :: String -> i
    jump      :: String -> i
    invert    :: i -> i

data Instruction i => BBlock i = BBlock [i] Int Int Int deriving Show
type CFG i = [BBlock i]

mapCFG :: (Instruction i, Instruction j) => (i -> j) -> CFG i -> CFG j
mapCFG f bs = [BBlock (map f is) n m o | BBlock is n m o <- bs]

concatMapCFG :: (Instruction i, Instruction j) => (i -> [j]) -> CFG i -> CFG j
concatMapCFG f bs = [BBlock (concatMap f is) n m o | BBlock is n m o <- bs]

instance Instruction i => Eq (BBlock i) where
	(BBlock _ n _ _) == (BBlock _ n' _ _) = n == n'

instance Instruction i => Ord (BBlock i) where
	compare (BBlock _ _ m _) (BBlock _ _ m' _) = compare m m'

build_cfg :: Instruction i => [i] -> State Int (CFG i)
build_cfg insts = do { g <- accum 0 True [] insts; return $ orderTrace g } where
    accum _ _ bs [] = return bs
    accum n True bs (i:is) | is_label i = accum (n+1) False ((BBlock [i] n 0 0):bs) is
    accum n True bs is = do
        lbl <- fresh (label . prefix "bb");
        accum (n+1) False ((BBlock [lbl] n 0 0):bs) is
    accum n False (b:bs) (i:is) | is_label i = accum n False (b:bs) ((jump $ ex_lbl $ inst_type i):i:is)
    accum n False (b:bs) (i:is) | is_jump i  = accum n True ((append_inst i b):bs) is
    accum n False (b:bs) (i:is)              = accum n False ((append_inst i b):bs) is

orderTrace :: Instruction i => CFG i -> CFG i
orderTrace [] = []
orderTrace bs = concat $ unfold getTrace restBs endBs (0, [], [entry], bs') where
    (entry:bs') = closePointedPfx $ findBy ((==0) . block_id) bs
    
    getTrace (_, tr, _,  _)  = tr
    restBs   (n, _,  lb, rb) = nextTrace n lb rb
    endBs    (_, _,  lb, _)  = lb == []

nextTrace :: Instruction i => Int -> CFG i -> CFG i -> (Int, CFG i, CFG i, CFG i)
nextTrace n [] _ = (n, [], [], [])
nextTrace n [b] [] = (n+block_length b, [set_block_order b n], [], [])
nextTrace n (b:bs) availBs = tryOrder (block_dests b) where
    stepOrd = n + block_length b
    
    tryOrder [lbl]       | isDef lbl  = ordStep id lbl []
    tryOrder [flbl,tlbl] | isDef tlbl = ordStep id tlbl [flbl]
    tryOrder [flbl,tlbl] | isDef flbl = ordStep block_invjump flbl []
    tryOrder lbls = (stepOrd, [set_block_order b n], bs ++ bs', availBs') where
        (bs', availBs') = extractRefBlocks lbls availBs
    
    ordStep f lbl elbls = (n', (set_block_order (f b) n):bs''', lbs, availBs''') where
        (bs',  availBs')             = reorder lbl
        (bs'', availBs'')            = extractRefBlocks elbls availBs'
        (n', bs''', lbs, availBs''') = nextTrace stepOrd (bs' ++ bs'') availBs''
    
    reorder lbl | isLiveDef lbl = (closePointedPfx (findNamedBlock lbl bs), availBs)
    reorder lbl = (pointed i : bs, cutClose i) where
        i = findNamedBlock lbl availBs

    isBDef bs' lbl = any (== lbl) (map block_name bs')
    isLiveDef      = isBDef bs
    isAvailDef     = isBDef availBs
    isDef      lbl = isLiveDef lbl || isAvailDef lbl
    
    findNamedBlock   lbl  bs = findBy ((== lbl) . block_name) bs
    extractRefBlocks lbls bs = partition ((`elem` lbls) . block_name) bs
	
serialize_cfg :: Instruction i => CFG i -> [i]
serialize_cfg bbs = concat $ map block_insts $ fixup_blocks $ sort bbs where
    fixup_blocks bs = zipWith fixup_block bs ((tail bs)++[null_block])
    fixup_block b b' | falls_through (block_dests b) (block_name b') = strip_mjump b
    fixup_block b b'                                                 = add_false_jump b (block_dests b)
    falls_through [lbl] lbl' | lbl == lbl' = True
    falls_through (_:lbl:_) lbl' | lbl == lbl' = True
    falls_through _         _                  = False
    strip_mjump b | (length (block_dests b)) == 1 = block_cutjump b
    strip_mjump b                                 = b
    add_false_jump b (_:lbl:_) = append_inst (jump lbl) b
    add_false_jump b _         = b
    null_block = BBlock [] (-1) (-1) (-1)
    
cfg_node :: Instruction i => CFG i -> Int -> i
cfg_node bs idx = (block_insts b') !! (idx - block_order b') where
    b' = find_inst_block bs idx

node_count :: Instruction i => CFG i -> Int
node_count bs = sum (map (length . block_insts) bs)

node_succ :: Instruction i => CFG i -> Int -> [Int]
node_succ bs idx = find_succ b' idx' where
    find_succ b i | (i+1) < block_length b = [block_order b+i+1]
    find_succ b _                          = [block_order (named_block lbl bs) | lbl <- block_dests b]
    b'                                     = find_inst_block bs idx
    idx'                                   = idx - block_order b'

node_pred :: Instruction i => CFG i -> Int -> [Int]
node_pred bs idx = find_pred b' idx' where
    find_pred b i | i > 0 = [block_order b+i-1]
    find_pred b _         = [block_order b' + block_length b' - 1| b' <- bs, (block_name b) `elem` (block_dests b')]
    b'                    = find_inst_block bs idx
    idx'                  = idx - block_order b'
    
find_inst_block :: Instruction i => CFG i -> Int -> BBlock i
find_inst_block (b:bs) idx | withinLen idx (block_order b) (block_length b) = b
find_inst_block (_:bs) idx                                                  = find_inst_block bs idx
find_inst_block _      idx                                                  = error ("Invalid instruction index: " ++ show idx)

append_inst :: Instruction i => i -> BBlock i -> BBlock i
append_inst i (BBlock is n m o) = BBlock (is++[i]) n m o

block_insts :: Instruction i => BBlock i -> [i]
block_insts (BBlock is _ _ _) = is

blockNumberedInsts :: Instruction i => BBlock i -> [(Int, i)]
blockNumberedInsts (BBlock is _ m _) = zip (iterate (+1) m) is

block_length :: Instruction i => BBlock i -> Int
block_length b = length $ block_insts b

block_name :: Instruction i => BBlock i -> String
block_name (BBlock (i:is) _ _ _) = ex_lbl (inst_type i)
block_name (BBlock []     _ _ _) = "empty"

renameBlock :: Instruction i => BBlock i -> String -> BBlock i
renameBlock (BBlock (_:is) n m o) s = BBlock (label s : is) n m o
renameBlock (BBlock []     n m o) s = BBlock [label s] n m o

renameNamedBlock :: Instruction i => String -> String -> BBlock i -> BBlock i
renameNamedBlock s s' bb | block_name bb == s = renameBlock bb s'
renameNamedBlock _ _  bb                      = bb

block_id :: Instruction i => BBlock i -> Int
block_id (BBlock _ n _ _) = n

block_order :: Instruction i => BBlock i -> Int
block_order (BBlock _ _ m _) = m

set_block_order :: Instruction i => BBlock i -> Int -> BBlock i
set_block_order (BBlock is n _ o) m = BBlock is n m o

block_offset :: Instruction i => BBlock i -> Int
block_offset (BBlock _ _ _ o) = o

block_dests :: Instruction i => BBlock i -> [String]
block_dests (BBlock [] _ _ _) = []
block_dests (BBlock is _ _ _) = ex_dsts (inst_type (last is)) where
    ex_dsts (JumpSrc lbls)    = lbls
    ex_dsts _                 = []

block_invjump :: Instruction i => BBlock i -> BBlock i
block_invjump (BBlock is n m o) = BBlock is' n m o where
    (li:ris) = reverse is
    is'      = reverse ((invert li):ris)
    
block_cutjump :: Instruction i => BBlock i -> BBlock i
block_cutjump (BBlock is n m o) = BBlock is' n m o where
    (_:ris) = reverse is
    is'     = reverse ris
    
named_block :: Instruction i => String -> CFG i -> BBlock i
named_block lbl bs = bs !! (at lbl $ map block_name bs)

block_by_id :: Instruction i => Int -> CFG i -> BBlock i
block_by_id n bs = bs !! (at n $ map block_id bs)

push_instruction :: Instruction i => BBlock i -> i -> BBlock i
push_instruction (BBlock is m n o) i = BBlock (insert is) m n o where
    insert [] = [i]
    insert (i':is) | is_label i' = i' : insert is
    insert (i':is)               = i  : i' : is
    
first_inst :: Instruction i => BBlock i -> Maybe i
first_inst (BBlock is _ _ _) = find is where
    find (i:is) | is_label i = find is
    find (i:_)               = Just i
    find []                  = Nothing

is_first_inst :: Instruction i => BBlock i -> i -> Bool
is_first_inst bb i = match (first_inst bb) where
    match (Just i') = i' == i
    match Nothing = False

rpush_instruction :: Instruction i => BBlock i -> i -> BBlock i
rpush_instruction (BBlock is m n o) i = BBlock (reverse (insert (reverse is))) m n o where
    insert [] = [i]
    insert (i':is) | is_jump i' = i' : insert is
    insert (i':is)              = i : i' : is

last_inst :: Instruction i => BBlock i -> Maybe i
last_inst (BBlock is _ _ _) = Just (last is)

last_inst_ord :: Instruction i => BBlock i -> (Int, i)
last_inst_ord bb = (block_order bb + length is - 1, last is) where
    is = block_insts bb

is_label :: Instruction i => i -> Bool
is_label i = il (inst_type i) where
	il (LabelDef _) = True
	il _            = False

ex_lbl :: CFlowTy -> String
ex_lbl (LabelDef lbl) = lbl

is_jump :: Instruction i => i -> Bool
is_jump i = ij (inst_type i) where
	ij (JumpSrc _) = True
	ij _           = False
    
{-
    Graphviz serialization of control flow graphs
-}
cfgDiagram :: Instruction i => String -> CFG i -> String
cfgDiagram name bs =
    "  subgraph cluster_" ++ name' ++ " {\n" ++
    "    label=\"" ++ name' ++ "\";\n" ++
    concat (map block_desc bs) ++
    concat (map edge_desc edges) ++
    "  }\n"
    where
        name' = gvEsc name
        block_desc b =
            "    \"" ++ name' ++ "_block" ++ show (block_id b) ++ "\" " ++
            "[" ++
                "style=\"filled, bold\" " ++ 
                "penwidth=1 " ++
                "fillcolor=\"white\" " ++
                "fontname=\"Courier New\" " ++ 
                "shape=\"Mrecord\" " ++
                "label=<" ++ inst_table b ++ ">" ++
            "];\n"
        edge_desc (src, dst) =
            "    \"" ++ name' ++ "_block" ++ show src ++ "\" -> \"" ++ name' ++ "_block" ++ show dst ++ "\" " ++
            "[" ++
                "penwidth=1 " ++
                "fontsize=28 " ++
                "fontcolor=\"black\" " ++
            "];\n"
        edges = [(block_id b, block_id $ named_block d bs) | b <- bs, d <- block_dests b]
        inst_table b =
            "<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\">" ++
            concat ["<tr><td align=\"left\">" ++ show idx ++ ": " ++ gvEsc (show i) ++ "</td></tr>" | (i, idx) <- zip (block_insts b) (map (+(block_order b)) [0..])] ++
            "</table>"