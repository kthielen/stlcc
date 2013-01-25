
module ASM.Liveness where
import ASM.Type
import ASM.Term
import ASM.SSA

import Util.Annotated
import Util.CFG
import Util.Graph
import Util.Sequence
import Util.Tuples
import Util.Recursion

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import Data.Char

{-
	Liveness of variables at each instruction location in a control-flow graph
-}
type RegSet   a = Set.Set (Reg a)
type Liveness a = Map.Map Int (RegSet a, RegSet a)

liveness :: (Annotation a, Eq a) => CFG (Inst a) -> Liveness a
liveness bs = fixedPoint lstep Map.empty where
    lstep   lv   = foldl lsstep lv $ reverse [0 .. (node_count bs)-1]
    lsstep  lv n = Map.insert n (ins_in lv n, ins_out lv n) lv
    ins_out lv n = Set.union (live_out lv n) (Set.unions (map (live_in lv) (node_succ bs n)))
    ins_in  lv n = Set.union uses (Set.difference (live_out lv n) defs) where
        uses = Set.fromList $ inst_reg_uses i
        defs = Set.fromList $ inst_reg_defs i
        i    = cfg_node bs n
    
live_in :: Liveness a -> Int -> RegSet a
live_in lv n = rselect (Map.lookup n lv) where
    rselect (Just (li, _)) = li
    rselect Nothing        = Set.empty

live_out :: Liveness a -> Int -> RegSet a
live_out lv n = rselect (Map.lookup n lv) where
    rselect (Just (_, lo)) = lo
    rselect Nothing        = Set.empty

{-
	The interference-graph of a control-flow graph
-}

data InterferenceCategory = Interfere | MayCoalesce deriving (Show, Eq, Ord)
type InterferenceGraph a  = Map.Map (Reg a) (Bool, Map.Map (Reg a) InterferenceCategory)

initialIG :: (Annotation a, Eq a) => [Reg a] -> InterferenceGraph a
initialIG regs = addMachineRegs (Map.fromList [(retractedStdReg r, (True, Map.empty)) | r <- regs]) where
    addMachineRegs ig = foldl (add_edge Interfere) ig [(Reg (TPrim nullAnnotation "int") r, Reg (TPrim nullAnnotation "int") r') | r <- mregs, r' <- mregs, r /= r']

cfgRegRefs :: (Annotation a, Eq a) => CFG (Inst a) -> [Reg a]
cfgRegRefs = unique . concatMap (unique . concatMap inst_reg_refs . block_insts)

interference :: (Annotation a, Eq a) => CFG (Inst a) -> InterferenceGraph a
interference bs = foldl ifstep (initialIG (cfgRegRefs bs)) [0 .. (node_count bs)-1] where
    lv          = liveness bs
    ifstep ig n = add_edges ig n (cfg_node bs n)
    add_edges ig n i | is_reg_move i    = foldl (add_edge Interfere) ig' [(d, s) | d <- inst_reg_defs i, s <- Set.toList (live_in lv n), not (s `elem` (inst_reg_uses i))] where
        ig' = foldl (add_edge MayCoalesce) ig [(d, s) | d <- inst_reg_defs i, s <- inst_reg_uses i]
    add_edges ig n i                    = foldl (add_edge Interfere) ig  [(d, s) | d <- inst_reg_defs i, s <- Set.toList (live_out lv n)]

interferences :: InterferenceGraph a -> Reg a -> [(Reg a, InterferenceCategory)]
interferences ig s = either (\(_, ig') -> Map.toList ig') (const []) $ choice (Map.lookup s ig)

interferes :: InterferenceGraph a -> Reg a -> Reg a -> Bool
interferes ig s0 s1 = either (\(_, ig') -> intf ig') (const False) $ choice (Map.lookup s0 ig) where
    intf ig' = either is_interference (const False) $ choice (Map.lookup s1 ig')
    is_interference Interfere = True
    is_interference _         = False

add_edge :: InterferenceCategory -> InterferenceGraph a -> (Reg a, Reg a) -> InterferenceGraph a
add_edge _ ig (s0, s1) | s0 == s1 = ig
add_edge ity ig (s0, s1) = add_half (add_half ig s0 s1) s1 s0 where
    add_half ig s0 s1 = either add_half_ins (\() -> add_half_ins (True, Map.empty)) $ choice (Map.lookup s0 ig) where
        add_half_ins (f, sig) = Map.insert s0 (merge_ins ity (Map.lookup s1 sig)) ig where
            merge_ins Interfere   (Just _) = (f, Map.insert s1 Interfere sig)
            merge_ins MayCoalesce (Just x) = (f, Map.insert s1 x         sig)
            merge_ins x           Nothing  = (f, Map.insert s1 x         sig)

enable_var :: Bool -> InterferenceGraph a -> Reg a -> InterferenceGraph a
enable_var f ig s = upd (Map.lookup s ig) where
    upd (Just (f', _)) | f == f' = ig
    upd (Just (_, ig'))          = Map.insert s (f, ig') ig
    upd Nothing                  = ig
    
del_var :: InterferenceGraph a -> Reg a -> InterferenceGraph a
del_var = enable_var False

ins_var :: InterferenceGraph a -> Reg a -> InterferenceGraph a
ins_var = enable_var True

createVar :: InterferenceGraph a -> Reg a -> InterferenceGraph a
createVar ig r = ins (Map.lookup r ig) where
    ins (Just _) = ig
    ins Nothing  = Map.insert r (True, Map.empty) ig

killVar :: InterferenceGraph a -> Reg a -> InterferenceGraph a
killVar ig r = Map.map killVarRefs ig' where
    killVarRefs (enabled, m') = (enabled, Map.delete r m')
    ig' = Map.delete r ig

is_active_node :: InterferenceGraph a -> Reg a -> Bool
is_active_node ig s = either first (const False) $ choice $ Map.lookup s ig

is_move_node :: InterferenceGraph a -> Reg a -> Bool
is_move_node ig s0 = either (\(f, ig') -> f && flt (Map.toList ig')) (const False) $ choice (Map.lookup s0 ig) where
    flt ((s1, MayCoalesce):rs) = is_active_node ig s1
    flt (_:rs)                 = flt rs
    flt []                     = False

freeze_moves :: InterferenceGraph a -> Reg a -> InterferenceGraph a
freeze_moves ig s = either fm (const ig) $ choice (Map.lookup s ig) where
    fm (f, ig') = Map.insert s (f, Map.map (const Interfere) ig') ig
    
degree :: InterferenceGraph a -> Reg a -> Int
degree ig s = cnt (Map.lookup s ig) where
    cnt (Just (_, ig')) = count (is_active_node ig) [s | (s, _) <- Map.toList ig']
    cnt Nothing         = 0
    
precolored :: Reg a -> Bool
precolored (Reg _ r) = r `elem` mregs

subgraph_of :: InterferenceGraph a -> [Reg a] -> InterferenceGraph a
subgraph_of ig vs = Map.mapWithKey enableVar ig where
    enableVar v (_, hig) | v `elem` vs = (True,  hig)
    enableVar v (_, hig)               = (False, hig)

{-
    Interference diagrams for SSA CFGs
-}
ssaInterference :: (Annotation a, Eq a) => SSADefUse a -> CFG (SSAInst a) -> InterferenceGraph a
ssaInterference du cfg = foldlSSARegs calcRegInterference (initialIG (ssaDURegRefs du)) du where
    calcRegInterference m (vn, (ty, def, uses)) = fst varUseIG where
        varUseIG = foldl ssaLiveness (m, Set.empty) (instsWithLocs uses)
        
        ssaLiveness (m, v) (i, Phi ty' vn' vs) = ssaBlockLiveOut (m', v) (predecessors cfg (find_inst_block cfg i) !! (at vn vs)) where
            m' = add_edge MayCoalesce m (Reg ty vn, Reg ty' vn')
        ssaLiveness (m, v) (i, inst) = ssaInstLiveOut (m, v) (i, inst)
        
        ssaBlockLiveOut (m, v) bb | Set.member (block_id bb) v = (m, v)
        ssaBlockLiveOut (m, v) bb                              = ssaInstLiveOut (m, Set.insert (block_id bb) v) (last_inst_ord bb)
        
        ssaInstLiveIn (m, v) (i, op) = markAtStmt (is_first_inst bb' op) where
            bb' = find_inst_block cfg i
            markAtStmt True  = foldl ssaBlockLiveOut (m, v) (predecessors cfg bb')
            markAtStmt False = ssaInstLiveOut (m, v) (i - 1, cfg_node cfg (i - 1))

        ssaInstLiveOut (m, v) (i, op) = if not (null sd) then (m', v) else ssaInstLiveIn (m', v) (i, op) where
            vreg       = Reg ty vn
            m'         = addInterferences m defs
            (sd, defs) = List.partition (\(Reg _ vn') -> vn == vn') (ssaInstRegDefs op)
            addInterferences m regs = foldl (ssaAddEdge op) m [(vreg, reg) | reg <- regs]

        ssaAddEdge op m (domreg, defreg) = add_edge (ssaInterferenceCategory op domreg defreg) m (retractedStdReg domreg, retractedStdReg defreg)
        
    instsWithLocs uses = [(u, cfg_node cfg u) | u <- Set.toList uses]

ssaInterferenceCategory :: SSAInst a -> Reg a -> Reg a -> InterferenceCategory
ssaInterferenceCategory (Phi _ vn svs) (Reg _ vn') _ = if vn' `elem` svs then MayCoalesce else Interfere
ssaInterferenceCategory (SSAInst i)    domreg      _ = choose (is_reg_move i && domreg `elem` inst_reg_uses i) (ssaFakeAssignment i) where
    choose True _                              = MayCoalesce
    choose _    (Just (dr, sr)) | sr == domreg = MayCoalesce
    choose _    _                              = Interfere

{-
    Graphviz serialization of interference-graphs
-}
type RegColors = [(String, String)]

interferenceDiagram :: InterferenceGraph a -> RegColors -> Bool -> String
interferenceDiagram = interferenceDiagramWithWrapping "ig" "digraph G {\n   edge [dir=none];\n" "}\n"

interferenceSubDiagram :: InterferenceGraph a -> RegColors -> Bool -> String -> String
interferenceSubDiagram ig rc sdc n = interferenceDiagramWithWrapping n ("subgraph cluster_" ++ n ++ " {\n   label=\"" ++ n ++ "\";\n   edge [dir=none];\n") "}\n" ig rc sdc

interferenceDiagramWithWrapping :: String -> String -> String -> InterferenceGraph a -> RegColors -> Bool -> String
interferenceDiagramWithWrapping id pfx sfx ig rc showDCs =
    pfx ++
    concatMap nodeDef [r | (r, _) <- Map.toList ig, not (ignoreNode r), is_active_node ig r, degree ig r > 0] ++
    concat (map ig_edge ig_edges) ++
    sfx
    where
        rcM = Map.fromList rc
        nodeColor rn = either (\x->x) (const "white") (choice $ Map.lookup rn rcM)
        machineIFregs = unique [r' | (r, (True, ig')) <- Map.toList ig, not (isMachineReg r), (r', _) <- Map.toList ig', isMachineReg r']
        ignoreNode r = isMachineReg r && not (r `elem` machineIFregs)
        nodeDef r@(Reg _ rn) = "   \"" ++ rname r ++ "\" [style=filled, fillcolor=" ++ nodeColor rn ++ ", label=<" ++ rlabel r ++ ">];\n"
        ig_edges = unique [(canon (s0, s1), ity) | (s0, (_, ig')) <- Map.toList ig, (s1, ity) <- Map.toList ig', is_active_node ig s0, is_active_node ig s1, not (ignoreNode s0) && not (ignoreNode s1)]
        ig_edge ((s0, s1), Interfere)   = "   \"" ++ rname s0 ++ "\" -> \"" ++ rname s1 ++ "\";\n"
        ig_edge ((s0, s1), MayCoalesce) = "   \"" ++ rname s0 ++ "\" -> \"" ++ rname s1 ++ "\" [style=dotted];\n"
        canon p@(s0, s1) | s0 < s1 = p
        canon   (s0, s1)           = (s1, s0)
        rname   (Reg _ r)          = id ++ r
        rlabel  (Reg _ r)          = subnum r
        
        subnum ('(':cs) = if isNum p then "<font point-size=\"8\">" ++ p ++ "</font>" ++ subnum cs' else '(' : subnum cs where (p,cs') = toEP cs
        subnum (c:cs)   = c : subnum cs
        subnum []       = []
        isNum s         = all isDigit s
        toEP s          = (p, drop (length p + 1) s) where p = takeWhile (not . (== ')')) s
