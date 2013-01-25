{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.RegAllocation where
import AML.Term
import ASM.Term
import ASM.Type
import ASM.InstCFG
import ASM.Liveness

import Util.Annotated
import Util.CFG
import Util.Sequence
import Util.Tuples
import Util.Recursion
import Util.Graphviz

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import Control.Monad
import Control.Monad.State

{-
    Register allocation based on the fixpoint of graph simplification, register coalescence, move freezing, and optimistic register spilling
-}

k :: Int
k = length mregs

simplify :: InterferenceGraph a -> (InterferenceGraph a, [Reg a])
simplify ig = simplify_iter (ig, []) where
    simplify_iter (ig', s) = if s' == [] then (ig', s) else simplify_iter (ig'', s ++ s') where
        s'   = [t | (t, (f, _)) <- Map.toList ig', f == True, not (precolored t), not (is_move_node ig' t), degree ig' t < k]
        ig'' = foldl del_var ig' s'

coalesce :: InterferenceGraph a -> (InterferenceGraph a, [(Reg a, Reg a)])
coalesce ig = (ig', cs') where
    (ig', cs') = foldl cstep (ig, []) cs
    cstep   (ig, cs) (s0, s1) | should_coalesce ig s0 s1 = (coalesce_vars ig s0 s1, (s0, s1):cs)
    cstep p@(ig, cs) _                                   = p
    cs  = [(s0, s1) | (s0, (True, ig')) <- Map.toList ig, (s1, MayCoalesce) <- Map.toList ig', should_coalesce ig s0 s1]

coalesce_vars :: InterferenceGraph a -> Reg a -> Reg a -> InterferenceGraph a
coalesce_vars ig s0 s1 | precolored s1 = coalesce_vars ig s1 s0
coalesce_vars ig s0 s1                 = del_var ig' s1 where
    ig' = foldl (\ig (s, c) -> add_edge c ig (s0, s)) ig (interferences ig s1)

should_coalesce :: InterferenceGraph a -> Reg a -> Reg a -> Bool
should_coalesce ig s0 s1 = both_active && all is_acceptable [t | (t, Interfere) <- interferences ig s0] where
    is_acceptable t = not (active t) || interferes ig t s1 || degree ig t < k
    active r        = is_active_node ig r
    both_active     = active s0 && active s1

freeze :: InterferenceGraph a -> InterferenceGraph a
freeze ig = upd pvs where
    upd []    = ig
    upd (v:_) = freeze_moves ig v
    pvs       = [t | (t, (True, _)) <- Map.toList ig, not (precolored t), is_move_node ig t, degree ig t < k]
    
spill :: InterferenceGraph a -> (InterferenceGraph a, [Reg a])
spill ig = upd pvs where
    upd []    = (ig, [])
    upd (v:_) = (del_var ig v, [v])
    pvs       = [t | (t, (True, _)) <- Map.toList ig, not (precolored t), degree ig t >= k]
    
type Coalescences a = Map.Map (Reg a) (Reg a)
data SelectResult a = SpilledVars [Reg a] (Coalescences a) | RegisterAssignment (Coalescences a) deriving Eq
select :: InterferenceGraph a -> [Reg a] -> SelectResult a
select ig vs = pick spills where
    pick []                        = RegisterAssignment vm
    pick sps                       = SpilledVars sps (coalesce_spills ig sps)
    (_, vm, spills)                = foldl select_var (ig, Map.empty, []) vs
    select_var (ig', vm, spills) v = either (\c -> (ins_var ig' v, Map.insert v c vm, spills)) (const (ig', vm, v:spills)) $ choice (choose_color ig' v vm)

coalesce_spills :: InterferenceGraph a -> [Reg a] -> Coalescences a
coalesce_spills ig vs = second $ fixedPoint cstep (subgraph_of ig vs, Map.empty, vs) where
    cstep (ig', vm, vs') = foldl cs (ig', vm, []) vs'
    cs (ig', vm, vs') v  = coal (moves ig' v) where
        coal []     = (ig', vm, vs')
        coal (v':_) = (coalesce_vars ig' v v', Map.insert v v' vm, v:vs')
    moves ig v | not (is_active_node ig v) = []
    moves ig v                             = [t | (t, ity) <- interferences ig v, is_active_node ig t, ity == MayCoalesce]
    
choose_color :: InterferenceGraph a -> Reg a -> Coalescences a -> Maybe (Reg a)
choose_color ig v@(Reg ty s) vm = pick avail_colors where
    pick (x:_)   = Just x
    pick []      = Nothing
    avail_colors = [Reg ty r | r <- mregs, not (r `elem` colors)]
    colors       = [c | Just (Reg _ c) <- ncolors]
    ncolors      = [rlookup t | (t, _) <- interferences ig v, is_active_node ig t]
    rlookup r | precolored r = Just r
    rlookup r                = Map.lookup r vm
    
try_assign_registers :: Annotation a => InterferenceGraph a -> SelectResult a
try_assign_registers ig = merge_moves (select ig' ss) mvs where
    (ig', ss, mvs)          = concentricFixedPoint [simplify', coalesce', freeze', spill'] (ig, [], [])
    simplify' (ig, ss, mvs) = (ig', ss' ++ ss, mvs)         where (ig', ss')  = simplify ig
    coalesce' (ig, ss, mvs) = (ig', ss,        mvs' ++ mvs) where (ig', mvs') = coalesce ig
    freeze'   (ig, ss, mvs) = (ig', ss,        mvs)         where ig'         = freeze   ig
    spill'    (ig, ss, mvs) = (ig', ss ++ ss', mvs)         where (ig', ss')  = spill    ig

-- apply coalescing decisions so that coalesced registers are rewritten to use common base registers
merge_moves :: Annotation a => SelectResult a -> [(Reg a, Reg a)] -> SelectResult a
merge_moves p@(SpilledVars _ _)         _   = p
merge_moves   (RegisterAssignment vmap) mvs = RegisterAssignment vm'' where
    vm''             = subst (select $ unify (`elem` tmregs) mvs vmap)
    tmregs           = map (Reg (TPrim nullAnnotation "int")) mregs
    select (Left x)  = x
    select (Right e) = error e

-- allocate registers until there are no spills
allocate_registers :: (Annotation a, Eq a) => CFG (Inst a) -> CFG (Inst a)
allocate_registers bs = apply (try_assign_registers $ interference bs) where
    apply (SpilledVars vs mvs)     = allocate_registers (spill_registers bs vs mvs)
    apply (RegisterAssignment rgs) = rename_registers bs rgs

allocateRegisters :: (Annotation a, Eq a) => CFG (Inst a) -> InterferenceGraph a -> CFG (Inst a)
allocateRegisters bs ig = apply (try_assign_registers ig) where
    apply (RegisterAssignment rgs) = rename_registers bs rgs
    apply (SpilledVars vs mvs) = allocateRegisters bs' ig' where
        (bs', ig') = spillRegisters bs ig vs mvs

-- generate frames explaining register allocation step-by-step
regAllocDiagrams :: (Annotation a, Eq a) => String -> String -> CFG (Inst a) -> InterferenceGraph a -> [String]
regAllocDiagrams name label bs ig = apply (assignRegDiagrams name label ig) where
    apply (ds, ig', ss, mvs') = ds ++ step ig' ss Map.empty where
        mvs = Map.fromList mvs'
        step _ [] _ = []
        step sig (v:vs) rmap = stepDiag (merge_moves' (selectStep sig rmap v) mvs') where
            stepDiag (ig', RegisterAssignment rmap') = regAllocDiagram name label ig' vs mvs rmap' : step ig' vs rmap' where
            stepDiag (ig', SpilledVars vs mvs)    = regAllocDiagrams name (label ++ "'") bs' ig'' where
                (bs', ig'') = spillRegisters bs ig vs mvs
                
            merge_moves' (ig, p) m = (ig, merge_moves p m)

assignRegDiagrams :: String -> String -> InterferenceGraph a -> ([String], InterferenceGraph a, [Reg a], [(Reg a, Reg a)])
assignRegDiagrams name label ig = (map stepDiagram steps, ig', ss, mvs) where
    stepDiagram (ig, ss, mvs) = regAllocDiagram name label ig ss (Map.fromList mvs) Map.empty
    
    (ig', ss, mvs) = last steps
    steps          = concentricFixedPointSeq [simplify', coalesce', freeze', spill'] (ig, [], [])
    
    simplify' (ig, ss, mvs) = (ig', ss' ++ ss, mvs)         where (ig', ss')  = simplifyStep ig
    coalesce' (ig, ss, mvs) = (ig', ss,        mvs' ++ mvs) where (ig', mvs') = coalesceStep ig
    freeze'   (ig, ss, mvs) = (ig', ss,        mvs)         where ig'         = freeze   ig
    spill'    (ig, ss, mvs) = (ig', ss ++ ss', mvs)         where (ig', ss')  = spill    ig

simplifyStep :: InterferenceGraph a -> (InterferenceGraph a, [Reg a])
simplifyStep ig = step ig ss where
    step ig []    = (ig, [])
    step ig (s:_) = (del_var ig s, [s])
    ss   = [t | (t, (True, _)) <- Map.toList ig, not (precolored t), not (is_move_node ig t), degree ig t < k]

coalesceStep :: InterferenceGraph a -> (InterferenceGraph a, [(Reg a, Reg a)])
coalesceStep ig = step cs where
    step [] = (ig, [])
    step (c@(s0, s1):_) = (coalesce_vars ig s0 s1, [c])
    cs  = [(s0, s1) | (s0, (True, ig')) <- Map.toList ig, (s1, MayCoalesce) <- Map.toList ig', should_coalesce ig s0 s1]

selectStep :: InterferenceGraph a -> Coalescences a -> Reg a -> (InterferenceGraph a, SelectResult a)
selectStep ig mvs v = pick spills where
    pick []                        = (ig', RegisterAssignment vm)
    pick sps                       = (ig, SpilledVars sps (coalesce_spills ig sps))
    (ig', vm, spills)              = select_var (ig, mvs, []) v
    select_var (ig', vm, spills) v = either (\c -> (ins_var ig' v, Map.insert v c vm, spills)) (const (ig', vm, v:spills)) $ choice (choose_color ig' v vm)

-- show a step of register allocation as a diagram
regAllocDiagram :: String -> String -> InterferenceGraph a -> [Reg a] -> Coalescences a -> Coalescences a -> String
regAllocDiagram name label ig selStack moves regAssignments = digraphDescWLabel label (igDef ++ ssDef ++ mvDef ++ colKey) where
    igDef  = interferenceSubDiagram ig regColors False "Interference"
    ssDef  = selStackSubDiagram selStack
    mvDef  = regCoalesceDiagram moves regColors
    colKey = regColoringKeyDiagram
    regColors = [(mr, regColor mr) | mr <- mregs] ++ [(s0, regColor s1) | (Reg _ s0, Reg _ s1) <- regAssignments']
    regAssignments' = [(r, r') | (r, Just r') <- [(r, regDef r) | (r, _) <- Map.toList regAssignments]]
    regDef r = step (Map.lookup r regAssignments) where
        step (Just r) | isMachineReg r = Just r
        step (Just r)                  = regDef r
        step Nothing                   = Nothing

selStackSubDiagram :: [Reg a] -> String
selStackSubDiagram ss =
    "subgraph cluster_selstack {\n" ++
    "   label = \"Select Stack\";\n" ++
    "   \"selstack\" " ++
    "[" ++
        "style=\"filled, bold\" " ++ 
        "penwidth=1 " ++
        "fillcolor=\"white\" " ++
        "fontname=\"Courier New\" " ++ 
        "shape=\"Mrecord\" " ++
        "label=<" ++ tableDef ++ ">" ++
    "];\n" ++
    "}\n"
    where
        tableDef  = if ss == [] then etableDef else tableDef'
        etableDef = "   "
        tableDef' = "<table>" ++ concat ["<tr><td>" ++ show i ++ ": </td><td>" ++ rn ++ "</td></tr>" | (Reg _ rn, i) <- zip ss [0..]] ++ "</table>"

regCoalesceDiagram :: Coalescences a -> [(String,String)] -> String
regCoalesceDiagram mvs regColors =
    "subgraph cluster_coalesce {\n" ++
    "   label = \"Coalescences\";\n" ++
    concatMap nodeDef mvRegs ++
    concatMap edgeDef mvs' ++
    "}\n"
    where
        nodeDef (Reg _ rn) = "   \"" ++ rn ++ "\" [style=filled, fillcolor=" ++ regColor rn ++ ", label=<" ++ gvEsc rn ++ ">];\n"
        edgeDef (Reg _ rn, Reg _ rn') = "   \"" ++ rn ++ "\" -> \"" ++ rn' ++ "\" [dir=\"both\"];\n"
        mvRegs = unique (map fst mvs' ++ map snd mvs')
        mvs'   = Map.toList mvs
        regColor s = either id (const "white") (choice $ lookup s regColors)

regColoringKeyDiagram :: String
regColoringKeyDiagram =
    "subgraph cluster_regkey {\n" ++
    "   label = \"Key\";\n" ++
    "   \"keydef\" " ++
    "[" ++
        "style=\"filled, bold\" " ++ 
        "penwidth=1 " ++
        "fillcolor=\"white\" " ++
        "fontname=\"Courier New\" " ++ 
        "shape=\"Mrecord\" " ++
        "label=<" ++ tableDef ++ ">" ++
    "];\n" ++
    "}\n"
    where
        tableDef = "<table>" ++ concat ["<tr><td border=\"1\" bgcolor=\"" ++ regColor rn ++ "\">   </td><td>" ++ rn ++ "</td></tr>" | rn <- mregs] ++ "</table>"

regColor :: String -> String
regColor "ax" = "red"
regColor "cx" = "green"
regColor "dx" = "blue"
regColor "bx" = "yellow"
regColor "si" = "purple"
regColor "di" = "orange"
regColor "bp" = "grey"
regColor rn   = error ("No color defined for the machine register '" ++ rn ++ "'.")
