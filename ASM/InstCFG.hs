
module ASM.InstCFG where
import ASM.Type
import ASM.Term
import ASM.Liveness

import Util.Annotated
import Util.CFG
import Util.Sequence
import Util.Num
import Util.Tuples
import Util.Recursion

import qualified Data.Map as Map

block_reg_refs :: (Annotation a, Eq a) => BBlock (Inst a) -> [Reg a]
block_reg_refs (BBlock is _ _ _) = unique $ concat $ map inst_reg_refs is

cfg_reg_refs :: (Annotation a, Eq a) => CFG (Inst a) -> [Reg a]
cfg_reg_refs cfg = unique $ concat $ map block_reg_refs cfg

rename_registers :: (Annotation a, Eq a) => CFG (Inst a) -> Map.Map (Reg a) (Reg a) -> CFG (Inst a)
rename_registers bs vn = map rename_block bs where
    rename_block (BBlock is m n o) = BBlock (map rename_inst is) m n o
    rename_inst   (Op p o dsts srcs f jmps) = Op p o (map rename_loc dsts) (map rename_loc srcs) f jmps
    rename_inst   (RMov p ty dst src)       = RMov p ty (subst dst) (subst src)
    rename_inst   p                         = p
    rename_loc    (OAReg     p ty s)        = OAReg p ty (subst s)
    rename_loc    (OARMemOff p ty s x)      = OARMemOff p ty (subst s) x
    rename_loc    p                         = p
    subst s                                 = either id (const s) $ choice (Map.lookup s vn')
    vn'                                     = Map.fromList [(s0, s1) | (Reg _ s0, Reg _ s1) <- Map.toList vn]

normalizeMoves :: (Annotation a, Eq a) => CFG (Inst a) -> CFG (Inst a)
normalizeMoves bs = map normalizeBlock bs where
    normalizeBlock (BBlock is m n o) = BBlock [normalize i | i <- is, not (redundant i)] m n o
    normalize (RMov p ty dst src) = realmov p (OAReg p ty dst) (OAReg p ty src)
    normalize x                   = x
    
    redundant (RMov _ ty dst src)          | dst == src = True
    redundant (Op _ "mov" [dst] [src] f j) | dst == src = True
    redundant _                                         = False

next_frame_slot :: (Annotation a, Eq a) => CFG (Inst a) -> Int
next_frame_slot bs = 1 + (seqMax 0 (map bmax bs)) where
    bmax   (BBlock is _ _ _)   = seqMax 0 (map imax is)
    imax   (Op _ _ ds ss _ _)  = seqMax (seqMax 0 (map oaslot ds)) (map oaslot ss)
    imax   _                   = 0
    oaslot (OAFrameSlot _ _ i _) = i
    oaslot _                     = 0

spill_registers :: (Annotation a, Eq a) => CFG (Inst a) -> [Reg a] -> Map.Map (Reg a) (Reg a) -> CFG (Inst a)
spill_registers bs vs mvs = orderTrace $ reverse $ first $ foldl spill_block ([], 0) bs where
    spill_block (bs', n) (BBlock is m k o) = ((BBlock is' m k o):bs', n') where (is', n') = spill_insts is n
    spill_insts (i:is) n                   = (i' ++ is', n'') where (i', n') = spill_inst i n; (is', n'') = spill_insts is n'
    spill_insts [] n                       = ([], n)
    spill_inst i n                         = spill_rewrite_inst_regs ss i n
    ss                                     = choose_stack_slots bs vs mvs

spill_rewrite_inst_regs :: Annotation a => Map.Map (Reg a) Int -> Inst a -> Int -> ([Inst a], Int)
spill_rewrite_inst_regs ss i n = (pfx ++ [i''] ++ sfx, n'') where
    (pfx, i',  n')  = foldl spill_read  ([], i,  n)  uses
    (i'', sfx, n'') = foldl spill_write (i', [], n') defs
    uses = marked_regs (inst_reg_uses i)
    defs = marked_regs (inst_reg_defs i)
    marked_regs rseq = [(r, s) | (r, Just s) <- [(r, Map.lookup r ss) | r <- rseq]]
    spill_read  (pfx, i,   n) (r, s) = (pfx ++ pfx', i', n') where (pfx', i',   n') = spill_inst_read  i r s n
    spill_write (i,   sfx, n) (r, s) = (i', sfx' ++ sfx, n') where (i',   sfx', n') = spill_inst_write i r s n

spill_inst_read :: Annotation a => Inst a -> Reg a -> Int -> Int -> ([Inst a], Inst a, Int)
spill_inst_read (Op p "tailcall" [] srcs f []) (Reg ty r) s n = ([], i', n) where
    i' = Op p "tailcall" [] (map (rewriteRegister r (OAFrameSlot p ty s Local)) srcs) f []
spill_inst_read i (Reg ty r) s n = (prefix, rewrite i, n + 1) where
    prefix   = [mov na treg tspill]
    tregname = "spr" ++ r ++ show n
    treg     = OAReg na ty tregname
    tspill   = OAFrameSlot na ty s Local
    rewrite   (Op p o dsts srcs f lbls)       = Op p o dsts (map (rename_register r tregname) srcs) f lbls
    rewrite   (RMov p ty' dst src) | src == r = RMov p ty' dst tregname
    rewrite p@(RMov _ _   _   _)              = p
    na = nullAnnotation

spill_inst_write :: Annotation a => Inst a -> Reg a -> Int -> Int -> (Inst a, [Inst a], Int)
spill_inst_write i (Reg ty r) s n = (rewrite i, suffix, n + 1) where
    suffix = [mov na tspill treg]
    tregname = "spw" ++ r ++ show n
    treg     = OAReg na ty tregname
    tspill   = OAFrameSlot na ty s Local
    rewrite   (Op p o dsts srcs f lbls)       = Op p o (map (rename_register r tregname) dsts) srcs f lbls
    rewrite   (RMov p ty' dst src) | dst == r = RMov p ty' tregname src
    rewrite p@(RMov _ _   _   _)              = p
    na = nullAnnotation

rename_register :: String -> String -> OpArg a -> OpArg a
rename_register f t   (OAReg p ty s)       | s == f = OAReg p ty t
rename_register f t   (OARMemOff p ty s n) | s == f = OARMemOff p ty t n
rename_register _ _   x                             = x

rewriteRegister :: String -> OpArg a -> OpArg a -> OpArg a
rewriteRegister r x (OAReg _ _ r') | r == r' = x
rewriteRegister _ _ x                        = x

choose_stack_slots :: (Annotation a, Eq a) => CFG (Inst a) -> [Reg a] -> Map.Map (Reg a) (Reg a) -> Map.Map (Reg a) Int
choose_stack_slots bs vs mvs = first $ fixedPoint merge_mvs (vs', Map.toList mvs) where
    n                              = next_frame_slot bs
    vs'                            = Map.fromList $ mapi (\n' vn -> (vn, n' + n)) vs
    merge_mvs (d, mvs)             = foldl merge_mv (d, []) mvs
    merge_mv  p@(d, iv) (s0, s1)   = merge_def (merge_def p s0 s1 (Map.lookup s1 d)) s1 s0 (Map.lookup s0 d)
    merge_def (d, iv) n r (Just x) = (Map.insert n x d, iv)
    merge_def (d, iv) n r Nothing  = (d, (n,r):iv)

------ better register spilling
type StackLocs     a = Map.Map (Reg a) Int
type CoalescedRegs a = Map.Map (Reg a) (Reg a)

spillInterference :: InterferenceGraph a -> [Reg a] -> InterferenceGraph a
spillInterference ig regs = foldl (add_edge Interfere) ig' [(r1, r2) | r1 <- regs, r2 <- regs, r1 /= r2] where
    ig' = foldl createVar ig regs

spillRegisters :: (Annotation a, Eq a) => CFG (Inst a) -> InterferenceGraph a -> [Reg a] -> CoalescedRegs a -> (CFG (Inst a), InterferenceGraph a)
spillRegisters bs ig vs mvs = (orderTrace (reverse bs'), ig'') where
    (bs', _, ig'') = foldl spillBlock ([], 0, ig') bs
    ss             = chooseStackSlots bs vs mvs
    ig'            = Map.foldWithKey (\r _ ig' -> killVar ig' r) ig ss
    
    spillBlock (bs', n, ig') (BBlock is m k o) = (BBlock is' m k o : bs', n', ig'') where
        (is', n', ig'') = spillInsts is n ig'

    spillInsts [] n ig = ([], n, ig)
    spillInsts (i:is) n ig = (i' ++ is', n'', ig'') where
        (i', n', ig')    = spillRewriteInstRegs ss i n ig
        (is', n'', ig'') = spillInsts is n' ig'

spillRewriteInstRegs :: Annotation a => StackLocs a -> Inst a -> Int -> InterferenceGraph a -> ([Inst a], Int, InterferenceGraph a)
spillRewriteInstRegs ss i n ig = (is, n'', ig') where
    is  = pfx ++ [i''] ++ sfx
    ig' = spillInterference ig (unique (concatMap inst_reg_refs is))
    
    (pfx, i',  n')  = foldl spillRead  ([], i,  n)  uses
    (i'', sfx, n'') = foldl spillWrite (i', [], n') defs
    
    uses = markedRegs (inst_reg_uses i)
    defs = markedRegs (inst_reg_defs i)
    markedRegs rseq = [(r, s) | (r, Just s) <- [(r, Map.lookup r ss) | r <- rseq]]
    
    spillRead (pfx, i, n) (r, s) = (pfx ++ pfx', i', n') where
        (pfx', i', n') = spillInstRead i r s n

    spillWrite (i, sfx, n) (r, s) = (i', sfx' ++ sfx, n') where
        (i', sfx', n') = spillInstWrite i r s n

spillInstRead :: Annotation a => Inst a -> Reg a -> Int -> Int -> ([Inst a], Inst a, Int)
spillInstRead (Op p "tailcall" [] srcs f []) (Reg ty r) s n = ([], i', n) where
    i' = Op p "tailcall" [] (map (rewriteRegister r (OAFrameSlot nullAnnotation ty s Local)) srcs) f []
spillInstRead i (Reg ty r) s n = (prefix, rewrite i, n + 1) where
    prefix   = [mov na treg tspill]
    tregname = "spr" ++ r ++ show n
    treg     = OAReg na ty tregname
    tspill   = OAFrameSlot na ty s Local
    rewrite   (Op p o dsts srcs f lbls) = Op p o dsts (map (rename_register r tregname) srcs) f lbls
    rewrite   (RMov p ty' dst src) | src == r = RMov p ty' dst tregname
    rewrite p@(RMov _ _   _   _)              = p
    na = nullAnnotation
    
spillInstWrite :: Annotation a => Inst a -> Reg a -> Int -> Int -> (Inst a, [Inst a], Int)
spillInstWrite i (Reg ty r) s n = (rewrite i, suffix, n + 1) where
    suffix = [mov na tspill treg]
    tregname = "spw" ++ r ++ show n
    treg     = OAReg na ty tregname
    tspill   = OAFrameSlot na ty s Local
    rewrite   (Op p o dsts srcs f lbls) = Op p o (map (rename_register r tregname) dsts) srcs f lbls
    rewrite   (RMov p ty' dst src) | dst == r = RMov p ty' tregname src
    rewrite p@(RMov _ _   _   _)              = p
    na = nullAnnotation

chooseStackSlots :: (Annotation a, Eq a) => CFG (Inst a) -> [Reg a] -> CoalescedRegs a -> StackLocs a
chooseStackSlots bs vs mvs = ss where
    (ss, _) = fixedPoint mergeMoves (vs', Map.toList mvs)
    n       = next_frame_slot bs
    vs'     = Map.fromList (mapi (\n' vn -> (vn, n' + n)) vs)
    
    mergeMoves (d, mvs) = foldl mergeMove (d, []) mvs
    
    mergeMove p@(d, iv) (s0, s1) = mergeDef (mergeDef p s0 s1 (Map.lookup s1 d)) s1 s0 (Map.lookup s0 d)
    
    mergeDef (d, iv) n r (Just x) = (Map.insert n x d, iv)
    mergeDef (d, iv) n r Nothing  = (d, (n,r):iv)

-- add a prolog/epilog to allocate local variables and preserve callee-save registers
-- also, commit stack frame references to actual offsets against the sp register
finalizeFunction :: Annotation a => Eq a => CFG (Inst a) -> CFG (Inst a)
finalizeFunction [] = []
finalizeFunction bs = orderTrace bs''' where
    bs''' = [BBlock prolog 0 0 0] ++ bs'' ++ [BBlock epilog (1 + length bs) 0 0]
    bs''  = map (renameNamedBlock ibname entrance) [BBlock is (n + 1) m o | BBlock is n m o <- bs']
    
    bs' = concatMapCFG patchExit (assignFrameOffsets frefs sregs bs)
    patchExit (Op p "ret" _ _ _ _) = [jmp p exit]
    patchExit (Op p "tailcall" _ [OAConst _ (CInt _ cid), x] _ _) = deallocFrame (Just cid) frefs sregs ++ [jmpArg p x]
    patchExit x = [x]
    
    rootBlock = block_by_id 0 bs
    ibname    = block_name rootBlock
    entrance  = "#" ++ ibname ++ "_entrance"
    prolog    = [LblDef na ibname] ++ allocFrame frefs sregs ++ [jmp na entrance]
    epilog    = [LblDef na exit] ++ deallocFrame Nothing frefs sregs ++ [ret na (argFrameSize frefs)]
    exit      = "#" ++ ibname ++ "_exit"
    
    frefs = findFrameRefs bs
    sregs = findSaveRegs bs
    
    na = nullAnnotation

-- produces the code to initialize a call frame
allocFrame :: Annotation a => FrameRefs a -> [String] -> [Inst a]
allocFrame frefs sregs = alloc (localFrameSize frefs + tailFrameExpansion frefs) ++ saves where
    alloc 0 = []
    alloc n = [sub na (OAReg na (TPrim na "int") "sp") (OAConst na (CInt na n))]
    saves   = [push na (OAReg na (TPrim na "int") r) | r <- sregs]
    
    na = nullAnnotation
    
-- produces the code to uninitialize a call frame optionally trimming the stack frame for a tail call
deallocFrame :: Annotation a => Maybe Int -> FrameRefs a -> [String] -> [Inst a]
deallocFrame tailctx frefs sregs = restores ++ dealloc (deallocSize tailctx) where
    deallocSize (Just cid) = localFrameSize frefs + tailCallExpansion frefs cid
    deallocSize Nothing    = localFrameSize frefs + tailFrameExpansion frefs
    
    restores = [pop na (OAReg na (TPrim na "int") r)  | r <- reverse sregs]
    dealloc 0 = []
    dealloc n = [add na (OAReg na (TPrim na "int") "sp") (OAConst na (CInt na n))]
    
    na = nullAnnotation

-- add extra stack space when tail-calling a function with a larger frame
tailFrameExpansion :: Annotation a => FrameRefs a -> Int
tailFrameExpansion frefs = ef (tailFrameSize frefs - argFrameSize frefs) where
    ef n | n > 0 = n
    ef _         = 0

-- the stack space required to make a particular tail call
tailCallExpansion :: Annotation a => FrameRefs a -> Int -> Int
tailCallExpansion frefs cid = ef (tailFrameSize frefs - tailCallSize frefs cid) + ef (argFrameSize frefs - tailFrameSize frefs) where
    ef n | n > 0 = n
    ef _         = 0

-- reduce excess stack space when tail-calling a function with a smaller frame
trimTailFrame :: Annotation a => FrameRefs a -> Int -> [Inst a]
trimTailFrame frefs cid = tf (argFrameSize frefs - tailCallSize frefs cid) where
    na           = nullAnnotation
    tf n | n > 0 = [add na (OAReg na (TPrim na "int") "sp") (OAConst na (CInt na n))]
    tf _         = []

-- rewrite frame slot references to explicit memory offsets from the stack pointer
assignFrameOffsets :: (Annotation a, Eq a) => FrameRefs a -> [String] -> CFG (Inst a) -> CFG (Inst a)
assignFrameOffsets frefs@(args, locals, tailas) sregs bs = mapRewriteArgs assignFrameOffset bs where
    assignFrameOffset off (OAFrameSlot p ty sid (TailParam cid)) = OARMemOff p ty "sp" (off + sregoff + tailFrameOffset toffs cid sid + tailFrameExpansion frefs)
    assignFrameOffset off (OAFrameSlot p ty sid _)               = OARMemOff p ty "sp" (off + sregoff + frameOffset sid + tailFrameExpansion frefs)
    assignFrameOffset _   x                                      = x
    
    tailArgStart    = argFrameSize frefs + sizeof (TPrim na "int") + localFrameSize frefs
    toffs           = tailFrameOffsets tailArgStart frefs
    frameOffset sid = uj (Map.lookup sid foffs)
    foffs           = frameOffsets frefs
    sregoff         = sizeof (TPrim na "int") * length sregs
    
    na = relatedNullAnnotation (cfg_node bs 0)

-- find references to local variables and arguments in order
type SlotTys        a = Map.Map Int (Ty a)
type TailSlotFrames a = Map.Map Int (SlotTys a)
type FrameRefs      a = (SlotTys a, SlotTys a, TailSlotFrames a)

findFrameRefs :: (Annotation a, Eq a) => CFG (Inst a) -> FrameRefs a
findFrameRefs bs = (order args, order locals, tailas) where
    order x = Map.fromList x
    (args, locals, tailas) = foldl blockGather ([], [], Map.empty) bs
    blockGather (args, locals, tailas) (BBlock is m k o) = foldl instGather (args, locals, tailas) is
    
    -- for 0-arity tail calls (like to the 'failed_match' built-in function), make sure that the tail call is tracked
    instGather (args, locals, tailas) (Op _ "tailcall" _ [OAConst _ (CInt _ cid), _] _ _) = (args, locals, def (Map.lookup cid tailas)) where
        def (Just _) = tailas
        def Nothing  = Map.insert cid Map.empty tailas
        
    -- track the number and type of frame slots for parameters, local variables, and for each tail call
    instGather (args, locals, tailas) i = (args ++ args', locals ++ locals', foldl tinsert tailas tailas') where
        args'   = [(n, ty)      | OAFrameSlot _ ty n Param           <- instArgs i]
        locals' = [(n, ty)      | OAFrameSlot _ ty n Local           <- instArgs i]
        tailas' = [(cid, n, ty) | OAFrameSlot _ ty n (TailParam cid) <- instArgs i, n >= 0]
        tinsert m (cid, n, ty) = insertMapVal m cid n ty

argFrameSize :: Annotation a => FrameRefs a -> Int
argFrameSize (args, _, _) = frameSize args

localFrameSize :: Annotation a => FrameRefs a -> Int
localFrameSize (_, locals, _) = frameSize locals

tailFrameSize :: Annotation a => FrameRefs a -> Int
tailFrameSize (_, _, tailas) = seqMax 0 [frameSize frame | (_, frame) <- Map.toList tailas]

tailCallSize :: Annotation a => FrameRefs a -> Int -> Int
tailCallSize (_, _, tailas) cid = frameSize (uj (Map.lookup cid tailas))

frameSize :: Annotation a => SlotTys a -> Int
frameSize ss = sum [sizeof t | (_, t) <- Map.toList ss]

-- given a sequence of frame variables, determines the offset of each within the frame
type FrameOffsets     = Map.Map Int Int
type TailFrameOffsets = Map.Map Int FrameOffsets

frameOffsets :: Annotation a => FrameRefs a -> FrameOffsets
frameOffsets (args, locals, _) = snd $ foldl computeOffset (0, Map.empty) (reverse $ Map.toList args ++ [(-1, TPrim nullAnnotation "int")] ++ Map.toList locals) where
    computeOffset (off, foMap) (i, ty) = (off + sizeof ty, Map.insert i off foMap)

tailFrameOffsets :: Annotation a => Int -> FrameRefs a -> TailFrameOffsets
tailFrameOffsets start (_, _, tsfs) = Map.map tailCallOffsets tsfs where
    tailCallOffsets fvs = snd $ foldl computeOffset (start, Map.empty) (Map.toList fvs ++ [(-1, TPrim nullAnnotation "int")])
    computeOffset (off, foMap) (i, ty) = (off', Map.insert i off' foMap) where off' = off - sizeof ty

tailFrameOffset :: TailFrameOffsets -> Int -> Int -> Int
tailFrameOffset tfo cid sid = uj (map2Lookup tfo cid sid)

-- allow the rewriting of stack frame offsets within a sequence of pushes for a function call
mapRewriteArgs :: (Annotation a, Eq a) => (Int -> OpArg a -> OpArg a) -> CFG (Inst a) -> CFG (Inst a)
mapRewriteArgs f bs = map rewrite bs where
    rewrite (BBlock is m k o) = BBlock (reverse $ second $ foldl rewritei (0, []) is) m k o
    rewritei (off, is') i = (clear i off + doff i, rewriteargs off i : is')
    
    clear (Op _ "call" _ _ _ _) _ = 0
    clear _                     x = x
    
    doff (Op _ "push" _ [r] _ _) = sizeof (argTy r)
    doff _                       = 0
    
    rewriteargs off (Op p o dsts srcs sf lbls) = Op p o (map (f off) dsts) (map (f off) srcs) sf lbls
    rewriteargs _   x                          = x

-- find the set of callee-save registers written in a body of code
findSaveRegs :: (Annotation a, Eq a) => CFG (Inst a) -> [String]
findSaveRegs bs = unique rs where
    rs = foldr gather_brefs [] bs
    gather_brefs (BBlock is _ _ _) rs = foldr gather_irefs rs is
    gather_irefs i                 rs = [r | Reg _ r <- inst_reg_defs i, not (r `elem` cregs)] ++ rs

-- display the stack frame structure of a control-flow graph
frameRefDiag :: (Annotation a, Eq a) => CFG (Inst a) -> String
frameRefDiag bs =
    "digraph G {\n" ++
    "   label = \"Stack Alignments\";\n" ++
    "   img [shape=\"Mrecord\" label=<" ++ showFrameRefs bs ++ ">];\n" ++
    "}\n"

showFrameRefs :: (Annotation a, Eq a) => CFG (Inst a) -> String
showFrameRefs bs = frameTable where
    frameTable = "<table>" ++ concat [frameBlock name frame ["red","blue"] | (name, frame) <- ("arguments", args) : map (\(i,v) -> ("tail call #" ++ show i, v)) (Map.toList tailcs)] ++ "</table>"
    (args, locals, tailcs) = findFrameRefs bs
    sregs = findSaveRegs bs

frameBlock :: Annotation a => String -> SlotTys a -> [String] -> String
frameBlock name f colors = "<tr><td>" ++ name ++ "</td>" ++ concat [tyBlock ty c | ((_, ty), c) <- zip (Map.toList f) (cycle colors)] ++ "</tr>"

tyBlock :: Annotation a => Ty a -> String -> String
tyBlock ty color = concat (take (sizeof ty) (repeat ("<td bgcolor=\"" ++ color ++ "\"> </td>")))
