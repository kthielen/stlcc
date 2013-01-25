{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module ASM.SSA where
import ASM.Type
import ASM.Term

import Util.Annotated
import Util.CFG
import Util.Graph
import Util.String
import Util.Sequence
import Util.Tuples

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
    An SSA instruction is either a regular instruction or a phi-instruction
        (To compute SSA instructions, we'll need CFGs to be identifiable as graphs)
-}
data SSAInst a = Phi (Ty a) String [String] | SSAInst (Inst a) deriving Eq

instance Annotation a => Show (SSAInst a) where
    show (Phi _ v vs) = v ++ " <- #|Phi|#(" ++ cdelim vs ", " ++ ")"
    show (SSAInst i)  = show i

ssaInstRegDefs :: SSAInst a -> [Reg a]
ssaInstRegDefs (Phi ty v _) = [Reg ty v]
ssaInstRegDefs (SSAInst i)  = inst_reg_defs i

ssaMapRegDefs :: (Reg a -> Reg a) -> SSAInst a -> SSAInst a
ssaMapRegDefs f (Phi ty v vs) = Phi ty' v' vs where Reg ty' v' = f (Reg ty v)
ssaMapRegDefs f (SSAInst i)   = SSAInst (mapRegDefs f i)

ssaInstRegUses :: SSAInst a -> [Reg a]
ssaInstRegUses (Phi ty _ vs) = map (Reg ty) vs
ssaInstRegUses (SSAInst i)   = inst_reg_uses i

ssaMapRegUses :: (Reg a -> Reg a) -> SSAInst a -> SSAInst a
ssaMapRegUses f (Phi ty v vs) = Phi ty v [vn | Reg _ vn <- map (f . Reg ty) vs]
ssaMapRegUses f (SSAInst i)   = SSAInst (mapRegUses f i)

instance (Annotation a, Eq a) => Instruction (SSAInst a) where
    inst_type (SSAInst i) = inst_type i
    inst_type _           = NoCFlow
    label s               = SSAInst (label s)
    jump s                = SSAInst (jump s)
    invert (SSAInst i)    = SSAInst (invert i)
    invert i              = i

instance Instruction i => Node (BBlock i) where
    nodeName = block_name

instance Instruction i => Graph (CFG i) (BBlock i) where
    entry bbs = block_by_id 0 bbs
    nodes = id
    successors bbs bb = [named_block lbl bbs | lbl <- block_dests bb]
    predecessors bbs bb = [bb' | bb' <- bbs, block_name bb `elem` block_dests bb']

{-
    Convert an SSA CFG to a standard CFG by inserting moves:
        - Along CFG paths corresponding to phi arguments
        - Before pretend-3-address-codes
            For example:
                a <- add b, c
            Becomes:
                mov a, b
                add a, c
            (Because 'add' actually modifies its first argument.)
-}
ssaRetract :: (Annotation a, Eq a) => CFG (SSAInst a) -> CFG (Inst a)
ssaRetract bbs = orderTrace [insertRegMoves bb (bmvs bb) | bb <- bbs'] where
    bmvs bb = reverse (mlist (Map.lookup (block_id bb) mvs))
    mvs     = ssaPhiMoves bbs
    bbs'    = map ssaRetractBlockInsts bbs

ssaRetractBlockInsts :: (Annotation a, Eq a) => BBlock (SSAInst a) -> BBlock (Inst a)
ssaRetractBlockInsts (BBlock is m n o) = BBlock (ssaRetractInsts is) m n o

ssaRetractInsts :: [SSAInst a] -> [Inst a]
ssaRetractInsts is = concatMap rewriteInst is where
    rewriteInst (Phi _ _ _) = []
    rewriteInst (SSAInst i) = unTriple i' (ssaFakeAssignment i') where i' = retractStdRegNames i
    unTriple i (Just (Reg ty dr, Reg _ sr)) = [RMov (annotation ty) ty dr sr, mapRegUses (renameReg sr dr) i]
    unTriple i Nothing                      = [i]
    renameReg from to (Reg ty x) | x == from = Reg ty to
    renameReg _    _  x                      = x

ssaFakeAssignment :: Inst a -> Maybe (Reg a, Reg a)
ssaFakeAssignment i = choose (hasVarDest && (argDepShapeMismatch i) && regFirstArg) where
    choose True  = Just (fakeTripleDest, realTripleDest)
    choose False = Nothing
    
    fakeTripleDest = first [Reg ty rn | Reg ty rn <- inst_reg_defs i, not (isMachineRegName rn)]
    realTripleDest = first (inst_reg_uses i)
    
    hasVarDest  = count (\(Reg _ rn) -> not (isMachineRegName rn)) (inst_reg_defs i) == 1
    
    argDepShapeMismatch i@(Op _ _ ds ss _ lbls) = length ds + length ss + length lbls > length (instArgs i)
    argDepShapeMismatch _ = False
    
    regFirstArg = freg (instArgs i) where
        freg ((OAReg _ _ _):_) = True
        freg _                 = False

retractStdRegNames :: Inst a -> Inst a
retractStdRegNames i = mapRegs retractedStdReg i

retractedStdReg :: Reg a -> Reg a
retractedStdReg (Reg ty rn) = Reg ty (retractedStdRegName rn)

retractedStdRegName :: String -> String
retractedStdRegName s = if isMachineRegName s' then s' else s where
    s' = fst (lsplit s "(")

type PhiMoves a = Map.Map Int [(Reg a, Reg a)]
ssaPhiMoves :: (Annotation a, Eq a) => CFG (SSAInst a) -> PhiMoves a
ssaPhiMoves bbs = Map.fromList (group (concatMap (blockPhiMoves bbs) bbs))

blockPhiMoves :: (Annotation a, Eq a) => CFG (SSAInst a) -> BBlock (SSAInst a) -> [(Int, (Reg a, Reg a))]
blockPhiMoves bbs bb = concatMap phiMoves (block_insts bb) where
    bbps = predecessors bbs bb
    phiMoves (Phi ty v vs) = [(block_id (bbps !! vi), (Reg ty v, Reg ty vn)) | (vn, vi) <- zip vs [0..]]
    phiMoves _             = []

insertRegMoves :: (Annotation a, Eq a) => BBlock (Inst a) -> [(Reg a, Reg a)] -> BBlock (Inst a)
insertRegMoves bb mvs = foldl insertRegMove bb mvs

insertRegMove :: (Annotation a, Eq a) => BBlock (Inst a) -> (Reg a, Reg a) -> BBlock (Inst a)
insertRegMove bb (Reg ty dst, Reg _ src) = rpush_instruction bb (RMov (annotation ty) ty dst src)

{-
    Convert a CFG to SSA form by placing phi instructions and renaming variable references
-}
ssaConvert :: (Annotation a, Eq a) => CFG (Inst a) -> CFG (SSAInst a)
ssaConvert bbs = assignUniqueNames rdf defs bbs' where
    bbs' = foldl (placePhiOps rdf defs) (liftCFG bbs) vars
    defs = varDefSites bbs
    vars = Set.toList (definedVariables defs)
    
    rdf  = RefDominanceInfo bbs df doms
    df   = dominanceFrontiers bbs doms
    doms = dominators bbs

-- rename 'v' to 'v(n)' for each variable v
type RenameState = Map.Map String (Int, [Int])

assignUniqueNames :: (Annotation a, Eq a) => RefDominanceInfo a -> DefSites a -> CFG (SSAInst a) -> CFG (SSAInst a)
assignUniqueNames rdi defs cfg = cfg' where
    (_, cfg') = renameAcrossBlock rdi (initRenameState defs) cfg 0

renameAcrossBlock :: (Annotation a, Eq a) => RefDominanceInfo a -> RenameState -> CFG (SSAInst a) -> Int -> (RenameState, CFG (SSAInst a))
renameAcrossBlock rdi s cfg n = (popBlockVarState s'' bb, cfg''') where
    bb            = block_by_id n cfg
    (s', bb')     = renameAcrossBlockInstructions s bb
    cfg'          = replaceCFGBlock cfg bb'
    cfg''         = renamePhiArgsInSuccessors s' cfg' bb'
    (s'', cfg''') = renameAcrossBlockChildren rdi s' cfg'' n

replaceCFGBlock :: Instruction i => CFG i -> BBlock i -> CFG i
replaceCFGBlock bs b = [if block_id b' == block_id b then b else b' | b' <- bs]

renameAcrossBlockChildren :: (Annotation a, Eq a) => RefDominanceInfo a -> RenameState -> CFG (SSAInst a) -> Int -> (RenameState, CFG (SSAInst a))
renameAcrossBlockChildren rdi s cfg n = foldl renameChild (s, cfg) (refIDomChildren rdi n) where
    renameChild (s', cfg') c = renameAcrossBlock rdi s' cfg' c

renameAcrossBlockInstructions :: (Annotation a, Eq a) => RenameState -> BBlock (SSAInst a) -> (RenameState, BBlock (SSAInst a))
renameAcrossBlockInstructions s (BBlock is m n o) = (s', BBlock (reverse is') m n o) where
    (s', is') = foldl renameInst (s, []) is
    renameInst (s, is) i = (s', i'':is) where
        i'  = renameVarUses s i
        dvs = ssaInstRegDefs i'
        s'  = foldl pushIncrCountIdx s [v | Reg _ v <- dvs]
        i'' = renameVarDefs s' i'

renamePhiArgsInSuccessors :: (Annotation a, Eq a) => RenameState -> CFG (SSAInst a) -> BBlock (SSAInst a) -> CFG (SSAInst a)
renamePhiArgsInSuccessors s cfg bb = map renamePhi cfg where
    renamePhi bb' | not (block_name bb' `elem` block_dests bb) = bb'
    renamePhi bb'@(BBlock is m n o) = BBlock (map (renamePhiOp s i) is) m n o where
        i = at bb (predecessors cfg bb')

renameVarUses :: RenameState -> SSAInst a -> SSAInst a
renameVarUses s i@(Phi _ _ _) = i
renameVarUses s i             = ssaMapRegUses (\(Reg ty v) -> Reg ty (currentVarName s v)) i

renameVarDefs :: RenameState -> SSAInst a -> SSAInst a
renameVarDefs s i = ssaMapRegDefs (\(Reg ty v) -> Reg ty (currentVarName s v)) i

renamePhiOp :: RenameState -> Int -> SSAInst a -> SSAInst a
renamePhiOp s j (Phi ty v vs) = Phi ty v [if i == j then currentVarName s v' else v' | (v', i) <- zip vs [0..]]
renamePhiOp _ _ op            = op

currentVarName :: RenameState -> String -> String
currentVarName s v = v ++ "(" ++ show (currentVarIdx s v) ++ ")"

currentVarIdx :: RenameState -> String -> Int
currentVarIdx s v = tos (Map.lookup v s) where
    tos (Just (_, c:_)) = c
    tos (Just (_, []))  = error ("'" ++ v ++ "' stack underflow while reading index stack.")
    tos Nothing         = error ("'" ++ v ++ "' undefined while reading its index stack.")

pushIncrCountIdx :: RenameState -> String -> RenameState
pushIncrCountIdx s v = push (Map.lookup v s) where
    push (Just (c, cs)) = Map.insert v (c', c':cs) s where c' = c + 1
    push Nothing        = error ("'" ++ v ++ "' undefined while pushing name count.")

popBlockVarState :: (Annotation a, Eq a) => RenameState -> BBlock (SSAInst a) -> RenameState
popBlockVarState s (BBlock is _ _ _) = foldl pop s (concat [ssaInstRegDefs i | i <- is]) where
    pop s' (Reg _ v) = popVarIdx s' v

popVarIdx :: RenameState -> String -> RenameState
popVarIdx s v = pop (Map.lookup v s) where
    pop (Just (c, _:cs)) = Map.insert v (c, cs) s
    pop Nothing          = error ("'" ++ v ++ "' undefined while popping name count.")
    pop (Just (_, []))   = error ("'" ++ v ++ "' stack underflow while popping name count.")

initRenameState :: DefSites a -> RenameState
initRenameState defs = foldl insert Map.empty (Set.toList (definedVariables defs)) where
    insert m v = Map.insert v (0, [0]) m

-- place 'v <- phi(v,v,...,v)' in every block in the dominance frontier of nodes where v is defined
--  (this placement is itself a new definition for v, so the procedure must be iterated to a fixed point)
placePhiOps :: (Annotation a, Eq a) => RefDominanceInfo a -> DefSites a -> CFG (SSAInst a) -> String -> CFG (SSAInst a)
placePhiOps rdi defs cfg v = orderTrace (place cfg Set.empty (Set.toList vsites)) where
    vsites = defSites defs v
    ty     = defVarTy defs v
    
    place cfg _    []     = cfg
    place cfg tset (b:bs) = place cfg' tset' bs' where
        cfg'  = foldl (condInsertPhiNode tset) cfg ys
        ys    = refDominanceFrontier rdi b
        tset' = Set.union (Set.fromList ys) tset
        bs'   = [y | y <- ys, not (Set.member y tset)] ++ bs

    condInsertPhiNode tset cfg bid | Set.member bid tset = cfg
    condInsertPhiNode _    cfg bid = [if block_id bb == bid then insertPhiNode cfg bb else bb | bb <- cfg]
    
    insertPhiNode cfg bb = push_instruction bb (Phi ty v (take n (repeat v))) where
        n = length (predecessors cfg bb)

liftCFG :: (Annotation a, Eq a) => CFG (Inst a) -> CFG (SSAInst a)
liftCFG bbs = [BBlock (map SSAInst is) m n o | BBlock is m n o <- bbs]

{-
    Use the dominance frontier of the original (non-SSA) CFG
-}
data RefDominanceInfo a = RefDominanceInfo (CFG (Inst a)) (DominanceFrontier (BBlock (Inst a))) (DomTree (BBlock (Inst a)))

refDominanceFrontier :: (Annotation a, Eq a) => RefDominanceInfo a -> Int -> [Int]
refDominanceFrontier (RefDominanceInfo cfg df _) i = map block_id (Set.toList (dominanceFrontier df (block_by_id i cfg)))

refIDomChildren :: (Annotation a, Eq a) => RefDominanceInfo a -> Int -> [Int]
refIDomChildren (RefDominanceInfo cfg _ t) i = map block_id (Set.toList (idomChildren t (block_by_id i cfg)))

{-
    Determine all blocks where a given variable is defined in a standard CFG
-}
type DefSites a = Map.Map String (Ty a, Set.Set Int)

varDefSites :: (Annotation a, Eq a) => CFG (Inst a) -> DefSites a
varDefSites bbs = foldl insert Map.empty vdefs where
    vdefs = [(v, ty, blockID) | BBlock is blockID _ _ <- bbs, i <- is, Reg ty v <- inst_reg_defs i]
    
    insert m (v, ty, bid) = Map.insert v (append ty bid (Map.lookup v m)) m
    append _  bid (Just (ty, bids)) = (ty, Set.insert bid bids)
    append ty bid Nothing           = (ty, Set.singleton bid)

defVarTy :: DefSites a -> String -> Ty a
defVarTy d v = fst (uj (Map.lookup v d))

defSites :: DefSites a -> String -> Set.Set Int
defSites d v = snd (uj (Map.lookup v d))

definedVariables :: DefSites a -> Set.Set String
definedVariables = Map.keysSet

{-
    Determine all blocks where variables are defined/used in an SSA CFG
-}
type SSAUseLocs           = Set.Set Int
type SSADefUseData a      = (Ty a, Int, SSAUseLocs)
type SSANamedDefUseData a = (String, SSADefUseData a)
type SSADefUse a          = Map.Map String (SSADefUseData a)

ssaDefUse :: (Annotation a, Eq a) => CFG (SSAInst a) -> SSADefUse a
ssaDefUse bbs = foldl gatherBlockDefUse Map.empty bbs where
    gatherBlockDefUse m bb = foldl gatherInstDefUse m (blockNumberedInsts bb)
    gatherInstDefUse m (n, i) = m'' where
        m'  = foldl (gatherInstDef n) m  (ssaInstRegDefs i)
        m'' = foldl (gatherInstUse n) m' (ssaInstRegUses i)
    gatherInstDef n m (Reg ty vn) = ssaRegDefAt m n ty vn
    gatherInstUse n m (Reg ty vn) = ssaRegUseAt m n ty vn

ssaRegDefAt :: SSADefUse a -> Int -> Ty a -> String -> SSADefUse a
ssaRegDefAt m n ty vn = Map.insert vn (ty, n, ssaRegUses m vn) m

ssaRegUseAt :: SSADefUse a -> Int -> Ty a -> String -> SSADefUse a
ssaRegUseAt m n ty vn = Map.insert vn (ty, def (Map.lookup vn m), Set.insert n (ssaRegUses m vn)) m where
    def (Just (_, n, _)) = n
    def Nothing          = -1

ssaRegUses :: SSADefUse a -> String -> SSAUseLocs
ssaRegUses m vn = regUses (Map.lookup vn m) where
    regUses (Just (_, _, uses)) = uses
    regUses Nothing             = Set.empty

mapSSARegs :: (SSANamedDefUseData a -> b) -> SSADefUse a -> [b]
mapSSARegs f du = map f (Map.toList du)

ssaDURegRefs :: SSADefUse a -> [Reg a]
ssaDURegRefs du = [Reg ty vn | (vn, (ty, _, _)) <- Map.toList du]

foldlSSARegs :: (b -> SSANamedDefUseData a -> b) -> b -> SSADefUse a -> b
foldlSSARegs f s du = foldl f s (Map.toList du)
