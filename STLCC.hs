
module STLCC where
import ASM.Reader
import STLC.Reader
import STLC.Term
import STLC.Type
import AML.Term
import AML.Type (machineTy, sizeof)
import qualified AML.Type as AT
import ASM.Term

import AML.Translate
import ASM.SSA
import ASM.Liveness
import ASM.RegAllocation
import ASM.InstCFG
import ASM.X86_32.InstSel
import ASM.X86_32.Assembler
import Link.ObjFile

import Util.Annotated
import Util.CFG
import Util.String
import Util.Tuples
import Util.FileBuf
import Util.Graphviz
import Util.Directory

import Control.Monad.State

type TEnv a = [(String,Ty a)]

-- parse a set of STLC definitions and compile them
compileSTLCFile :: String -> IO CObj
compileSTLCFile fname = do
    let mname = first (rsplit (second (rsplit fname "/")) ".");
    (imports, itenv, defs) <- readSTLCFile fname;
    return (compile [imports ++ itenv] imports mname defs)

-- convert a set of STLCC definitions into a compiled object
compile :: (Annotation a, Eq a, TypeEnv tenv a) => tenv -> [(String, Ty a)] -> String -> [Definition a] -> CObj
compile tenv imports mname defs = assembleUnit (assemble tenv imports mname defs)

-- convert a set of STLC definitions into an assembly program
assemble :: (Annotation a, Eq a, TypeEnv tenv a) => tenv -> [(String, Ty a)] -> String -> [Definition a] -> [Inst a]
assemble tenv imports mname defs = ccode where
    (codeDefs, dataDefs)  = splitDefs (moveStringsToTop defs)
    mtenv                 = [imports ++ [(v, ty) | (v, _, ty, _) <- (codeDefs ++ dataDefs)]]
    
    ccode = first $ runState cdefs 0
    cdefs = do
        let datadecls = globalAllocData dataDefs;
        minit     <- moduleInitFn mtenv mname dataDefs;
        codeDefs' <- mapM compileCDef codeDefs;
        return $ datadecls ++ minit ++ concat codeDefs'
    compileCDef d = do
        code <- functionMachine mtenv d;
        compileFn code

-- convert a set of STLC definitions into an abstract machine program
translateToAbsCode :: (Annotation a, Eq a, TypeEnv tenv a) => tenv -> String -> [Definition a] -> [Stmt a]
translateToAbsCode tenv mname defs = ccode where
    (codeDefs, dataDefs)  = splitDefs (moveStringsToTop defs)
    mtenv                 = pushtys tenv [(v, ty) | (v, _, ty, _) <- (codeDefs ++ dataDefs)]
    
    ccode = first $ runState cdefs 0
    cdefs = do
        minit     <- moduleInitFn' mtenv mname dataDefs;
        codeDefs' <- mapM (functionMachine mtenv) codeDefs;
        return $ Label nullAnnotation ("_init_" ++ mname) : minit ++ concat codeDefs'

-- compile a function definition (from a sequence of pseudo-code instructions to machine code)
compileFn :: (Annotation a, Eq a) => [Stmt a] -> State Int [Inst a]
compileFn stmts = do
    pcfg <- build_cfg stmts;
    cfg  <- translateCFG pcfg;
    let cfg' = ssaConvert cfg;
    let ig   = ssaInterference (ssaDefUse cfg') cfg';
    let mcfg = normalizeMoves (allocateRegisters (ssaRetract cfg') ig);
    return (serialize_cfg (finalizeFunction mcfg))

-- split a sequence of definitions into data / code definitions
splitDefs :: [Definition a] -> ([Definition a], [Definition a])
splitDefs ds = foldr classify ([], []) ds where
    classify d@(_, [], _, _) (fds, dds) = (fds,   d:dds)
    classify f               (fds, dds) = (f:fds, dds)

-- create data labels to allocate global space for data definitions
globalAllocData :: Annotation a => [Definition a] -> [Inst a]
globalAllocData ds = concatMap galloc ds where
    galloc (s, _, ty, Prim _ (CString _ s')) = [DataDef na ("_" ++ s ++ "_bytes") (str s'), DataDef na ("_" ++ s) (zinit ty)]
    galloc (s, _, ty, d)                     = [DataDef na ("_" ++ s) (zinit ty)]
    zinit  ty                                = take (sizeof (machineTy ty)) (repeat 0)
    na = nullAnnotation

-- create a module initialization function to fill global data
moduleInitFn :: (Annotation a, Eq a, TypeEnv tenv a) => tenv -> String -> [Definition a] -> State Int [Inst a]
moduleInitFn tenv mname defs = do
    defs' <- mapM tfnDef defs;
    let efndef = ESeq na (sseq ((Arg na (AT.TPrim na "int") "_") : defs')) (Const na (AT.CInt na 0));
    
    mfn <- functionExpr efndef;
    compileFn ((Label na ("_init_" ++ mname)) : mfn)
    where
        tfnDef (v, [], sty, Prim p (CString _ s')) = do
            let dlbl = LName p ("_" ++ v ++ "_bytes");
            let vlbl = LName p ("_" ++ v);
            return (Move p (Mem p (machineTy sty) vlbl) dlbl)
        tfnDef (v, [], ty, e) = do
            absMachine tenv (Move na (Mem na (machineTy ty) (LName na ("_" ++ v)))) e
            
        na = nullAnnotation
    
moduleInitFn' tenv mname defs = do
    let na = nullAnnotation
    
    let tfnDef (v, _, ty, e) = do
        absMachine tenv (Move na (Mem na (machineTy ty) (LName na ("_" ++ v)))) e
        
    defs' <- mapM tfnDef defs;
    let efndef = ESeq na (sseq ((Arg na (machineTy (TPrim na "int")) "_") : defs')) (Const na (AT.CInt na 0));
    
    functionExpr efndef

moveStringsToTop :: Annotation a => [Definition a] -> [Definition a]
moveStringsToTop defs = join (runState (liftBulkConstants defs) (0, [])) where
    join (defs', (_, hdefs)) = hdefs ++ defs'

liftBulkConstants :: Annotation a => [Definition a] -> State (Int, [Definition a]) [Definition a]
liftBulkConstants defs = mapM liftBulkConstant defs where
    liftBulkConstant (v, [], ty, Prim p x) = return (v, [], ty, Prim p x)
    liftBulkConstant (v, d,  ty, e)        = do
        e' <- mapMTerm isStrConst liftStrConst e
        return (v, d, ty, e')
    isStrConst (Prim _ (CString _ _)) = True
    isStrConst _                      = False
    liftStrConst x = do
        (id, defs) <- get;
        let na = nullAnnotation
        let vname = "#str" ++ show id;
        put (id+1, (vname, [], typeof ([]::[[(String,Ty a)]]) (Prim na (CString na "")), x) : defs);
        return (Var na vname)

-- print all analysis diagrams from an STLC file
printSTLCAnalysis :: String -> String -> IO ()
printSTLCAnalysis infile outdir = do
    let (inpath, fname, ext) = parseFilePath infile;
    (imports, rootTFrame, defs) <- readSTLCFile infile;
    let itenv = [imports ++ rootTFrame];
    let (codeDefs, dataDefs) = splitDefs (moveStringsToTop defs);
    let mtenv = [imports ++ [(v, ty) | (v, _, ty, _) <- (codeDefs ++ dataDefs)]]
    let compileCDef d@(funcName, _, _, _) = do
        code <- functionMachine mtenv d;
        pcfg <- build_cfg code;
        cfg  <- translateCFG pcfg;
        let cfgdesc  = cfgDiagram funcName cfg;
        let cfg'     = ssaConvert cfg;
        let ssaIG    = ssaInterference (ssaDefUse cfg') cfg';
        let mcfg     = normalizeMoves (allocateRegisters (ssaRetract cfg') ssaIG);
        let regSteps = regAllocDiagrams "regalloc" "Register Allocation" (ssaRetract cfg') ssaIG;
        let reggs    = [("regalloc/step" ++ show i, gd) | (gd, i) <- zip regSteps [0..]];
        return (funcName, ("cfg", digraphDesc (cfgDiagram "" cfg')) : ("stack", frameRefDiag mcfg) : reggs)
        
    let (cfgDefs, _) = runState (mapM compileCDef codeDefs) 0;

    let outputDiag funcName (diagName, graphDesc) = do
        dotFile ("./diagrams/" ++ infile ++ "/" ++ funcName ++ "/" ++ diagName ++ ".gif") graphDesc

    -- briefly comment on what we're doing ...
    putStrLn $ "Generating diagrams for the file '" ++ infile ++ "'.";
        
    -- generate CFG and interference diagrams for each function
    forM_ cfgDefs (\(fn, gds) -> forM_ gds (outputDiag fn));
    
    -- generate term dependency graph for the whole module
    let datag = definitionDependencyGraph "data" "Data Dependency" (definitionDependencies dataDefs);
    let codeg = definitionDependencyGraph "code" "Code Dependency" (definitionDependencies codeDefs);
    dotFile ("./diagrams/" ++ infile ++ "/datadep.gif") (digraphDesc datag)
    dotFile ("./diagrams/" ++ infile ++ "/codedep.gif") (digraphDesc codeg)

