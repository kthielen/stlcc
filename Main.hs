module Main where
import STLCC
import STLC.Reader
import STLC.Term
import ASM.Reader
import Link.ObjFile
import qualified Link.Windows.PE as W32
import qualified Link.Linux.ELF as L32
import Util.String

import System
import System.Console.GetOpt
import Directory
import Control.Monad

data Options    = Options { accumf :: AccumField, stlcFiles :: [String], asmFiles :: [String], outputFile :: String, outputMode :: OutputMode, targetOS :: TargetOS } deriving (Eq, Show)
data AccumField = STLCFiles | AsmFiles deriving (Eq, Show)
data OutputMode = Executable | ParsedSTLC | STLCCode | AssemblyCode | AnalysisDiags | AbsMachine deriving (Eq, Show)
data TargetOS   = Win32 | Linux32 deriving (Eq, Show)

defaultOptions = Options { accumf=STLCFiles, stlcFiles=[], asmFiles=[], outputFile="./", outputMode=Executable, targetOS=Win32 }
programDesc = "Usage: stlcc [OPTION...]"

main :: IO ()
main = do
    args <- getArgs;
    let (actions, nonOpts, msgs) = getOpt (ReturnInOrder accumArg) options args;
    opts  <- mjoin (return defaultOptions) actions;
    evalOptions opts

evalOptions :: Options -> IO ()
evalOptions opts | outputMode opts == Executable = do
    objs  <- buildFiles (stlcFiles opts) (asmFiles opts);
    linkExecutable (targetOS opts) (outputFile opts) objs;
    putStrLn "Done."

evalOptions opts | outputMode opts == ParsedSTLC = do
    forM_ [(f, mustEndWith (outputFile opts) "/" ++ "unparsed_" ++ f) | f <- stlcFiles opts] outputParsedSTLCFile;
    putStrLn "Done."
    where
        outputParsedSTLCFile (infile, outfile) = do
            putStrLn $ "Unparsing '" ++ infile ++ "' to the STLC file '" ++ outfile ++ "'.";
            (_, _, defs) <- readSTLCFile infile;
            writeFile outfile (concatMap ((++"\n\n") . showDef) defs)

evalOptions opts | outputMode opts == STLCCode = do
    forM_ [(f, mustEndWith (outputFile opts) "/" ++ "bitter_" ++ f) | f <- stlcFiles opts] outputDefSTLCFile;
    putStrLn "Done."
    where
        outputDefSTLCFile (infile, outfile) = do
            putStrLn $ "Desugaring '" ++ infile ++ "' to the STLC file '" ++ outfile ++ "'.";
            (imports, itenv, defs) <- readSTLCFile infile;
            let defs' = moveStringsToTop defs;
            writeFile outfile (concatMap ((++"\n\n") . showDef) defs')

evalOptions opts | outputMode opts == AssemblyCode = do
    forM_ (stlcFiles opts) (assembleSTLCFile (outputFile opts))

evalOptions opts | outputMode opts == AnalysisDiags = do
    forM_ (stlcFiles opts) (\file -> printSTLCAnalysis file (outputFile opts))

evalOptions opts | outputMode opts == AbsMachine = do
    forM_ (stlcFiles opts) (prepareAbsSTLCFile (outputFile opts))
    where
        prepareAbsSTLCFile outdir infile = do
            let (inpath, fname, ext) = parseFilePath infile;
            let outfile = mustEndWith outdir "/" ++ fname ++ ".abs";
            putStrLn $ "Compiling '" ++ infile ++ "' to the abstract-machine code file '" ++ outfile ++ "'.";
            (imports, itenv, defs) <- readSTLCFile infile;
            writeFile outfile (concat [show i ++ "\n" | i <- translateToAbsCode [imports ++ itenv] fname defs])

assembleSTLCFile :: String -> String -> IO ()
assembleSTLCFile outdir infile = do
    let (inpath, fname, ext) = parseFilePath infile;
    let outfile = mustEndWith outdir "/" ++ fname ++ ".asm";
    putStrLn $ "Compiling '" ++ infile ++ "' to the assembly file '" ++ outfile ++ "'.";
    (imports, itenv, defs) <- readSTLCFile infile;
    writeFile outfile (concat [show i ++ "\n" | i <- assemble [imports ++ itenv] imports fname defs])
    
buildFiles :: [String] -> [String] -> IO [CObj]
buildFiles cfiles afiles = do
    cobjs <- compileFiles cfiles;
    aobjs <- assembleFiles afiles;
    return $ cobjs ++ aobjs
            
compileFiles :: [String] -> IO [CObj]
compileFiles fs = mapM cfile fs where
    cfile f = do
        putStrLn $ "Compiling '" ++ f ++ "'.";
        compileSTLCFile f

assembleFiles :: [String] -> IO [CObj]
assembleFiles fs = do { objs <- mapM afile fs; return (concat objs) } where
    afile f = do
        putStrLn $ "Assembling '" ++ f ++ "'.";
        translateAsmFile f

linkExecutable :: TargetOS -> String -> [CObj] -> IO ()
linkExecutable os f objs = do
    f' <- defaultExecName f;
    let f'' = outExecutableName os f';
    putStrLn $ "Linking '" ++ f'' ++ "'.";
    lf os f''
    where
        lf Win32   = W32.link objs
        lf Linux32 = L32.link objs
        
outExecutableName :: TargetOS -> String -> String
outExecutableName Win32   s = if upper ext == "EXE" then s else s ++ ".exe" where (_, ext) = rsplit s "."
outExecutableName Linux32 s = s

defaultExecName :: String -> IO String
defaultExecName s = do
    isDir <- doesDirectoryExist s;
    return $ if isDir then mustEndWith s "/" ++ "jimmy" else s

options :: [OptDescr (Options -> IO Options)]
options =
    [
        Option ['?'] ["help"]       (NoArg   showUsage)                   "describe options for invoking STLCC",
        Option ['V'] ["version"]    (NoArg   showVersion)                 "show STLCC version number",
        Option ['o'] ["output"]     (ReqArg  setOutput "FILE")            "set output filename/path",
        Option ['i'] ["input"]      (NoArg $ setOptMode STLCFiles)        "compile files",
        Option ['a'] ["asm"]        (NoArg $ setOptMode AsmFiles)         "assemble files",
        Option ['w'] ["windows"]    (NoArg $ setTargetOS Win32)           "compile an executable for MS Windows",
        Option ['l'] ["linux"]      (NoArg $ setTargetOS Linux32)         "compile an executable for Linux",
        Option ['x'] ["executable"] (NoArg $ setOutputMode Executable)    "after compilation, link an executable",
        Option ['m'] ["assembly"]   (NoArg $ setOutputMode AssemblyCode)  "after compilation, output assembly code",
        Option ['c'] ["cfg"]        (NoArg $ setOutputMode AnalysisDiags) "output analysis diagrams for input files",
        Option ['n'] ["abstract"]   (NoArg $ setOutputMode AbsMachine)    "output abstract machine code, don't compile",
        Option ['u'] ["unsweeten"]  (NoArg $ setOutputMode STLCCode)      "output desugared STLC, don't compile",
        Option ['p'] ["unparse"]    (NoArg $ setOutputMode ParsedSTLC)    "output parsed STLC, don't compile"
    ]

showVersion _ = do
    putStrLn "STLCC version 1.0.0";
    exitWith ExitSuccess

showUsage _ = do
    putStrLn $ usageInfo programDesc options;
    exitWith ExitSuccess
    
setOutput f opt = do
    return $ opt { outputFile = f }

setOptMode m opt = do
    return $ opt { accumf = m }
    
setTargetOS os opt = do
    return $ opt { targetOS = os }
    
setOutputMode m opt = do
    return $ opt { outputMode = m }
    
accumArg arg opt | accumf opt == STLCFiles = return $ opt {stlcFiles = stlcFiles opt ++ [arg]}
accumArg arg opt | accumf opt == AsmFiles  = return $ opt {asmFiles  = asmFiles  opt ++ [arg]}

mjoin :: Monad m => m a -> [a -> m a] -> m a
mjoin init acts = foldl (>>=) init acts
    