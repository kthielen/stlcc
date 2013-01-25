{
module ASM.Reader where
import ASM.Term
import ASM.Type
import ASM.X86_32.Assembler
import Link.ObjFile
import Link.Windows.DLL

import Util.ReadPosTracking
import Util.FileBuf
import Util.String
import Util.Sequence
import Util.Num

import Data.Char
import GHC.Word
import Control.Monad
import System.IO
}
%name asm
%tokentype { Tok }
%error { parseError }
%monad { ReadPosTrack }
%lexer { lexExtentTrack lexStep } { TEof }
%token 
      int             { TInt $$ }
      sym             { TSym $$ }
      str             { TStr $$ }
      tdata           { TData }
      timport         { TImport }
      tfrom           { TFrom }
      hexseq          { THexStr $$ }
      newline         { TNewline }
      dword           { TDword }
      word            { TWord }
      byte            { TByte }
      ptr             { TPtrT }
      '='             { TEqual }
      ':'             { TColon }
      '+'             { TPlus }
      '['             { TLBracket }
      ']'             { TRBracket }
      ','             { TComma }
%%
AsmPrgm : AsmPrgm AsmLine {% reduce 2 (\_ -> $2 : $1) }
        | {- nothing -}   {% reduce 0 (\_ -> []) }

AsmLine : AsmOp newline {% reduce 2 (\_ -> Just $1) }
        | newline       {% reduce 1 (\_ -> Nothing) }

AsmOp : timport sym tfrom str    {% reduce 4 (\d -> ImpOp d $2 $4) }
      | tdata sym '=' str        {% reduce 4 (\d -> DLDOp d $2 (str $4)) }
      | tdata sym '=' hexseq     {% reduce 4 (\d -> DLDOp d $2 (readHex $4)) }
      | sym ':'                  {% reduce 2 (\d -> CLDOp d $1) }
      | sym                      {% reduce 1 (\d -> InvOp d $1 []) }
      | sym OpArgl               {% reduce 2 (\d -> InvOp d $1 (reverse $2)) }

OpArgl: OpArgl ',' OpArg {% reduce 3 (\_ -> $3 : $1) }
      | OpArg            {% reduce 1 (\_ -> [$1]) }

OpArg: sym                         {% reduce 1 (\d -> AVar d $1) }
     | int                         {% reduce 1 (\d -> AInt d $1) }
     | hexseq                      {% reduce 1 (\d -> readHexSeq d $1) }
     | PtrSpec '[' sym ']'         {% reduce 4 (\d -> AVRef d $1 $3 0) }
     | PtrSpec '[' sym '+' int ']' {% reduce 6 (\d -> AVRef d $1 $3 $5) }

PtrSpec: dword ptr     {% reduce 2 (\d -> TPrim d "int") }
       | word ptr      {% reduce 2 (\d -> TPrim d "short") }
       | byte ptr      {% reduce 2 (\d -> TPrim d "char") }
       | {- empty -}   {% reduce 0 (\d -> TPrim d "char") }

{
parseError :: Tok -> ReadPosTrack a
parseError t = parseFailure ("On token '" ++ show t ++ "'.")

data Exp a
      = DLDOp a String [Word8]
      | CLDOp a String
      | ImpOp a String String
      | InvOp a String [EOpArg a]
    deriving Show

data EOpArg a
      = AVar a String
      | AByte a Int
      | AShort a Int
      | AInt a Int
      | AVRef a (Ty a) String Int
    deriving Show

data Tok
      = TInt Int
      | TSym String
      | TStr String
      | TData
      | TImport
      | TFrom
      | THexStr String
      | TDword
      | TWord
      | TByte
      | TPtrT
      | TEqual
      | TNewline
      | TColon
      | TPlus
      | TLBracket
      | TRBracket
      | TComma
      | TEof
    deriving Show

lexStep :: String -> LexDiff Tok
lexStep []             = lexDiff "" TEof
lexStep ('\r':'\n':cs) = dLine 1 (lexDiff cs TNewline)
lexStep ('\n':cs)      = dLine 1 (lexDiff cs TNewline)
lexStep ('\r':cs)      = dLine 1 (lexDiff cs TNewline)
lexStep (';':cs)       = dSkipCol 1 (lexComment cs)
lexStep ('\'':cs)      = dCol 1 (lexStr cs)
lexStep ('0':'x':cs)   = dCol 2 (lexHexStr cs)
lexStep ('=':cs)       = dCol 1 (lexDiff cs TEqual)
lexStep (':':cs)       = dCol 1 (lexDiff cs TColon)
lexStep ('+':cs)       = dCol 1 (lexDiff cs TPlus)
lexStep ('[':cs)       = dCol 1 (lexDiff cs TLBracket)
lexStep (']':cs)       = dCol 1 (lexDiff cs TRBracket)
lexStep (',':cs)       = dCol 1 (lexDiff cs TComma)
lexStep (c:cs)
      | isSpace   c = dSkipCol 1 (lexStep cs)
      | isDigit   c = lexNum (c:cs)
      | isSymChar c = lexSym (c:cs)
      
lexComment :: String -> LexDiff Tok
lexComment cs = dLine 1 (dSkipCol c (lexStep rest)) where
    (c, rest) = breakNewline 0 cs
    breakNewline n ('\r':'\n':cs) = (n, cs)
    breakNewline n ('\r':cs)      = (n, cs)
    breakNewline n ('\n':cs)      = (n, cs)
    breakNewline n (_:cs)         = breakNewline (n+1) cs
    breakNewline n []             = (n, [])
      
lexNum cs = dCol (length num) (lexDiff rest (TInt (read num))) where
    (num,rest) = span isDigit cs

lexHexStr cs = dCol (length pfx) (lexDiff rest (THexStr pfx)) where
    (pfx, rest) = span isHexDigit cs
    isHexDigit c | within c 'a' 'f' = True
    isHexDigit c | within c 'A' 'F' = True
    isHexDigit c                    = isDigit c

lexStr cs = dCol (length pfx) (lexDiff rest (TStr pfx)) where
    (pfx, _:rest) = span (not . (== '\'')) cs
    
lexSym cs = dCol (length sym) (lexDiff rest (select (upper sym))) where
    (sym, rest) = span isSymChar cs
    select "DATA"   = TData
    select "IMPORT" = TImport
    select "FROM"   = TFrom
    select "DWORD"  = TDword
    select "WORD"   = TWord
    select "BYTE"   = TByte
    select "PTR"    = TPtrT
    select _        = TSym sym

isSymChar c = isAlpha c || isDigit c || c `elem` "_/&$!@"

readHexSeq :: a -> String -> EOpArg a
readHexSeq p [h,l]             = AByte  p (castn (r8 (readHex [h,l])))
readHexSeq p [a,b,c,d]         = AShort p (castn (r16 (readHex [a,b,c,d])))
readHexSeq p [a,b,c,d,e,f,g,h] = AInt   p (castn (r32 (readHex [a,b,c,d,e,f,g,h])))
readHexSeq p s                 = error ("Misaligned hexadecimal constant: " ++ s)

importPairs :: [Exp a] -> [(String, String)]
importPairs ((ImpOp _ sym dllname):rs) = (dllname, sym) : importPairs rs
importPairs (_:rs)                     = importPairs rs
importPairs []                         = []

importObjects :: [Exp a] -> [CObj]
importObjects exps = map (uncurry importObject) (group (importPairs exps))

translateExprs :: [Exp a] -> [Inst a]
translateExprs ((DLDOp p ln dt):rs)  = DataDef p ln dt : translateExprs rs
translateExprs ((CLDOp p ln):rs)     = LblDef p ln : translateExprs rs
translateExprs ((ImpOp _ _ _):rs)    = translateExprs rs
translateExprs ((InvOp p o args):rs) = Op p o [] [] (\_ _ _ -> map translateArg args) [] : translateExprs rs
translateExprs []                    = []

translateArg :: EOpArg a -> OpArg a
translateArg (AVar p rn)       = collapseEither (translateSym p rn)
translateArg (AByte  p v)      = OAConst p (CByte p $ castn v)
translateArg (AShort p v)      = OAConst p (CShort p v)
translateArg (AInt   p v)      = OAConst p (CInt p v)
translateArg (AVRef p ty rn 0) = deref (translateSym p rn) where
    deref (Left (OAReg p _ rn))     = OARMemOff p ty rn 0
    deref (Right (OACConst p _ vn)) = OACCMemOff p ty vn
translateArg (AVRef p ty rn n) = deref (translateSym p rn) where
    deref (Left (OAReg p _ rn)) = OARMemOff p ty rn n

translateSym :: a -> String -> Either (OpArg a) (OpArg a)
translateSym p "eax" = Left $ OAReg p (TPrim p "int")   "ax"
translateSym p "ebx" = Left $ OAReg p (TPrim p "int")   "bx"
translateSym p "ecx" = Left $ OAReg p (TPrim p "int")   "cx"
translateSym p "edx" = Left $ OAReg p (TPrim p "int")   "dx"
translateSym p "ebp" = Left $ OAReg p (TPrim p "int")   "bp"
translateSym p "esp" = Left $ OAReg p (TPrim p "int")   "sp"
translateSym p "edi" = Left $ OAReg p (TPrim p "int")   "di"
translateSym p "esi" = Left $ OAReg p (TPrim p "int")   "si"
translateSym p "ax"  = Left $ OAReg p (TPrim p "short") "ax"
translateSym p "bx"  = Left $ OAReg p (TPrim p "short") "bx"
translateSym p "cx"  = Left $ OAReg p (TPrim p "short") "cx"
translateSym p "dx"  = Left $ OAReg p (TPrim p "short") "dx"
translateSym p "bp"  = Left $ OAReg p (TPrim p "short") "bp"
translateSym p "sp"  = Left $ OAReg p (TPrim p "short") "sp"
translateSym p "di"  = Left $ OAReg p (TPrim p "short") "di"
translateSym p "si"  = Left $ OAReg p (TPrim p "short") "si"
translateSym p "al"  = Left $ OAReg p (TPrim p "char")  "ax"
translateSym p "bl"  = Left $ OAReg p (TPrim p "char")  "bx"
translateSym p "cl"  = Left $ OAReg p (TPrim p "char")  "cx"
translateSym p "dl"  = Left $ OAReg p (TPrim p "char")  "dx"
translateSym p "ch"  = Left $ OAReg p (TPrim p "char")  "bp"
translateSym p "ah"  = Left $ OAReg p (TPrim p "char")  "sp"
translateSym p "bh"  = Left $ OAReg p (TPrim p "char")  "di"
translateSym p "dh"  = Left $ OAReg p (TPrim p "char")  "si"
translateSym p sym   = Right $ OACConst p (TPrim p "int") sym

translateAsmFile :: String -> IO [CObj]
translateAsmFile fname = do
    h <- openFile fname ReadMode;
    s <- hGetContents h;
    let exps = readAsm fname (s++"\n");
    return ((assembleUnit $ translateExprs exps) : importObjects exps)

readAsm :: String -> String -> [Exp FileExtent]
readAsm file text = (map uj . filter isJust . reverse) (applyReadState asm (initialReadState file text)) where
    isJust (Just _) = True
    isJust Nothing  = False

}
