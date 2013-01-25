{
module AML.Reader where
import AML.Type
import AML.Term
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

%name aml
%tokentype { Tok }
%error { parseError }
%monad { ReadPosTrack }
%lexer { lexExtentTrack lexStep } { TEof }
%token 
    ';'    { TSemiColon }
    '<-'   { TLArrow }
    '->'   { TRArrow }
    arg    { TArg }
    do     { TDo }
    int    { TInt $$ }
    str    { TStr $$ }
    sym    { TSym $$ }
    '::'   { THasTy }
    ':'    { TColon }
    jump   { TJump }
    jumpif { TJumpIf }
    uop    { TUop $$ }
    bop    { TBop $$ }
    rop    { TRop $$ }
    else   { TElse }
    return { TReturn }
    new    { TNew }
    exists { TEx }
    mu     { TMuT }
    '^'    { TCaret }
    '.'    { TDot }
    '<'    { TLt }
    '>'    { TGt }
    '['    { TLBracket }
    ']'    { TRBracket }
    '('    { TLParen }
    ')'    { TRParen }
    '{'    { TLBracket }
    '}'    { TRBracket }
    ','    { TComma }
    
%%

AmlPrgm : AmlPrgm Stmt  { $2 : $1 }
        | {- nothing -} { [] }

Stmt : Exp '<-' Exp ';'                    {% reduce 4 (\d -> Move d $1 $3) }
     | arg sym '::' Ty ';'                 {% reduce 5 (\d -> Arg d $4 $2) }
     | sym ':'                             {% reduce 2 (\d -> Label d $1) }
     | jump sym ';'                        {% reduce 3 (\d -> Jump d $2) }
     | jumpif Exp '<' Exp sym else sym ';' {% reduce 8 (\d -> CJump d RLt $2 $4 $5 $7) }
     | jumpif Exp '>' Exp sym else sym ';' {% reduce 8 (\d -> CJump d RGt $2 $4 $5 $7) }
     | jumpif Exp rop Exp sym else sym ';' {% reduce 8 (\d -> CJump d $3 $2 $4 $5 $7) }
     | return Exp ';'                      {% reduce 3 (\d -> Return d $2) }
     | do '{' AmlPrgm '}'                  {% reduce 4 (\d -> sseq $3) }
     | Exp ';'                             {% reduce 2 (\d -> EStmt d $1) }

Exp : new Ty                         {% reduce 2 (\d -> Malloc d Heap $2 0) }
    | new Ty '[' int ']'             {% reduce 5 (\d -> Malloc d Heap $2 $4) }
    | sym                            {% reduce 1 (\d -> LName d $1) }
    | Const                          {% reduce 1 (\d -> Const d $1) }
    | '[' Exp ']' '::' Ty            {% reduce 5 (\d -> Mem d $5 $2) }
    | sym '::' Ty                    {% reduce 3 (\d -> TReg d $3 $1) }
    | uop Exp                        {% reduce 2 (\d -> UOp d $1 $2) }
    | Exp '<' '<' Exp                {% reduce 4 (\d -> BOp d BShiftL $1 $4) }
    | Exp '>' '>' Exp                {% reduce 4 (\d -> BOp d BShiftR $1 $4) }
    | Exp bop Exp                    {% reduce 3 (\d -> BOp d $2 $1 $3) }
    | Exp '(' ExpSeq ')' '::' Ty     {% reduce 6 (\d -> Call d $6 $1 (reverse $3)) }
    | '{' Stmt ',' Exp '}'           {% reduce 5 (\d -> ESeq d $2 $4) }

ExpSeq : ExpSeq ',' Exp {% reduce 3 (const ($3 : $1)) }
       | Exp            {% reduce 1 (const [$1]) }

Const : '(' ')' {% reduce 2 (\d -> CUnit d) }
      | int     {% reduce 1 (\d -> CInt d $1) }
      | str     {% reduce 1 (\d -> CString d $1) }

Ty : PTy '->' Ty {% reduce 3 (\d -> TFn d CFn [$1] $3) }
   | PTy         {% reduce 1 (const $1) }

PTy : sym                 {% reduce 1 (\d -> if isPrimTy $1 then TPrim d $1 else TVar d $1) }
    | '(' Ty ')'          {% reduce 3 (const $2) }
    | '(' ')'             {% reduce 2 (\d -> TPrim d "unit") }
    | '<' TVDef '>'       {% reduce 3 (\d -> TVariant d (reverse $2)) }
    | '{' TVDef '}'       {% reduce 3 (\d -> TRecord d (reverse $2)) }
    | exists sym '.' Ty   {% reduce 4 (\d -> TExists d $2 $4) }
    | '[' Ty ']'          {% reduce 3 (\d -> TArray d $2) }
    | mu sym '.' Ty       {% reduce 4 (\d -> TMu d $2 $4) }
    | '^' Ty              {% reduce 2 (\d -> TPtr d $2) }

TVDef : TVDef ',' TVDefEnt {% reduce 3 (const ($3 : $1)) }
      | TVDefEnt           {% reduce 1 (const [$1]) }

TVDefEnt : sym ':' Ty {% reduce 3 (const ($1, $3)) }
      
{
data Tok = TSemiColon
         | TArg
         | TDo
         | TInt Int
         | TStr String
         | TSym String
         | THasTy
         | TColon
         | TJump
         | TJumpIf
         | TUop UOp
         | TBop BOp
         | TRop ROp
         | TElse
         | TReturn
         | TNew
         | TEx
         | TMuT
         | TCaret
         | TDot
         | TLt
         | TGt
         | TLArrow
         | TRArrow
         | TLBracket
         | TRBracket
         | TLParen
         | TRParen
         | TLBrace
         | TRBrace
         | TComma
         | TEof
         deriving (Eq, Show)

lexStep :: String -> LexDiff Tok
lexStep []             = lexDiff "" TEof
lexStep ('#':cs)       = dSkipCol 1 (lexComment cs)
lexStep ('0':'x':cs)   = dCol 2 (lexHexStr cs)
lexStep ('\"':cs)      = lexStr cs
lexStep ('<':'-':cs)   = dCol 2 (lexDiff cs TLArrow)
lexStep ('-':'>':cs)   = dCol 2 (lexDiff cs TRArrow)
lexStep (':':':':cs)   = dCol 2 (lexDiff cs THasTy)
lexStep (':':cs)       = dCol 1 (lexDiff cs TColon)
lexStep (';':cs)       = dCol 1 (lexDiff cs TSemiColon)
lexStep ('^':cs)       = dCol 1 (lexDiff cs TCaret)
lexStep ('.':cs)       = dCol 1 (lexDiff cs TDot)
lexStep ('<':'=':cs)   = dCol 2 (lexDiff cs (TRop RLte))
lexStep ('<':cs)       = dCol 1 (lexDiff cs TLt)
lexStep ('>':'=':cs)   = dCol 2 (lexDiff cs (TRop RGte))
lexStep ('>':cs)       = dCol 1 (lexDiff cs TGt)
lexStep ('[':cs)       = dCol 1 (lexDiff cs TLBracket)
lexStep (']':cs)       = dCol 1 (lexDiff cs TRBracket)
lexStep ('(':cs)       = dCol 1 (lexDiff cs TLParen)
lexStep (')':cs)       = dCol 1 (lexDiff cs TRParen)
lexStep ('{':cs)       = dCol 1 (lexDiff cs TLBrace)
lexStep ('}':cs)       = dCol 1 (lexDiff cs TRBrace)
lexStep (',':cs)       = dCol 1 (lexDiff cs TComma)
lexStep ('\n':cs)      = dLine 1 (lexStep cs)
lexStep (c:cs)
      | isSpace   c = dSkipCol 1 (lexStep cs)
      | isDigit   c = lexNum (c:cs)
      | isSymChar c = lexSym (c:cs)
      | otherwise   = error $ "Unexpected character: '" ++ show c ++ "'."

lexComment :: String -> LexDiff Tok
lexComment cs = dLine 1 (dSkipCol c (lexStep rest)) where
    (c, rest) = breakNewline 0 cs
    breakNewline n ('\r':'\n':cs) = (n, cs)
    breakNewline n ('\r':cs)      = (n, cs)
    breakNewline n ('\n':cs)      = (n, cs)
    breakNewline n (_:cs)         = breakNewline (n+1) cs
    breakNewline n []             = (n, [])

lexNum :: String -> LexDiff Tok
lexNum cs = dCol (length num) (lexDiff rest (TInt (read num))) where
    (num, rest) = span isDigit cs

lexHexStr :: String -> LexDiff Tok
lexHexStr cs = dCol (length pfx) (lexDiff rest (TInt (read ("0x" ++ pfx)))) where
    (pfx, rest) = span isHexDigit cs
    isHexDigit c = within c 'a' 'f' || within c 'A' 'F' || isDigit c

lexStr :: String -> LexDiff Tok
lexStr cs = dCol (length pfx) (lexDiff rest (TStr pfx)) where
    (pfx, _:rest) = span (not . (== '\"')) cs

isSymChar :: Char -> Bool
isSymChar c = c `elem` "~!<>=_$-+^&|@" || isAlpha c || isDigit c

lexSym :: String -> LexDiff Tok
lexSym cs = dCol (length sym) (lexDiff rest (select (upper sym))) where
    (sym, rest) = span isSymChar cs
    select "ARG"    = TArg
    select "DO"     = TDo
    select "JUMP"   = TJump
    select "JUMPIF" = TJumpIf
    select "ELSE"   = TElse
    select "RETURN" = TReturn
    select "NEW"    = TNew
    select "EXISTS" = TEx
    select "MU"     = TMuT
    select "~"      = TUop UNeg
    select "-@"     = TUop ULoByte
    select "+@"     = TUop UHiByte
    select "-@@"    = TUop ULoShort
    select "+@@"    = TUop UHiShort
    select "@^@@"   = TUop UCastByteToShort
    select "@@^@"   = TUop UCastShortToByte
    select "+"      = TBop BAdd
    select "-"      = TBop BSub
    select "*"      = TBop BMul
    select "/"      = TBop BDiv
    select "%"      = TBop BMod
    select "|"      = TBop BOr
    select "&"      = TBop BAnd
    select "^"      = TBop BXor
    select "="      = TRop REq
    select "<>"     = TRop RNeq
    select "><"     = TRop RNeq
    select "!="     = TRop RNeq
    select "<"      = TRop RLt
    select "<="     = TRop RLte
    select ">"      = TRop RGt
    select ">="     = TRop RGte
    select _        = TSym sym

readAMLFile :: String -> IO [Stmt FileExtent]
readAMLFile fname = do
    h <- openFile fname ReadMode;
    s <- hGetContents h;
    return (reverse (applyReadState aml (initialReadState fname s)))

readAML :: String -> [Stmt FileExtent]
readAML s = reverse (applyReadState aml (initialReadState "input" s))

parseError :: Tok -> ReadPosTrack a
parseError t = parseFailure ("On token '" ++ show t ++ "'.")
}
