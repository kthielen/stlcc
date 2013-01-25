{
module STLC.Reader where
import STLC.Type
import STLC.Term
import STLC.Check
import Util.ReadPosTracking
import Util.Num
import Util.String
import Util.Recursion

import Data.Char
import GHC.Word
import Control.Monad
import System.IO
}

%name stlc
%tokentype { Tok }
%error { parseError }
%monad { ReadPosTrack }
%lexer { lexExtentTrack lexStep } { TEof }

%token 
      type            { TType }
      mu              { TMuT }
      exists          { TEx }
      extern          { TExtern }
      from            { TFrom }
      let             { TLet }
      in              { TIn }
      case            { TCase }
      of              { TOf }
      roll            { TRoll }
      unroll          { TUnroll }
      pack            { TPack }
      as              { TAs }
      unpack          { TUnpack }
      new             { TNew }
      int             { TInt $$ }
      sym             { TSym $$ }
      str             { TStr $$ }
      unit            { TUnit }
      hasty           { THasTy }
      '->'            { TFnArrow }
      ':'             { TColon }
      ';'             { TSemiColon }
      '.'             { TDot }
      '='             { TEqual }
      '('             { TLParen }
      ')'             { TRParen }
      '<'             { TLT }
      '>'             { TGT }
      '|'             { TBar }
      '['             { TLBracket }
      ']'             { TRBracket }
      '{'             { TLBrace }
      '}'             { TRBrace }
      ','             { TComma }
      '+'             { TPlus }
      '-'             { TMinus }

%%

Prgm : Prgm Def      { $2 : $1 }
     | {- nothing -} { [] }

Def : sym hasty Type '=' Expr ';'        {% reduce 6 (\d -> NamedTerm d $1 [] $3 $5) }
    | sym hasty Type FnDef '=' Expr ';'  {% reduce 7 (\d -> NamedTerm d $1 (reverse $4) $3 $6) }
    | extern sym hasty Type ';'          {% reduce 5 (\d -> Extern d $2 $4) }
    | type sym '=' Type ';'              {% reduce 5 (\d -> TypeAlias d $2 $4) }

FnDef : sym '(' ArgNameSeq ')' {% reduce 4 (const $3) }

ArgNameSeq : ArgNameSeq ',' sym {% reduce 3 (const ($3 : $1)) }
           | sym                {% reduce 1 (const [$1]) }

Type : PType '->' Type          {% reduce 3 (\d -> TFn d CFn [$1] $3) }
     | '(' TyArgl ')' '->' Type {% reduce 5 (\d -> TFn d CFn (reverse $2) $5) }
     | PType                    {% reduce 1 (const $1) }

TyArgl : TyArgl ',' Type {% reduce 3 (const ($3 : $1)) }
       | Type            {% reduce 1 (const [$1]) }

PType : sym                 {% reduce 1 (\d -> if isPrimTy $1 then TPrim d $1 else TVar d $1) }
      | '<' TVDef '>'       {% reduce 3 (\d -> TVariant d (reverse $2)) }
      | '{' TVDef '}'       {% reduce 3 (\d -> TRecord d (reverse $2)) }
      | exists sym '.' Type {% reduce 4 (\d -> TExists d $2 $4) }
      | '[' Type ']'        {% reduce 3 (\d -> TArray d $2) }
      | mu sym '.' Type     {% reduce 4 (\d -> TMu d $2 $4) }

TVDef : TVDef ',' TVDefEnt {% reduce 3 (const ($3 : $1)) }
      | TVDefEnt           {% reduce 1 (const [$1]) }

TVDefEnt : sym ':' Type {% reduce 3 (const ($1, $3)) }

Expr : PExpr '(' ExprArgl ')' {% reduce 4 (\d -> App d $1 (reverse $3)) }
     | PExpr                  {% reduce 1 (const $1) }

ExprArgl : ExprArgl ',' Expr {% reduce 3 (const ($3 : $1)) }
         | Expr              {% reduce 1 (const [$1]) }

PExpr : Const                                      {% reduce 1 (\d -> Prim d $1) }
      | sym                                        {% reduce 1 (\d -> Var d $1) }
      | roll '[' Type ']' Expr                     {% reduce 5 (\d -> Roll d $3 $5) }
      | unroll '[' Type ']' Expr                   {% reduce 5 (\d -> Unroll d $3 $5) }
      | unit                                       {% reduce 1 (\d -> Prim d (CUnit d)) }
      | '(' Expr ')'                               {% reduce 3 (const $2) }
      | let sym '=' Expr in Expr                   {% reduce 6 (\d -> Let d $2 $4 $6) }
      | '<' sym '=' Expr '|' TySeq '>'             {% reduce 7 (\d -> Variant d $2 $4 (reverse $6)) }
      | '{' FDefSeq '}'                            {% reduce 3 (\d -> Record d (reverse $2)) }
      | case Expr of CaseSeq                       {% reduce 4 (\d -> VCase d $2 (reverse $4)) }
      | PExpr '.' sym                              {% reduce 3 (\d -> RProj d $1 $3) }
      | pack Expr as Type                          {% reduce 4 (\d -> Pack d $2 $4) }
      | unpack Expr as '{' sym ',' sym '}' in Expr {% reduce 10 (\d -> Unpack d $2 $5 $7 $10) }
      | '[' ExprSeq ']'                            {% reduce 3 (\d -> Array d (reverse $2)) }
      | PExpr '[' Expr ']'                         {% reduce 4 (\d -> ArrElem d $1 $3) }
      | new Type '[' Expr ']'                      {% reduce 5 (\d -> ArrAlloc d $2 $4) }

Const : '(' ')' {% reduce 2 (\d -> CUnit d) }
      | int     {% reduce 1 (\d -> CInt d $1) }
      | str     {% reduce 1 (\d -> CString d $1) }

TySeq : TySeq ',' TySeqEnt {% reduce 3 (const ($3 : $1)) }
      | TySeqEnt           {% reduce 1 (const [$1]) }

TySeqEnt : sym ':' Type {% reduce 3 (const ($1, $3)) }

FDefSeq : FDefSeq ',' FDefEnt {% reduce 3 (const ($3 : $1)) }
        | FDefEnt             {% reduce 1 (const [$1]) }
        
FDefEnt : sym '=' Expr {% reduce 3 (const ($1, $3)) }

CaseSeq : CaseSeq '|' CaseEnt {% reduce 3 (const ($3 : $1)) }
        | CaseEnt             {% reduce 1 (const [$1]) }
        
CaseEnt : sym sym '.' Expr {% reduce 4 (const ($1, $2, $4)) }

ExprSeq : ExprSeq ',' Expr {% reduce 3 (const ($3 : $1)) }
        | Expr             {% reduce 1 (const [$1]) }

{
data Def a = Extern a String (Ty a) | TypeAlias a String (Ty a) | NamedTerm a String [String] (Ty a) (Term a) deriving (Eq, Show)

data Tok
        = TType
        | TMuT
        | TEx
        | TExtern
        | TFrom
        | TLet
        | TIn
        | TCase
        | TOf
        | TRoll
        | TUnroll
        | TPack
        | TAs
        | TUnpack
        | TNew
        | TInt Int
        | TSym String
        | TStr String
        | TUnit
        | THasTy
        | TDot
        | TEqual
        | TLT
        | TGT
        | TBar
        | TLBracket
        | TRBracket
        | TLParen
        | TRParen
        | TComma
        | TFnArrow
        | TColon
        | TSemiColon
        | TLBrace
        | TRBrace
        | TPlus
        | TMinus
        | TEof
    deriving Show

lexStep :: String -> LexDiff Tok
lexStep []             = lexDiff "" TEof
lexStep ('#':cs)       = dSkipCol 1 (lexComment cs)
lexStep ('0':'x':cs)   = dCol 2 (lexHexStr cs)
lexStep ('\"':cs)      = dCol 1 (lexStr cs)
lexStep (':':':':cs)   = dCol 2 (lexDiff cs THasTy)
lexStep ('-':'>':cs)   = dCol 2 (lexDiff cs TFnArrow)
lexStep (':':cs)       = dCol 1 (lexDiff cs TColon)
lexStep (';':cs)       = dCol 1 (lexDiff cs TSemiColon)
lexStep ('.':cs)       = dCol 1 (lexDiff cs TDot)
lexStep ('=':cs)       = dCol 1 (lexDiff cs TEqual)
lexStep ('(':')':cs)   = dCol 2 (lexDiff cs TUnit)
lexStep ('(':cs)       = dCol 1 (lexDiff cs TLParen)
lexStep (')':cs)       = dCol 1 (lexDiff cs TRParen)
lexStep ('<':cs)       = dCol 1 (lexDiff cs TLT)
lexStep ('>':cs)       = dCol 1 (lexDiff cs TGT)
lexStep ('|':cs)       = dCol 1 (lexDiff cs TBar)
lexStep ('[':cs)       = dCol 1 (lexDiff cs TLBracket)
lexStep (']':cs)       = dCol 1 (lexDiff cs TRBracket)
lexStep ('{':cs)       = dCol 1 (lexDiff cs TLBrace)
lexStep ('}':cs)       = dCol 1 (lexDiff cs TRBrace)
lexStep (',':cs)       = dCol 1 (lexDiff cs TComma)
lexStep ('+':cs)       = dCol 1 (lexDiff cs TPlus)
lexStep ('-':cs)       = dCol 1 (lexDiff cs TMinus)
lexStep ('\n':cs)      = dLine 1 (lexStep cs)
lexStep (c:cs)
      | isSpace   c = dSkipCol 1 (lexStep cs)
      | isDigit   c = lexNum (c:cs)
      | isSymChar c = lexSym (c:cs)
      | otherwise   = error ("Unexpected character: '" ++ show c ++ "'.")

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
lexStr cs = dCol (length str + 1) (lexDiff rest (TStr str)) where
    (str, _:rest) = span (not . (== '\"')) cs

isSymChar :: Char -> Bool
isSymChar c = c `elem` "_$-+^&@" || isAlpha c || isDigit c

lexSym :: String -> LexDiff Tok
lexSym cs = dCol (length sym) (lexDiff rest (select (upper sym))) where
    (sym, rest) = span isSymChar cs
    select "TYPE"   = TType
    select "MU"     = TMuT
    select "EXISTS" = TEx
    select "EXTERN" = TExtern
    select "FROM"   = TFrom
    select "LET"    = TLet
    select "IN"     = TIn
    select "CASE"   = TCase
    select "OF"     = TOf
    select "ROLL"   = TRoll
    select "UNROLL" = TUnroll
    select "PACK"   = TPack
    select "AS"     = TAs
    select "UNPACK" = TUnpack
    select "NEW"    = TNew
    select _        = TSym sym

readSTLCFile :: String -> IO ([(String, Ty FileExtent)], [(String, Ty FileExtent)], [(String, [String], Ty FileExtent, Term FileExtent)])
readSTLCFile fname = do
    h <- openFile fname ReadMode;
    s <- hGetContents h;
    let defs = reverse (applyReadState stlc (initialReadState fname s));
    let tsyms   = fixedPoint (\tsyms -> [(v, expandTy tsyms ty) | (v, ty) <- tsyms]) [(v, ty) | TypeAlias _ v ty <- defs];
    let imports = [(v, expandTy tsyms ty) | Extern    _ v ty <- defs];
    let deftenv = [(v, expandTy tsyms ty) | NamedTerm _ v vs ty _ <- defs];
    let tdefs   = [(v, vs, expandTy tsyms ty, expandTermTy tsyms e)  | NamedTerm _ v vs ty e <- defs];
    
    checkDefinitions [imports ++ deftenv] tdefs (imports, deftenv, tdefs)
--    return (imports, deftenv, tdefs)

readSTLC :: String -> [Def FileExtent]
readSTLC s = reverse (applyReadState stlc (initialReadState "input" s))

parseError :: Tok -> ReadPosTrack a
parseError t = parseFailure ("On token '" ++ show t ++ "'.")
}
