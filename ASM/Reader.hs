{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
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

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Tok)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

action_0 (4) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (4) = happyGoto action_2
action_1 _ = happyFail

action_2 (11) = happyShift action_6
action_2 (13) = happyShift action_7
action_2 (14) = happyShift action_8
action_2 (17) = happyShift action_9
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 _ = happyFail

action_3 (11) = happyShift action_6
action_3 (13) = happyShift action_7
action_3 (14) = happyShift action_8
action_3 (17) = happyShift action_9
action_3 (28) = happyAccept
action_3 (5) = happyGoto action_4
action_3 (6) = happyGoto action_5
action_3 _ = happyFail

action_4 _ = happyReduce_1

action_5 (17) = happyShift action_22
action_5 _ = happyFail

action_6 (10) = happyShift action_15
action_6 (11) = happyShift action_16
action_6 (16) = happyShift action_17
action_6 (18) = happyShift action_18
action_6 (19) = happyShift action_19
action_6 (20) = happyShift action_20
action_6 (23) = happyShift action_21
action_6 (25) = happyReduce_21
action_6 (7) = happyGoto action_12
action_6 (8) = happyGoto action_13
action_6 (9) = happyGoto action_14
action_6 _ = happyReduce_9

action_7 (11) = happyShift action_11
action_7 _ = happyFail

action_8 (11) = happyShift action_10
action_8 _ = happyFail

action_9 _ = happyReduce_4

action_10 (15) = happyShift action_29
action_10 _ = happyFail

action_11 (22) = happyShift action_28
action_11 _ = happyFail

action_12 (27) = happyShift action_27
action_12 _ = happyReduce_10

action_13 _ = happyReduce_12

action_14 (25) = happyShift action_26
action_14 _ = happyFail

action_15 _ = happyReduce_14

action_16 _ = happyReduce_13

action_17 _ = happyReduce_15

action_18 (21) = happyShift action_25
action_18 _ = happyFail

action_19 (21) = happyShift action_24
action_19 _ = happyFail

action_20 (21) = happyShift action_23
action_20 _ = happyFail

action_21 _ = happyReduce_8

action_22 _ = happyReduce_3

action_23 _ = happyReduce_20

action_24 _ = happyReduce_19

action_25 _ = happyReduce_18

action_26 (11) = happyShift action_34
action_26 _ = happyFail

action_27 (10) = happyShift action_15
action_27 (11) = happyShift action_16
action_27 (16) = happyShift action_17
action_27 (18) = happyShift action_18
action_27 (19) = happyShift action_19
action_27 (20) = happyShift action_20
action_27 (8) = happyGoto action_33
action_27 (9) = happyGoto action_14
action_27 _ = happyReduce_21

action_28 (12) = happyShift action_31
action_28 (16) = happyShift action_32
action_28 _ = happyFail

action_29 (12) = happyShift action_30
action_29 _ = happyFail

action_30 _ = happyReduce_5

action_31 _ = happyReduce_6

action_32 _ = happyReduce_7

action_33 _ = happyReduce_11

action_34 (24) = happyShift action_35
action_34 (26) = happyShift action_36
action_34 _ = happyFail

action_35 (10) = happyShift action_37
action_35 _ = happyFail

action_36 _ = happyReduce_16

action_37 (26) = happyShift action_38
action_37 _ = happyFail

action_38 _ = happyReduce_17

happyReduce_1 = happyMonadReduce 2 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\_ -> happy_var_2 : happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happyMonadReduce 0 4 happyReduction_2
happyReduction_2 (happyRest) tk
	 = happyThen (( reduce 0 (\_ -> []))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_3 = happyMonadReduce 2 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\_ -> Just happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_4 = happyMonadReduce 1 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\_ -> Nothing))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_5 = happyMonadReduce 4 6 happyReduction_5
happyReduction_5 ((HappyTerminal (TStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> ImpOp d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_6 = happyMonadReduce 4 6 happyReduction_6
happyReduction_6 ((HappyTerminal (TStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> DLDOp d happy_var_2 (str happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_7 = happyMonadReduce 4 6 happyReduction_7
happyReduction_7 ((HappyTerminal (THexStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> DLDOp d happy_var_2 (readHex happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_8 = happyMonadReduce 2 6 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> CLDOp d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_9 = happyMonadReduce 1 6 happyReduction_9
happyReduction_9 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> InvOp d happy_var_1 []))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_10 = happyMonadReduce 2 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> InvOp d happy_var_1 (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_11 = happyMonadReduce 3 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\_ -> happy_var_3 : happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_12 = happyMonadReduce 1 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\_ -> [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_13 = happyMonadReduce 1 8 happyReduction_13
happyReduction_13 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> AVar d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_14 = happyMonadReduce 1 8 happyReduction_14
happyReduction_14 ((HappyTerminal (TInt happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> AInt d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_15 = happyMonadReduce 1 8 happyReduction_15
happyReduction_15 ((HappyTerminal (THexStr happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> readHexSeq d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_16 = happyMonadReduce 4 8 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> AVRef d happy_var_1 happy_var_3 0))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_17 = happyMonadReduce 6 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyTerminal (TInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 6 (\d -> AVRef d happy_var_1 happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_18 = happyMonadReduce 2 9 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> TPrim d "int"))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_19 = happyMonadReduce 2 9 happyReduction_19
happyReduction_19 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> TPrim d "short"))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_20 = happyMonadReduce 2 9 happyReduction_20
happyReduction_20 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> TPrim d "char"))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_21 = happyMonadReduce 0 9 happyReduction_21
happyReduction_21 (happyRest) tk
	 = happyThen (( reduce 0 (\d -> TPrim d "char"))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyNewToken action sts stk
	= lexExtentTrack lexStep(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEof -> action 28 28 tk (HappyState action) sts stk;
	TInt happy_dollar_dollar -> cont 10;
	TSym happy_dollar_dollar -> cont 11;
	TStr happy_dollar_dollar -> cont 12;
	TData -> cont 13;
	TImport -> cont 14;
	TFrom -> cont 15;
	THexStr happy_dollar_dollar -> cont 16;
	TNewline -> cont 17;
	TDword -> cont 18;
	TWord -> cont 19;
	TByte -> cont 20;
	TPtrT -> cont 21;
	TEqual -> cont 22;
	TColon -> cont 23;
	TPlus -> cont 24;
	TLBracket -> cont 25;
	TRBracket -> cont 26;
	TComma -> cont 27;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => ReadPosTrack a -> (a -> ReadPosTrack b) -> ReadPosTrack b
happyThen = (>>=)
happyReturn :: () => a -> ReadPosTrack a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ReadPosTrack a
happyReturn1 = happyReturn
happyError' :: () => (Tok) -> ReadPosTrack a
happyError' tk = parseError tk

asm = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates\\GenericTemplate.hs" #-}








{-# LINE 49 "templates\\GenericTemplate.hs" #-}

{-# LINE 59 "templates\\GenericTemplate.hs" #-}

{-# LINE 68 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
