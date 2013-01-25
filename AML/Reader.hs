{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
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

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Tok)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (4) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (4) = happyGoto action_2
action_1 _ = happyFail

action_2 (16) = happyShift action_7
action_2 (17) = happyShift action_8
action_2 (18) = happyShift action_9
action_2 (19) = happyShift action_10
action_2 (20) = happyShift action_11
action_2 (23) = happyShift action_12
action_2 (24) = happyShift action_13
action_2 (25) = happyShift action_14
action_2 (29) = happyShift action_15
action_2 (30) = happyShift action_16
action_2 (37) = happyShift action_17
action_2 (39) = happyShift action_18
action_2 (41) = happyShift action_19
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 _ = happyFail

action_3 (16) = happyShift action_7
action_3 (17) = happyShift action_8
action_3 (18) = happyShift action_9
action_3 (19) = happyShift action_10
action_3 (20) = happyShift action_11
action_3 (23) = happyShift action_12
action_3 (24) = happyShift action_13
action_3 (25) = happyShift action_14
action_3 (29) = happyShift action_15
action_3 (30) = happyShift action_16
action_3 (37) = happyShift action_17
action_3 (39) = happyShift action_18
action_3 (41) = happyShift action_19
action_3 (44) = happyAccept
action_3 (5) = happyGoto action_4
action_3 (6) = happyGoto action_5
action_3 (8) = happyGoto action_6
action_3 _ = happyFail

action_4 _ = happyReduce_1

action_5 (13) = happyShift action_42
action_5 (14) = happyShift action_43
action_5 (26) = happyShift action_44
action_5 (35) = happyShift action_45
action_5 (36) = happyShift action_46
action_5 (39) = happyShift action_47
action_5 _ = happyFail

action_6 _ = happyReduce_16

action_7 (20) = happyShift action_41
action_7 _ = happyFail

action_8 (41) = happyShift action_40
action_8 _ = happyFail

action_9 _ = happyReduce_28

action_10 _ = happyReduce_29

action_11 (21) = happyShift action_38
action_11 (22) = happyShift action_39
action_11 _ = happyReduce_15

action_12 (20) = happyShift action_37
action_12 _ = happyFail

action_13 (18) = happyShift action_9
action_13 (19) = happyShift action_10
action_13 (20) = happyShift action_23
action_13 (25) = happyShift action_14
action_13 (30) = happyShift action_16
action_13 (37) = happyShift action_17
action_13 (39) = happyShift action_18
action_13 (41) = happyShift action_19
action_13 (6) = happyGoto action_36
action_13 (8) = happyGoto action_6
action_13 _ = happyFail

action_14 (18) = happyShift action_9
action_14 (19) = happyShift action_10
action_14 (20) = happyShift action_23
action_14 (25) = happyShift action_14
action_14 (30) = happyShift action_16
action_14 (37) = happyShift action_17
action_14 (39) = happyShift action_18
action_14 (41) = happyShift action_19
action_14 (6) = happyGoto action_35
action_14 (8) = happyGoto action_6
action_14 _ = happyFail

action_15 (18) = happyShift action_9
action_15 (19) = happyShift action_10
action_15 (20) = happyShift action_23
action_15 (25) = happyShift action_14
action_15 (30) = happyShift action_16
action_15 (37) = happyShift action_17
action_15 (39) = happyShift action_18
action_15 (41) = happyShift action_19
action_15 (6) = happyGoto action_34
action_15 (8) = happyGoto action_6
action_15 _ = happyFail

action_16 (20) = happyShift action_26
action_16 (31) = happyShift action_27
action_16 (32) = happyShift action_28
action_16 (33) = happyShift action_29
action_16 (35) = happyShift action_30
action_16 (37) = happyShift action_31
action_16 (39) = happyShift action_32
action_16 (41) = happyShift action_33
action_16 (9) = happyGoto action_24
action_16 (10) = happyGoto action_25
action_16 _ = happyFail

action_17 (18) = happyShift action_9
action_17 (19) = happyShift action_10
action_17 (20) = happyShift action_23
action_17 (25) = happyShift action_14
action_17 (30) = happyShift action_16
action_17 (37) = happyShift action_17
action_17 (39) = happyShift action_18
action_17 (41) = happyShift action_19
action_17 (6) = happyGoto action_22
action_17 (8) = happyGoto action_6
action_17 _ = happyFail

action_18 (40) = happyShift action_21
action_18 _ = happyFail

action_19 (16) = happyShift action_7
action_19 (17) = happyShift action_8
action_19 (18) = happyShift action_9
action_19 (19) = happyShift action_10
action_19 (20) = happyShift action_11
action_19 (23) = happyShift action_12
action_19 (24) = happyShift action_13
action_19 (25) = happyShift action_14
action_19 (29) = happyShift action_15
action_19 (30) = happyShift action_16
action_19 (37) = happyShift action_17
action_19 (39) = happyShift action_18
action_19 (41) = happyShift action_19
action_19 (5) = happyGoto action_20
action_19 (6) = happyGoto action_5
action_19 (8) = happyGoto action_6
action_19 _ = happyFail

action_20 (43) = happyShift action_75
action_20 _ = happyFail

action_21 _ = happyReduce_27

action_22 (26) = happyShift action_44
action_22 (35) = happyShift action_45
action_22 (36) = happyShift action_46
action_22 (38) = happyShift action_74
action_22 (39) = happyShift action_47
action_22 _ = happyFail

action_23 (21) = happyShift action_38
action_23 _ = happyReduce_15

action_24 (37) = happyShift action_73
action_24 _ = happyReduce_13

action_25 (15) = happyShift action_72
action_25 _ = happyReduce_31

action_26 _ = happyReduce_32

action_27 (20) = happyShift action_71
action_27 _ = happyFail

action_28 (20) = happyShift action_70
action_28 _ = happyFail

action_29 (20) = happyShift action_26
action_29 (31) = happyShift action_27
action_29 (32) = happyShift action_28
action_29 (33) = happyShift action_29
action_29 (35) = happyShift action_30
action_29 (37) = happyShift action_31
action_29 (39) = happyShift action_32
action_29 (41) = happyShift action_33
action_29 (9) = happyGoto action_69
action_29 (10) = happyGoto action_25
action_29 _ = happyFail

action_30 (20) = happyShift action_64
action_30 (11) = happyGoto action_68
action_30 (12) = happyGoto action_63
action_30 _ = happyFail

action_31 (20) = happyShift action_26
action_31 (31) = happyShift action_27
action_31 (32) = happyShift action_28
action_31 (33) = happyShift action_29
action_31 (35) = happyShift action_30
action_31 (37) = happyShift action_31
action_31 (39) = happyShift action_32
action_31 (41) = happyShift action_33
action_31 (9) = happyGoto action_67
action_31 (10) = happyGoto action_25
action_31 _ = happyFail

action_32 (20) = happyShift action_26
action_32 (31) = happyShift action_27
action_32 (32) = happyShift action_28
action_32 (33) = happyShift action_29
action_32 (35) = happyShift action_30
action_32 (37) = happyShift action_31
action_32 (39) = happyShift action_32
action_32 (40) = happyShift action_66
action_32 (41) = happyShift action_33
action_32 (9) = happyGoto action_65
action_32 (10) = happyGoto action_25
action_32 _ = happyFail

action_33 (20) = happyShift action_64
action_33 (11) = happyGoto action_62
action_33 (12) = happyGoto action_63
action_33 _ = happyFail

action_34 (13) = happyShift action_61
action_34 (26) = happyShift action_44
action_34 (35) = happyShift action_45
action_34 (36) = happyShift action_46
action_34 (39) = happyShift action_47
action_34 _ = happyFail

action_35 (26) = happyShift action_44
action_35 (35) = happyShift action_45
action_35 (36) = happyShift action_46
action_35 (39) = happyShift action_47
action_35 _ = happyReduce_19

action_36 (26) = happyShift action_44
action_36 (27) = happyShift action_58
action_36 (35) = happyShift action_59
action_36 (36) = happyShift action_60
action_36 (39) = happyShift action_47
action_36 _ = happyFail

action_37 (13) = happyShift action_57
action_37 _ = happyFail

action_38 (20) = happyShift action_26
action_38 (31) = happyShift action_27
action_38 (32) = happyShift action_28
action_38 (33) = happyShift action_29
action_38 (35) = happyShift action_30
action_38 (37) = happyShift action_31
action_38 (39) = happyShift action_32
action_38 (41) = happyShift action_33
action_38 (9) = happyGoto action_56
action_38 (10) = happyGoto action_25
action_38 _ = happyFail

action_39 _ = happyReduce_5

action_40 (4) = happyGoto action_55
action_40 _ = happyReduce_2

action_41 (21) = happyShift action_54
action_41 _ = happyFail

action_42 _ = happyReduce_12

action_43 (18) = happyShift action_9
action_43 (19) = happyShift action_10
action_43 (20) = happyShift action_23
action_43 (25) = happyShift action_14
action_43 (30) = happyShift action_16
action_43 (37) = happyShift action_17
action_43 (39) = happyShift action_18
action_43 (41) = happyShift action_19
action_43 (6) = happyGoto action_53
action_43 (8) = happyGoto action_6
action_43 _ = happyFail

action_44 (18) = happyShift action_9
action_44 (19) = happyShift action_10
action_44 (20) = happyShift action_23
action_44 (25) = happyShift action_14
action_44 (30) = happyShift action_16
action_44 (37) = happyShift action_17
action_44 (39) = happyShift action_18
action_44 (41) = happyShift action_19
action_44 (6) = happyGoto action_52
action_44 (8) = happyGoto action_6
action_44 _ = happyFail

action_45 (35) = happyShift action_51
action_45 _ = happyFail

action_46 (36) = happyShift action_50
action_46 _ = happyFail

action_47 (18) = happyShift action_9
action_47 (19) = happyShift action_10
action_47 (20) = happyShift action_23
action_47 (25) = happyShift action_14
action_47 (30) = happyShift action_16
action_47 (37) = happyShift action_17
action_47 (39) = happyShift action_18
action_47 (41) = happyShift action_19
action_47 (6) = happyGoto action_48
action_47 (7) = happyGoto action_49
action_47 (8) = happyGoto action_6
action_47 _ = happyFail

action_48 (26) = happyShift action_44
action_48 (35) = happyShift action_45
action_48 (36) = happyShift action_46
action_48 (39) = happyShift action_47
action_48 _ = happyReduce_26

action_49 (40) = happyShift action_96
action_49 (43) = happyShift action_97
action_49 _ = happyFail

action_50 (18) = happyShift action_9
action_50 (19) = happyShift action_10
action_50 (20) = happyShift action_23
action_50 (25) = happyShift action_14
action_50 (30) = happyShift action_16
action_50 (37) = happyShift action_17
action_50 (39) = happyShift action_18
action_50 (41) = happyShift action_19
action_50 (6) = happyGoto action_95
action_50 (8) = happyGoto action_6
action_50 _ = happyFail

action_51 (18) = happyShift action_9
action_51 (19) = happyShift action_10
action_51 (20) = happyShift action_23
action_51 (25) = happyShift action_14
action_51 (30) = happyShift action_16
action_51 (37) = happyShift action_17
action_51 (39) = happyShift action_18
action_51 (41) = happyShift action_19
action_51 (6) = happyGoto action_94
action_51 (8) = happyGoto action_6
action_51 _ = happyFail

action_52 (26) = happyShift action_44
action_52 (35) = happyShift action_45
action_52 (36) = happyShift action_46
action_52 (39) = happyShift action_47
action_52 _ = happyReduce_22

action_53 (13) = happyShift action_93
action_53 (26) = happyShift action_44
action_53 (35) = happyShift action_45
action_53 (36) = happyShift action_46
action_53 (39) = happyShift action_47
action_53 _ = happyFail

action_54 (20) = happyShift action_26
action_54 (31) = happyShift action_27
action_54 (32) = happyShift action_28
action_54 (33) = happyShift action_29
action_54 (35) = happyShift action_30
action_54 (37) = happyShift action_31
action_54 (39) = happyShift action_32
action_54 (41) = happyShift action_33
action_54 (9) = happyGoto action_92
action_54 (10) = happyGoto action_25
action_54 _ = happyFail

action_55 (16) = happyShift action_7
action_55 (17) = happyShift action_8
action_55 (18) = happyShift action_9
action_55 (19) = happyShift action_10
action_55 (20) = happyShift action_11
action_55 (23) = happyShift action_12
action_55 (24) = happyShift action_13
action_55 (25) = happyShift action_14
action_55 (29) = happyShift action_15
action_55 (30) = happyShift action_16
action_55 (37) = happyShift action_17
action_55 (39) = happyShift action_18
action_55 (41) = happyShift action_19
action_55 (42) = happyShift action_91
action_55 (5) = happyGoto action_4
action_55 (6) = happyGoto action_5
action_55 (8) = happyGoto action_6
action_55 _ = happyFail

action_56 _ = happyReduce_18

action_57 _ = happyReduce_6

action_58 (18) = happyShift action_9
action_58 (19) = happyShift action_10
action_58 (20) = happyShift action_23
action_58 (25) = happyShift action_14
action_58 (30) = happyShift action_16
action_58 (37) = happyShift action_17
action_58 (39) = happyShift action_18
action_58 (41) = happyShift action_19
action_58 (6) = happyGoto action_90
action_58 (8) = happyGoto action_6
action_58 _ = happyFail

action_59 (18) = happyShift action_9
action_59 (19) = happyShift action_10
action_59 (20) = happyShift action_23
action_59 (25) = happyShift action_14
action_59 (30) = happyShift action_16
action_59 (35) = happyShift action_51
action_59 (37) = happyShift action_17
action_59 (39) = happyShift action_18
action_59 (41) = happyShift action_19
action_59 (6) = happyGoto action_89
action_59 (8) = happyGoto action_6
action_59 _ = happyFail

action_60 (18) = happyShift action_9
action_60 (19) = happyShift action_10
action_60 (20) = happyShift action_23
action_60 (25) = happyShift action_14
action_60 (30) = happyShift action_16
action_60 (36) = happyShift action_50
action_60 (37) = happyShift action_17
action_60 (39) = happyShift action_18
action_60 (41) = happyShift action_19
action_60 (6) = happyGoto action_88
action_60 (8) = happyGoto action_6
action_60 _ = happyFail

action_61 _ = happyReduce_10

action_62 (42) = happyShift action_87
action_62 (43) = happyShift action_83
action_62 _ = happyFail

action_63 _ = happyReduce_42

action_64 (22) = happyShift action_86
action_64 _ = happyFail

action_65 (40) = happyShift action_85
action_65 _ = happyFail

action_66 _ = happyReduce_34

action_67 (38) = happyShift action_84
action_67 _ = happyFail

action_68 (36) = happyShift action_82
action_68 (43) = happyShift action_83
action_68 _ = happyFail

action_69 _ = happyReduce_40

action_70 (34) = happyShift action_81
action_70 _ = happyFail

action_71 (34) = happyShift action_80
action_71 _ = happyFail

action_72 (20) = happyShift action_26
action_72 (31) = happyShift action_27
action_72 (32) = happyShift action_28
action_72 (33) = happyShift action_29
action_72 (35) = happyShift action_30
action_72 (37) = happyShift action_31
action_72 (39) = happyShift action_32
action_72 (41) = happyShift action_33
action_72 (9) = happyGoto action_79
action_72 (10) = happyGoto action_25
action_72 _ = happyFail

action_73 (18) = happyShift action_78
action_73 _ = happyFail

action_74 (21) = happyShift action_77
action_74 _ = happyFail

action_75 (18) = happyShift action_9
action_75 (19) = happyShift action_10
action_75 (20) = happyShift action_23
action_75 (25) = happyShift action_14
action_75 (30) = happyShift action_16
action_75 (37) = happyShift action_17
action_75 (39) = happyShift action_18
action_75 (41) = happyShift action_19
action_75 (6) = happyGoto action_76
action_75 (8) = happyGoto action_6
action_75 _ = happyFail

action_76 (26) = happyShift action_44
action_76 (35) = happyShift action_45
action_76 (36) = happyShift action_46
action_76 (39) = happyShift action_47
action_76 (42) = happyShift action_110
action_76 _ = happyFail

action_77 (20) = happyShift action_26
action_77 (31) = happyShift action_27
action_77 (32) = happyShift action_28
action_77 (33) = happyShift action_29
action_77 (35) = happyShift action_30
action_77 (37) = happyShift action_31
action_77 (39) = happyShift action_32
action_77 (41) = happyShift action_33
action_77 (9) = happyGoto action_109
action_77 (10) = happyGoto action_25
action_77 _ = happyFail

action_78 (38) = happyShift action_108
action_78 _ = happyFail

action_79 _ = happyReduce_30

action_80 (20) = happyShift action_26
action_80 (31) = happyShift action_27
action_80 (32) = happyShift action_28
action_80 (33) = happyShift action_29
action_80 (35) = happyShift action_30
action_80 (37) = happyShift action_31
action_80 (39) = happyShift action_32
action_80 (41) = happyShift action_33
action_80 (9) = happyGoto action_107
action_80 (10) = happyGoto action_25
action_80 _ = happyFail

action_81 (20) = happyShift action_26
action_81 (31) = happyShift action_27
action_81 (32) = happyShift action_28
action_81 (33) = happyShift action_29
action_81 (35) = happyShift action_30
action_81 (37) = happyShift action_31
action_81 (39) = happyShift action_32
action_81 (41) = happyShift action_33
action_81 (9) = happyGoto action_106
action_81 (10) = happyGoto action_25
action_81 _ = happyFail

action_82 _ = happyReduce_35

action_83 (20) = happyShift action_64
action_83 (12) = happyGoto action_105
action_83 _ = happyFail

action_84 _ = happyReduce_38

action_85 _ = happyReduce_33

action_86 (20) = happyShift action_26
action_86 (31) = happyShift action_27
action_86 (32) = happyShift action_28
action_86 (33) = happyShift action_29
action_86 (35) = happyShift action_30
action_86 (37) = happyShift action_31
action_86 (39) = happyShift action_32
action_86 (41) = happyShift action_33
action_86 (9) = happyGoto action_104
action_86 (10) = happyGoto action_25
action_86 _ = happyFail

action_87 _ = happyReduce_36

action_88 (20) = happyShift action_103
action_88 (26) = happyShift action_44
action_88 (35) = happyShift action_45
action_88 (36) = happyShift action_46
action_88 (39) = happyShift action_47
action_88 _ = happyFail

action_89 (20) = happyShift action_102
action_89 (26) = happyShift action_44
action_89 (35) = happyShift action_45
action_89 (36) = happyShift action_46
action_89 (39) = happyShift action_47
action_89 _ = happyFail

action_90 (20) = happyShift action_101
action_90 (26) = happyShift action_44
action_90 (35) = happyShift action_45
action_90 (36) = happyShift action_46
action_90 (39) = happyShift action_47
action_90 _ = happyFail

action_91 _ = happyReduce_11

action_92 (13) = happyShift action_100
action_92 _ = happyFail

action_93 _ = happyReduce_3

action_94 (26) = happyShift action_44
action_94 (35) = happyShift action_45
action_94 (36) = happyShift action_46
action_94 (39) = happyShift action_47
action_94 _ = happyReduce_20

action_95 (26) = happyShift action_44
action_95 (35) = happyShift action_45
action_95 (36) = happyShift action_46
action_95 (39) = happyShift action_47
action_95 _ = happyReduce_21

action_96 (21) = happyShift action_99
action_96 _ = happyFail

action_97 (18) = happyShift action_9
action_97 (19) = happyShift action_10
action_97 (20) = happyShift action_23
action_97 (25) = happyShift action_14
action_97 (30) = happyShift action_16
action_97 (37) = happyShift action_17
action_97 (39) = happyShift action_18
action_97 (41) = happyShift action_19
action_97 (6) = happyGoto action_98
action_97 (8) = happyGoto action_6
action_97 _ = happyFail

action_98 (26) = happyShift action_44
action_98 (35) = happyShift action_45
action_98 (36) = happyShift action_46
action_98 (39) = happyShift action_47
action_98 _ = happyReduce_25

action_99 (20) = happyShift action_26
action_99 (31) = happyShift action_27
action_99 (32) = happyShift action_28
action_99 (33) = happyShift action_29
action_99 (35) = happyShift action_30
action_99 (37) = happyShift action_31
action_99 (39) = happyShift action_32
action_99 (41) = happyShift action_33
action_99 (9) = happyGoto action_114
action_99 (10) = happyGoto action_25
action_99 _ = happyFail

action_100 _ = happyReduce_4

action_101 (28) = happyShift action_113
action_101 _ = happyFail

action_102 (28) = happyShift action_112
action_102 _ = happyFail

action_103 (28) = happyShift action_111
action_103 _ = happyFail

action_104 _ = happyReduce_43

action_105 _ = happyReduce_41

action_106 _ = happyReduce_39

action_107 _ = happyReduce_37

action_108 _ = happyReduce_14

action_109 _ = happyReduce_17

action_110 _ = happyReduce_24

action_111 (20) = happyShift action_117
action_111 _ = happyFail

action_112 (20) = happyShift action_116
action_112 _ = happyFail

action_113 (20) = happyShift action_115
action_113 _ = happyFail

action_114 _ = happyReduce_23

action_115 (13) = happyShift action_120
action_115 _ = happyFail

action_116 (13) = happyShift action_119
action_116 _ = happyFail

action_117 (13) = happyShift action_118
action_117 _ = happyFail

action_118 _ = happyReduce_8

action_119 _ = happyReduce_7

action_120 _ = happyReduce_9

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happyMonadReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> Move d happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_4 = happyMonadReduce 5 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Arg d happy_var_4 happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_5 = happyMonadReduce 2 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> Label d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_6 = happyMonadReduce 3 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> Jump d happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_7 = happyMonadReduce 8 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_5)) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 8 (\d -> CJump d RLt happy_var_2 happy_var_4 happy_var_5 happy_var_7))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_8 = happyMonadReduce 8 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_5)) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 8 (\d -> CJump d RGt happy_var_2 happy_var_4 happy_var_5 happy_var_7))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_9 = happyMonadReduce 8 5 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyTerminal (TSym happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_5)) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyTerminal (TRop happy_var_3)) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 8 (\d -> CJump d happy_var_3 happy_var_2 happy_var_4 happy_var_5 happy_var_7))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_10 = happyMonadReduce 3 5 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> Return d happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_11 = happyMonadReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> sseq happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_12 = happyMonadReduce 2 5 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> EStmt d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_13 = happyMonadReduce 2 6 happyReduction_13
happyReduction_13 ((HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> Malloc d Heap happy_var_2 0))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_14 = happyMonadReduce 5 6 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyTerminal (TInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Malloc d Heap happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_15 = happyMonadReduce 1 6 happyReduction_15
happyReduction_15 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> LName d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_16 = happyMonadReduce 1 6 happyReduction_16
happyReduction_16 ((HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> Const d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_17 = happyMonadReduce 5 6 happyReduction_17
happyReduction_17 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Mem d happy_var_5 happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_18 = happyMonadReduce 3 6 happyReduction_18
happyReduction_18 ((HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TReg d happy_var_3 happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_19 = happyMonadReduce 2 6 happyReduction_19
happyReduction_19 ((HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal (TUop happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> UOp d happy_var_1 happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_20 = happyMonadReduce 4 6 happyReduction_20
happyReduction_20 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> BOp d BShiftL happy_var_1 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_21 = happyMonadReduce 4 6 happyReduction_21
happyReduction_21 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> BOp d BShiftR happy_var_1 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_22 = happyMonadReduce 3 6 happyReduction_22
happyReduction_22 ((HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (TBop happy_var_2)) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> BOp d happy_var_2 happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_23 = happyMonadReduce 6 6 happyReduction_23
happyReduction_23 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 6 (\d -> Call d happy_var_6 happy_var_1 (reverse happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_24 = happyMonadReduce 5 6 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> ESeq d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_25 = happyMonadReduce 3 7 happyReduction_25
happyReduction_25 ((HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_26 = happyMonadReduce 1 7 happyReduction_26
happyReduction_26 ((HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_27 = happyMonadReduce 2 8 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> CUnit d))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_28 = happyMonadReduce 1 8 happyReduction_28
happyReduction_28 ((HappyTerminal (TInt happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> CInt d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_29 = happyMonadReduce 1 8 happyReduction_29
happyReduction_29 ((HappyTerminal (TStr happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> CString d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_30 = happyMonadReduce 3 9 happyReduction_30
happyReduction_30 ((HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TFn d CFn [happy_var_1] happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_31 = happyMonadReduce 1 9 happyReduction_31
happyReduction_31 ((HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_32 = happyMonadReduce 1 10 happyReduction_32
happyReduction_32 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> if isPrimTy happy_var_1 then TPrim d happy_var_1 else TVar d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_33 = happyMonadReduce 3 10 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_34 = happyMonadReduce 2 10 happyReduction_34
happyReduction_34 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> TPrim d "unit"))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_35 = happyMonadReduce 3 10 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TVariant d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_36 = happyMonadReduce 3 10 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TRecord d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_37 = happyMonadReduce 4 10 happyReduction_37
happyReduction_37 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> TExists d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_38 = happyMonadReduce 3 10 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TArray d happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_39 = happyMonadReduce 4 10 happyReduction_39
happyReduction_39 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> TMu d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_40 = happyMonadReduce 2 10 happyReduction_40
happyReduction_40 ((HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> TPtr d happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_41 = happyMonadReduce 3 11 happyReduction_41
happyReduction_41 ((HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_42 = happyMonadReduce 1 11 happyReduction_42
happyReduction_42 ((HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_43 = happyMonadReduce 3 12 happyReduction_43
happyReduction_43 ((HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_1, happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyNewToken action sts stk
	= lexExtentTrack lexStep(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEof -> action 44 44 tk (HappyState action) sts stk;
	TSemiColon -> cont 13;
	TLArrow -> cont 14;
	TRArrow -> cont 15;
	TArg -> cont 16;
	TDo -> cont 17;
	TInt happy_dollar_dollar -> cont 18;
	TStr happy_dollar_dollar -> cont 19;
	TSym happy_dollar_dollar -> cont 20;
	THasTy -> cont 21;
	TColon -> cont 22;
	TJump -> cont 23;
	TJumpIf -> cont 24;
	TUop happy_dollar_dollar -> cont 25;
	TBop happy_dollar_dollar -> cont 26;
	TRop happy_dollar_dollar -> cont 27;
	TElse -> cont 28;
	TReturn -> cont 29;
	TNew -> cont 30;
	TEx -> cont 31;
	TMuT -> cont 32;
	TCaret -> cont 33;
	TDot -> cont 34;
	TLt -> cont 35;
	TGt -> cont 36;
	TLBracket -> cont 37;
	TRBracket -> cont 38;
	TLParen -> cont 39;
	TRParen -> cont 40;
	TLBracket -> cont 41;
	TRBracket -> cont 42;
	TComma -> cont 43;
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

aml = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
