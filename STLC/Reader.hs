{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
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

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
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
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23

action_0 (4) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (4) = happyGoto action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_5
action_2 (27) = happyShift action_6
action_2 (40) = happyShift action_7
action_2 (5) = happyGoto action_4
action_2 _ = happyFail

action_3 (24) = happyShift action_5
action_3 (27) = happyShift action_6
action_3 (40) = happyShift action_7
action_3 (61) = happyAccept
action_3 (5) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_1

action_5 (40) = happyShift action_10
action_5 _ = happyFail

action_6 (40) = happyShift action_9
action_6 _ = happyFail

action_7 (43) = happyShift action_8
action_7 _ = happyFail

action_8 (25) = happyShift action_15
action_8 (26) = happyShift action_16
action_8 (40) = happyShift action_17
action_8 (49) = happyShift action_18
action_8 (51) = happyShift action_19
action_8 (54) = happyShift action_20
action_8 (56) = happyShift action_21
action_8 (8) = happyGoto action_13
action_8 (10) = happyGoto action_14
action_8 _ = happyFail

action_9 (43) = happyShift action_12
action_9 _ = happyFail

action_10 (48) = happyShift action_11
action_10 _ = happyFail

action_11 (25) = happyShift action_15
action_11 (26) = happyShift action_16
action_11 (40) = happyShift action_17
action_11 (49) = happyShift action_18
action_11 (51) = happyShift action_19
action_11 (54) = happyShift action_20
action_11 (56) = happyShift action_21
action_11 (8) = happyGoto action_36
action_11 (10) = happyGoto action_14
action_11 _ = happyFail

action_12 (25) = happyShift action_15
action_12 (26) = happyShift action_16
action_12 (40) = happyShift action_17
action_12 (49) = happyShift action_18
action_12 (51) = happyShift action_19
action_12 (54) = happyShift action_20
action_12 (56) = happyShift action_21
action_12 (8) = happyGoto action_35
action_12 (10) = happyGoto action_14
action_12 _ = happyFail

action_13 (40) = happyShift action_33
action_13 (48) = happyShift action_34
action_13 (6) = happyGoto action_32
action_13 _ = happyFail

action_14 (44) = happyShift action_31
action_14 _ = happyReduce_12

action_15 (40) = happyShift action_30
action_15 _ = happyFail

action_16 (40) = happyShift action_29
action_16 _ = happyFail

action_17 _ = happyReduce_15

action_18 (25) = happyShift action_15
action_18 (26) = happyShift action_16
action_18 (40) = happyShift action_17
action_18 (49) = happyShift action_18
action_18 (51) = happyShift action_19
action_18 (54) = happyShift action_20
action_18 (56) = happyShift action_21
action_18 (8) = happyGoto action_27
action_18 (9) = happyGoto action_28
action_18 (10) = happyGoto action_14
action_18 _ = happyFail

action_19 (40) = happyShift action_24
action_19 (11) = happyGoto action_26
action_19 (12) = happyGoto action_23
action_19 _ = happyFail

action_20 (25) = happyShift action_15
action_20 (26) = happyShift action_16
action_20 (40) = happyShift action_17
action_20 (49) = happyShift action_18
action_20 (51) = happyShift action_19
action_20 (54) = happyShift action_20
action_20 (56) = happyShift action_21
action_20 (8) = happyGoto action_25
action_20 (10) = happyGoto action_14
action_20 _ = happyFail

action_21 (40) = happyShift action_24
action_21 (11) = happyGoto action_22
action_21 (12) = happyGoto action_23
action_21 _ = happyFail

action_22 (57) = happyShift action_68
action_22 (58) = happyShift action_65
action_22 _ = happyFail

action_23 _ = happyReduce_22

action_24 (45) = happyShift action_67
action_24 _ = happyFail

action_25 (55) = happyShift action_66
action_25 _ = happyFail

action_26 (52) = happyShift action_64
action_26 (58) = happyShift action_65
action_26 _ = happyFail

action_27 _ = happyReduce_14

action_28 (50) = happyShift action_62
action_28 (58) = happyShift action_63
action_28 _ = happyFail

action_29 (47) = happyShift action_61
action_29 _ = happyFail

action_30 (47) = happyShift action_60
action_30 _ = happyFail

action_31 (25) = happyShift action_15
action_31 (26) = happyShift action_16
action_31 (40) = happyShift action_17
action_31 (49) = happyShift action_18
action_31 (51) = happyShift action_19
action_31 (54) = happyShift action_20
action_31 (56) = happyShift action_21
action_31 (8) = happyGoto action_59
action_31 (10) = happyGoto action_14
action_31 _ = happyFail

action_32 (48) = happyShift action_58
action_32 _ = happyFail

action_33 (49) = happyShift action_57
action_33 _ = happyFail

action_34 (29) = happyShift action_42
action_34 (31) = happyShift action_43
action_34 (33) = happyShift action_44
action_34 (34) = happyShift action_45
action_34 (35) = happyShift action_46
action_34 (37) = happyShift action_47
action_34 (38) = happyShift action_48
action_34 (39) = happyShift action_49
action_34 (40) = happyShift action_50
action_34 (41) = happyShift action_51
action_34 (42) = happyShift action_52
action_34 (49) = happyShift action_53
action_34 (51) = happyShift action_54
action_34 (54) = happyShift action_55
action_34 (56) = happyShift action_56
action_34 (13) = happyGoto action_39
action_34 (15) = happyGoto action_40
action_34 (16) = happyGoto action_41
action_34 _ = happyFail

action_35 (46) = happyShift action_38
action_35 _ = happyFail

action_36 (46) = happyShift action_37
action_36 _ = happyFail

action_37 _ = happyReduce_6

action_38 _ = happyReduce_5

action_39 (46) = happyShift action_96
action_39 _ = happyFail

action_40 (47) = happyShift action_93
action_40 (49) = happyShift action_94
action_40 (54) = happyShift action_95
action_40 _ = happyReduce_25

action_41 _ = happyReduce_28

action_42 (40) = happyShift action_92
action_42 _ = happyFail

action_43 (29) = happyShift action_42
action_43 (31) = happyShift action_43
action_43 (33) = happyShift action_44
action_43 (34) = happyShift action_45
action_43 (35) = happyShift action_46
action_43 (37) = happyShift action_47
action_43 (38) = happyShift action_48
action_43 (39) = happyShift action_49
action_43 (40) = happyShift action_50
action_43 (41) = happyShift action_51
action_43 (42) = happyShift action_52
action_43 (49) = happyShift action_53
action_43 (51) = happyShift action_54
action_43 (54) = happyShift action_55
action_43 (56) = happyShift action_56
action_43 (13) = happyGoto action_91
action_43 (15) = happyGoto action_40
action_43 (16) = happyGoto action_41
action_43 _ = happyFail

action_44 (54) = happyShift action_90
action_44 _ = happyFail

action_45 (54) = happyShift action_89
action_45 _ = happyFail

action_46 (29) = happyShift action_42
action_46 (31) = happyShift action_43
action_46 (33) = happyShift action_44
action_46 (34) = happyShift action_45
action_46 (35) = happyShift action_46
action_46 (37) = happyShift action_47
action_46 (38) = happyShift action_48
action_46 (39) = happyShift action_49
action_46 (40) = happyShift action_50
action_46 (41) = happyShift action_51
action_46 (42) = happyShift action_52
action_46 (49) = happyShift action_53
action_46 (51) = happyShift action_54
action_46 (54) = happyShift action_55
action_46 (56) = happyShift action_56
action_46 (13) = happyGoto action_88
action_46 (15) = happyGoto action_40
action_46 (16) = happyGoto action_41
action_46 _ = happyFail

action_47 (29) = happyShift action_42
action_47 (31) = happyShift action_43
action_47 (33) = happyShift action_44
action_47 (34) = happyShift action_45
action_47 (35) = happyShift action_46
action_47 (37) = happyShift action_47
action_47 (38) = happyShift action_48
action_47 (39) = happyShift action_49
action_47 (40) = happyShift action_50
action_47 (41) = happyShift action_51
action_47 (42) = happyShift action_52
action_47 (49) = happyShift action_53
action_47 (51) = happyShift action_54
action_47 (54) = happyShift action_55
action_47 (56) = happyShift action_56
action_47 (13) = happyGoto action_87
action_47 (15) = happyGoto action_40
action_47 (16) = happyGoto action_41
action_47 _ = happyFail

action_48 (25) = happyShift action_15
action_48 (26) = happyShift action_16
action_48 (40) = happyShift action_17
action_48 (49) = happyShift action_18
action_48 (51) = happyShift action_19
action_48 (54) = happyShift action_20
action_48 (56) = happyShift action_21
action_48 (8) = happyGoto action_86
action_48 (10) = happyGoto action_14
action_48 _ = happyFail

action_49 _ = happyReduce_45

action_50 _ = happyReduce_29

action_51 _ = happyReduce_46

action_52 _ = happyReduce_32

action_53 (29) = happyShift action_42
action_53 (31) = happyShift action_43
action_53 (33) = happyShift action_44
action_53 (34) = happyShift action_45
action_53 (35) = happyShift action_46
action_53 (37) = happyShift action_47
action_53 (38) = happyShift action_48
action_53 (39) = happyShift action_49
action_53 (40) = happyShift action_50
action_53 (41) = happyShift action_51
action_53 (42) = happyShift action_52
action_53 (49) = happyShift action_53
action_53 (50) = happyShift action_85
action_53 (51) = happyShift action_54
action_53 (54) = happyShift action_55
action_53 (56) = happyShift action_56
action_53 (13) = happyGoto action_84
action_53 (15) = happyGoto action_40
action_53 (16) = happyGoto action_41
action_53 _ = happyFail

action_54 (40) = happyShift action_83
action_54 _ = happyFail

action_55 (29) = happyShift action_42
action_55 (31) = happyShift action_43
action_55 (33) = happyShift action_44
action_55 (34) = happyShift action_45
action_55 (35) = happyShift action_46
action_55 (37) = happyShift action_47
action_55 (38) = happyShift action_48
action_55 (39) = happyShift action_49
action_55 (40) = happyShift action_50
action_55 (41) = happyShift action_51
action_55 (42) = happyShift action_52
action_55 (49) = happyShift action_53
action_55 (51) = happyShift action_54
action_55 (54) = happyShift action_55
action_55 (56) = happyShift action_56
action_55 (13) = happyGoto action_81
action_55 (15) = happyGoto action_40
action_55 (16) = happyGoto action_41
action_55 (23) = happyGoto action_82
action_55 _ = happyFail

action_56 (40) = happyShift action_80
action_56 (19) = happyGoto action_78
action_56 (20) = happyGoto action_79
action_56 _ = happyFail

action_57 (40) = happyShift action_77
action_57 (7) = happyGoto action_76
action_57 _ = happyFail

action_58 (29) = happyShift action_42
action_58 (31) = happyShift action_43
action_58 (33) = happyShift action_44
action_58 (34) = happyShift action_45
action_58 (35) = happyShift action_46
action_58 (37) = happyShift action_47
action_58 (38) = happyShift action_48
action_58 (39) = happyShift action_49
action_58 (40) = happyShift action_50
action_58 (41) = happyShift action_51
action_58 (42) = happyShift action_52
action_58 (49) = happyShift action_53
action_58 (51) = happyShift action_54
action_58 (54) = happyShift action_55
action_58 (56) = happyShift action_56
action_58 (13) = happyGoto action_75
action_58 (15) = happyGoto action_40
action_58 (16) = happyGoto action_41
action_58 _ = happyFail

action_59 _ = happyReduce_10

action_60 (25) = happyShift action_15
action_60 (26) = happyShift action_16
action_60 (40) = happyShift action_17
action_60 (49) = happyShift action_18
action_60 (51) = happyShift action_19
action_60 (54) = happyShift action_20
action_60 (56) = happyShift action_21
action_60 (8) = happyGoto action_74
action_60 (10) = happyGoto action_14
action_60 _ = happyFail

action_61 (25) = happyShift action_15
action_61 (26) = happyShift action_16
action_61 (40) = happyShift action_17
action_61 (49) = happyShift action_18
action_61 (51) = happyShift action_19
action_61 (54) = happyShift action_20
action_61 (56) = happyShift action_21
action_61 (8) = happyGoto action_73
action_61 (10) = happyGoto action_14
action_61 _ = happyFail

action_62 (44) = happyShift action_72
action_62 _ = happyFail

action_63 (25) = happyShift action_15
action_63 (26) = happyShift action_16
action_63 (40) = happyShift action_17
action_63 (49) = happyShift action_18
action_63 (51) = happyShift action_19
action_63 (54) = happyShift action_20
action_63 (56) = happyShift action_21
action_63 (8) = happyGoto action_71
action_63 (10) = happyGoto action_14
action_63 _ = happyFail

action_64 _ = happyReduce_16

action_65 (40) = happyShift action_24
action_65 (12) = happyGoto action_70
action_65 _ = happyFail

action_66 _ = happyReduce_19

action_67 (25) = happyShift action_15
action_67 (26) = happyShift action_16
action_67 (40) = happyShift action_17
action_67 (49) = happyShift action_18
action_67 (51) = happyShift action_19
action_67 (54) = happyShift action_20
action_67 (56) = happyShift action_21
action_67 (8) = happyGoto action_69
action_67 (10) = happyGoto action_14
action_67 _ = happyFail

action_68 _ = happyReduce_17

action_69 _ = happyReduce_23

action_70 _ = happyReduce_21

action_71 _ = happyReduce_13

action_72 (25) = happyShift action_15
action_72 (26) = happyShift action_16
action_72 (40) = happyShift action_17
action_72 (49) = happyShift action_18
action_72 (51) = happyShift action_19
action_72 (54) = happyShift action_20
action_72 (56) = happyShift action_21
action_72 (8) = happyGoto action_118
action_72 (10) = happyGoto action_14
action_72 _ = happyFail

action_73 _ = happyReduce_18

action_74 _ = happyReduce_20

action_75 (46) = happyShift action_117
action_75 _ = happyFail

action_76 (50) = happyShift action_115
action_76 (58) = happyShift action_116
action_76 _ = happyFail

action_77 _ = happyReduce_9

action_78 (57) = happyShift action_113
action_78 (58) = happyShift action_114
action_78 _ = happyFail

action_79 _ = happyReduce_51

action_80 (48) = happyShift action_112
action_80 _ = happyFail

action_81 _ = happyReduce_57

action_82 (55) = happyShift action_110
action_82 (58) = happyShift action_111
action_82 _ = happyFail

action_83 (48) = happyShift action_109
action_83 _ = happyFail

action_84 (50) = happyShift action_108
action_84 _ = happyFail

action_85 _ = happyReduce_44

action_86 (54) = happyShift action_107
action_86 _ = happyFail

action_87 (36) = happyShift action_106
action_87 _ = happyFail

action_88 (36) = happyShift action_105
action_88 _ = happyFail

action_89 (25) = happyShift action_15
action_89 (26) = happyShift action_16
action_89 (40) = happyShift action_17
action_89 (49) = happyShift action_18
action_89 (51) = happyShift action_19
action_89 (54) = happyShift action_20
action_89 (56) = happyShift action_21
action_89 (8) = happyGoto action_104
action_89 (10) = happyGoto action_14
action_89 _ = happyFail

action_90 (25) = happyShift action_15
action_90 (26) = happyShift action_16
action_90 (40) = happyShift action_17
action_90 (49) = happyShift action_18
action_90 (51) = happyShift action_19
action_90 (54) = happyShift action_20
action_90 (56) = happyShift action_21
action_90 (8) = happyGoto action_103
action_90 (10) = happyGoto action_14
action_90 _ = happyFail

action_91 (32) = happyShift action_102
action_91 _ = happyFail

action_92 (48) = happyShift action_101
action_92 _ = happyFail

action_93 (40) = happyShift action_100
action_93 _ = happyFail

action_94 (29) = happyShift action_42
action_94 (31) = happyShift action_43
action_94 (33) = happyShift action_44
action_94 (34) = happyShift action_45
action_94 (35) = happyShift action_46
action_94 (37) = happyShift action_47
action_94 (38) = happyShift action_48
action_94 (39) = happyShift action_49
action_94 (40) = happyShift action_50
action_94 (41) = happyShift action_51
action_94 (42) = happyShift action_52
action_94 (49) = happyShift action_53
action_94 (51) = happyShift action_54
action_94 (54) = happyShift action_55
action_94 (56) = happyShift action_56
action_94 (13) = happyGoto action_98
action_94 (14) = happyGoto action_99
action_94 (15) = happyGoto action_40
action_94 (16) = happyGoto action_41
action_94 _ = happyFail

action_95 (29) = happyShift action_42
action_95 (31) = happyShift action_43
action_95 (33) = happyShift action_44
action_95 (34) = happyShift action_45
action_95 (35) = happyShift action_46
action_95 (37) = happyShift action_47
action_95 (38) = happyShift action_48
action_95 (39) = happyShift action_49
action_95 (40) = happyShift action_50
action_95 (41) = happyShift action_51
action_95 (42) = happyShift action_52
action_95 (49) = happyShift action_53
action_95 (51) = happyShift action_54
action_95 (54) = happyShift action_55
action_95 (56) = happyShift action_56
action_95 (13) = happyGoto action_97
action_95 (15) = happyGoto action_40
action_95 (16) = happyGoto action_41
action_95 _ = happyFail

action_96 _ = happyReduce_3

action_97 (55) = happyShift action_135
action_97 _ = happyFail

action_98 _ = happyReduce_27

action_99 (50) = happyShift action_133
action_99 (58) = happyShift action_134
action_99 _ = happyFail

action_100 _ = happyReduce_38

action_101 (29) = happyShift action_42
action_101 (31) = happyShift action_43
action_101 (33) = happyShift action_44
action_101 (34) = happyShift action_45
action_101 (35) = happyShift action_46
action_101 (37) = happyShift action_47
action_101 (38) = happyShift action_48
action_101 (39) = happyShift action_49
action_101 (40) = happyShift action_50
action_101 (41) = happyShift action_51
action_101 (42) = happyShift action_52
action_101 (49) = happyShift action_53
action_101 (51) = happyShift action_54
action_101 (54) = happyShift action_55
action_101 (56) = happyShift action_56
action_101 (13) = happyGoto action_132
action_101 (15) = happyGoto action_40
action_101 (16) = happyGoto action_41
action_101 _ = happyFail

action_102 (40) = happyShift action_131
action_102 (21) = happyGoto action_129
action_102 (22) = happyGoto action_130
action_102 _ = happyFail

action_103 (55) = happyShift action_128
action_103 _ = happyFail

action_104 (55) = happyShift action_127
action_104 _ = happyFail

action_105 (25) = happyShift action_15
action_105 (26) = happyShift action_16
action_105 (40) = happyShift action_17
action_105 (49) = happyShift action_18
action_105 (51) = happyShift action_19
action_105 (54) = happyShift action_20
action_105 (56) = happyShift action_21
action_105 (8) = happyGoto action_126
action_105 (10) = happyGoto action_14
action_105 _ = happyFail

action_106 (56) = happyShift action_125
action_106 _ = happyFail

action_107 (29) = happyShift action_42
action_107 (31) = happyShift action_43
action_107 (33) = happyShift action_44
action_107 (34) = happyShift action_45
action_107 (35) = happyShift action_46
action_107 (37) = happyShift action_47
action_107 (38) = happyShift action_48
action_107 (39) = happyShift action_49
action_107 (40) = happyShift action_50
action_107 (41) = happyShift action_51
action_107 (42) = happyShift action_52
action_107 (49) = happyShift action_53
action_107 (51) = happyShift action_54
action_107 (54) = happyShift action_55
action_107 (56) = happyShift action_56
action_107 (13) = happyGoto action_124
action_107 (15) = happyGoto action_40
action_107 (16) = happyGoto action_41
action_107 _ = happyFail

action_108 _ = happyReduce_33

action_109 (29) = happyShift action_42
action_109 (31) = happyShift action_43
action_109 (33) = happyShift action_44
action_109 (34) = happyShift action_45
action_109 (35) = happyShift action_46
action_109 (37) = happyShift action_47
action_109 (38) = happyShift action_48
action_109 (39) = happyShift action_49
action_109 (40) = happyShift action_50
action_109 (41) = happyShift action_51
action_109 (42) = happyShift action_52
action_109 (49) = happyShift action_53
action_109 (51) = happyShift action_54
action_109 (54) = happyShift action_55
action_109 (56) = happyShift action_56
action_109 (13) = happyGoto action_123
action_109 (15) = happyGoto action_40
action_109 (16) = happyGoto action_41
action_109 _ = happyFail

action_110 _ = happyReduce_41

action_111 (29) = happyShift action_42
action_111 (31) = happyShift action_43
action_111 (33) = happyShift action_44
action_111 (34) = happyShift action_45
action_111 (35) = happyShift action_46
action_111 (37) = happyShift action_47
action_111 (38) = happyShift action_48
action_111 (39) = happyShift action_49
action_111 (40) = happyShift action_50
action_111 (41) = happyShift action_51
action_111 (42) = happyShift action_52
action_111 (49) = happyShift action_53
action_111 (51) = happyShift action_54
action_111 (54) = happyShift action_55
action_111 (56) = happyShift action_56
action_111 (13) = happyGoto action_122
action_111 (15) = happyGoto action_40
action_111 (16) = happyGoto action_41
action_111 _ = happyFail

action_112 (29) = happyShift action_42
action_112 (31) = happyShift action_43
action_112 (33) = happyShift action_44
action_112 (34) = happyShift action_45
action_112 (35) = happyShift action_46
action_112 (37) = happyShift action_47
action_112 (38) = happyShift action_48
action_112 (39) = happyShift action_49
action_112 (40) = happyShift action_50
action_112 (41) = happyShift action_51
action_112 (42) = happyShift action_52
action_112 (49) = happyShift action_53
action_112 (51) = happyShift action_54
action_112 (54) = happyShift action_55
action_112 (56) = happyShift action_56
action_112 (13) = happyGoto action_121
action_112 (15) = happyGoto action_40
action_112 (16) = happyGoto action_41
action_112 _ = happyFail

action_113 _ = happyReduce_36

action_114 (40) = happyShift action_80
action_114 (20) = happyGoto action_120
action_114 _ = happyFail

action_115 _ = happyReduce_7

action_116 (40) = happyShift action_119
action_116 _ = happyFail

action_117 _ = happyReduce_4

action_118 _ = happyReduce_11

action_119 _ = happyReduce_8

action_120 _ = happyReduce_50

action_121 _ = happyReduce_52

action_122 _ = happyReduce_56

action_123 (53) = happyShift action_144
action_123 _ = happyFail

action_124 (55) = happyShift action_143
action_124 _ = happyFail

action_125 (40) = happyShift action_142
action_125 _ = happyFail

action_126 _ = happyReduce_39

action_127 (29) = happyShift action_42
action_127 (31) = happyShift action_43
action_127 (33) = happyShift action_44
action_127 (34) = happyShift action_45
action_127 (35) = happyShift action_46
action_127 (37) = happyShift action_47
action_127 (38) = happyShift action_48
action_127 (39) = happyShift action_49
action_127 (40) = happyShift action_50
action_127 (41) = happyShift action_51
action_127 (42) = happyShift action_52
action_127 (49) = happyShift action_53
action_127 (51) = happyShift action_54
action_127 (54) = happyShift action_55
action_127 (56) = happyShift action_56
action_127 (13) = happyGoto action_141
action_127 (15) = happyGoto action_40
action_127 (16) = happyGoto action_41
action_127 _ = happyFail

action_128 (29) = happyShift action_42
action_128 (31) = happyShift action_43
action_128 (33) = happyShift action_44
action_128 (34) = happyShift action_45
action_128 (35) = happyShift action_46
action_128 (37) = happyShift action_47
action_128 (38) = happyShift action_48
action_128 (39) = happyShift action_49
action_128 (40) = happyShift action_50
action_128 (41) = happyShift action_51
action_128 (42) = happyShift action_52
action_128 (49) = happyShift action_53
action_128 (51) = happyShift action_54
action_128 (54) = happyShift action_55
action_128 (56) = happyShift action_56
action_128 (13) = happyGoto action_140
action_128 (15) = happyGoto action_40
action_128 (16) = happyGoto action_41
action_128 _ = happyFail

action_129 (53) = happyShift action_139
action_129 _ = happyReduce_37

action_130 _ = happyReduce_54

action_131 (40) = happyShift action_138
action_131 _ = happyFail

action_132 (30) = happyShift action_137
action_132 _ = happyFail

action_133 _ = happyReduce_24

action_134 (29) = happyShift action_42
action_134 (31) = happyShift action_43
action_134 (33) = happyShift action_44
action_134 (34) = happyShift action_45
action_134 (35) = happyShift action_46
action_134 (37) = happyShift action_47
action_134 (38) = happyShift action_48
action_134 (39) = happyShift action_49
action_134 (40) = happyShift action_50
action_134 (41) = happyShift action_51
action_134 (42) = happyShift action_52
action_134 (49) = happyShift action_53
action_134 (51) = happyShift action_54
action_134 (54) = happyShift action_55
action_134 (56) = happyShift action_56
action_134 (13) = happyGoto action_136
action_134 (15) = happyGoto action_40
action_134 (16) = happyGoto action_41
action_134 _ = happyFail

action_135 _ = happyReduce_42

action_136 _ = happyReduce_26

action_137 (29) = happyShift action_42
action_137 (31) = happyShift action_43
action_137 (33) = happyShift action_44
action_137 (34) = happyShift action_45
action_137 (35) = happyShift action_46
action_137 (37) = happyShift action_47
action_137 (38) = happyShift action_48
action_137 (39) = happyShift action_49
action_137 (40) = happyShift action_50
action_137 (41) = happyShift action_51
action_137 (42) = happyShift action_52
action_137 (49) = happyShift action_53
action_137 (51) = happyShift action_54
action_137 (54) = happyShift action_55
action_137 (56) = happyShift action_56
action_137 (13) = happyGoto action_151
action_137 (15) = happyGoto action_40
action_137 (16) = happyGoto action_41
action_137 _ = happyFail

action_138 (47) = happyShift action_150
action_138 _ = happyFail

action_139 (40) = happyShift action_131
action_139 (22) = happyGoto action_149
action_139 _ = happyFail

action_140 _ = happyReduce_30

action_141 _ = happyReduce_31

action_142 (58) = happyShift action_148
action_142 _ = happyFail

action_143 _ = happyReduce_43

action_144 (40) = happyShift action_147
action_144 (17) = happyGoto action_145
action_144 (18) = happyGoto action_146
action_144 _ = happyFail

action_145 (52) = happyShift action_155
action_145 (58) = happyShift action_156
action_145 _ = happyFail

action_146 _ = happyReduce_48

action_147 (45) = happyShift action_154
action_147 _ = happyFail

action_148 (40) = happyShift action_153
action_148 _ = happyFail

action_149 _ = happyReduce_53

action_150 (29) = happyShift action_42
action_150 (31) = happyShift action_43
action_150 (33) = happyShift action_44
action_150 (34) = happyShift action_45
action_150 (35) = happyShift action_46
action_150 (37) = happyShift action_47
action_150 (38) = happyShift action_48
action_150 (39) = happyShift action_49
action_150 (40) = happyShift action_50
action_150 (41) = happyShift action_51
action_150 (42) = happyShift action_52
action_150 (49) = happyShift action_53
action_150 (51) = happyShift action_54
action_150 (54) = happyShift action_55
action_150 (56) = happyShift action_56
action_150 (13) = happyGoto action_152
action_150 (15) = happyGoto action_40
action_150 (16) = happyGoto action_41
action_150 _ = happyFail

action_151 _ = happyReduce_34

action_152 _ = happyReduce_55

action_153 (57) = happyShift action_159
action_153 _ = happyFail

action_154 (25) = happyShift action_15
action_154 (26) = happyShift action_16
action_154 (40) = happyShift action_17
action_154 (49) = happyShift action_18
action_154 (51) = happyShift action_19
action_154 (54) = happyShift action_20
action_154 (56) = happyShift action_21
action_154 (8) = happyGoto action_158
action_154 (10) = happyGoto action_14
action_154 _ = happyFail

action_155 _ = happyReduce_35

action_156 (40) = happyShift action_147
action_156 (18) = happyGoto action_157
action_156 _ = happyFail

action_157 _ = happyReduce_47

action_158 _ = happyReduce_49

action_159 (30) = happyShift action_160
action_159 _ = happyFail

action_160 (29) = happyShift action_42
action_160 (31) = happyShift action_43
action_160 (33) = happyShift action_44
action_160 (34) = happyShift action_45
action_160 (35) = happyShift action_46
action_160 (37) = happyShift action_47
action_160 (38) = happyShift action_48
action_160 (39) = happyShift action_49
action_160 (40) = happyShift action_50
action_160 (41) = happyShift action_51
action_160 (42) = happyShift action_52
action_160 (49) = happyShift action_53
action_160 (51) = happyShift action_54
action_160 (54) = happyShift action_55
action_160 (56) = happyShift action_56
action_160 (13) = happyGoto action_161
action_160 (15) = happyGoto action_40
action_160 (16) = happyGoto action_41
action_160 _ = happyFail

action_161 _ = happyReduce_40

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

happyReduce_3 = happyMonadReduce 6 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 6 (\d -> NamedTerm d happy_var_1 [] happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_4 = happyMonadReduce 7 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 7 (\d -> NamedTerm d happy_var_1 (reverse happy_var_4) happy_var_3 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_5 = happyMonadReduce 5 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Extern d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_6 = happyMonadReduce 5 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> TypeAlias d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_7 = happyMonadReduce 4 6 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (const happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_8 = happyMonadReduce 3 7 happyReduction_8
happyReduction_8 ((HappyTerminal (TSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_9 = happyMonadReduce 1 7 happyReduction_9
happyReduction_9 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_10 = happyMonadReduce 3 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TFn d CFn [happy_var_1] happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_11 = happyMonadReduce 5 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> TFn d CFn (reverse happy_var_2) happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_12 = happyMonadReduce 1 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_13 = happyMonadReduce 3 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_14 = happyMonadReduce 1 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_15 = happyMonadReduce 1 10 happyReduction_15
happyReduction_15 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> if isPrimTy happy_var_1 then TPrim d happy_var_1 else TVar d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_16 = happyMonadReduce 3 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TVariant d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_17 = happyMonadReduce 3 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TRecord d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_18 = happyMonadReduce 4 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> TExists d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_19 = happyMonadReduce 3 10 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> TArray d happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_20 = happyMonadReduce 4 10 happyReduction_20
happyReduction_20 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> TMu d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_21 = happyMonadReduce 3 11 happyReduction_21
happyReduction_21 ((HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_22 = happyMonadReduce 1 11 happyReduction_22
happyReduction_22 ((HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_23 = happyMonadReduce 3 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_1, happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_24 = happyMonadReduce 4 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> App d happy_var_1 (reverse happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_25 = happyMonadReduce 1 13 happyReduction_25
happyReduction_25 ((HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_26 = happyMonadReduce 3 14 happyReduction_26
happyReduction_26 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_27 = happyMonadReduce 1 14 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_28 = happyMonadReduce 1 15 happyReduction_28
happyReduction_28 ((HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> Prim d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_29 = happyMonadReduce 1 15 happyReduction_29
happyReduction_29 ((HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> Var d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_30 = happyMonadReduce 5 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Roll d happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_31 = happyMonadReduce 5 15 happyReduction_31
happyReduction_31 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> Unroll d happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_32 = happyMonadReduce 1 15 happyReduction_32
happyReduction_32 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> Prim d (CUnit d)))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_33 = happyMonadReduce 3 15 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_34 = happyMonadReduce 6 15 happyReduction_34
happyReduction_34 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 6 (\d -> Let d happy_var_2 happy_var_4 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_35 = happyMonadReduce 7 15 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 7 (\d -> Variant d happy_var_2 happy_var_4 (reverse happy_var_6)))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_36 = happyMonadReduce 3 15 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> Record d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_37 = happyMonadReduce 4 15 happyReduction_37
happyReduction_37 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> VCase d happy_var_2 (reverse happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_38 = happyMonadReduce 3 15 happyReduction_38
happyReduction_38 ((HappyTerminal (TSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> RProj d happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_39 = happyMonadReduce 4 15 happyReduction_39
happyReduction_39 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> Pack d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_40 = happyMonadReduce 10 15 happyReduction_40
happyReduction_40 ((HappyAbsSyn13  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 10 (\d -> Unpack d happy_var_2 happy_var_5 happy_var_7 happy_var_10))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_41 = happyMonadReduce 3 15 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (\d -> Array d (reverse happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_42 = happyMonadReduce 4 15 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (\d -> ArrElem d happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_43 = happyMonadReduce 5 15 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 5 (\d -> ArrAlloc d happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_44 = happyMonadReduce 2 16 happyReduction_44
happyReduction_44 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 2 (\d -> CUnit d))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_45 = happyMonadReduce 1 16 happyReduction_45
happyReduction_45 ((HappyTerminal (TInt happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> CInt d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_46 = happyMonadReduce 1 16 happyReduction_46
happyReduction_46 ((HappyTerminal (TStr happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (\d -> CString d happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_47 = happyMonadReduce 3 17 happyReduction_47
happyReduction_47 ((HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_48 = happyMonadReduce 1 17 happyReduction_48
happyReduction_48 ((HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_49 = happyMonadReduce 3 18 happyReduction_49
happyReduction_49 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_1, happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn18 r))

happyReduce_50 = happyMonadReduce 3 19 happyReduction_50
happyReduction_50 ((HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_51 = happyMonadReduce 1 19 happyReduction_51
happyReduction_51 ((HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_52 = happyMonadReduce 3 20 happyReduction_52
happyReduction_52 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_1, happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_53 = happyMonadReduce 3 21 happyReduction_53
happyReduction_53 ((HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn21 r))

happyReduce_54 = happyMonadReduce 1 21 happyReduction_54
happyReduction_54 ((HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn21 r))

happyReduce_55 = happyMonadReduce 4 22 happyReduction_55
happyReduction_55 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_2)) `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 4 (const (happy_var_1, happy_var_2, happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_56 = happyMonadReduce 3 23 happyReduction_56
happyReduction_56 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 3 (const (happy_var_3 : happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_57 = happyMonadReduce 1 23 happyReduction_57
happyReduction_57 ((HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( reduce 1 (const [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyNewToken action sts stk
	= lexExtentTrack lexStep(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEof -> action 61 61 tk (HappyState action) sts stk;
	TType -> cont 24;
	TMuT -> cont 25;
	TEx -> cont 26;
	TExtern -> cont 27;
	TFrom -> cont 28;
	TLet -> cont 29;
	TIn -> cont 30;
	TCase -> cont 31;
	TOf -> cont 32;
	TRoll -> cont 33;
	TUnroll -> cont 34;
	TPack -> cont 35;
	TAs -> cont 36;
	TUnpack -> cont 37;
	TNew -> cont 38;
	TInt happy_dollar_dollar -> cont 39;
	TSym happy_dollar_dollar -> cont 40;
	TStr happy_dollar_dollar -> cont 41;
	TUnit -> cont 42;
	THasTy -> cont 43;
	TFnArrow -> cont 44;
	TColon -> cont 45;
	TSemiColon -> cont 46;
	TDot -> cont 47;
	TEqual -> cont 48;
	TLParen -> cont 49;
	TRParen -> cont 50;
	TLT -> cont 51;
	TGT -> cont 52;
	TBar -> cont 53;
	TLBracket -> cont 54;
	TRBracket -> cont 55;
	TLBrace -> cont 56;
	TRBrace -> cont 57;
	TComma -> cont 58;
	TPlus -> cont 59;
	TMinus -> cont 60;
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

stlc = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
