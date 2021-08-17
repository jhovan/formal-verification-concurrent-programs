module ImpCompStmSyntax
    (CompStm(..)
    ,ImpCompStm
    ,showCompStm
    ,showSeqStmBlock
    ,varsOfImpComm
    ,numOfStm
    )
-- Sintaxis de instrucciones compuestas del lenguaje Imp.
-----------------------------------------------------------------------------------------
--
-- Syntax of IMP language (Winskel 1993)
-- Author: mcb
--
-- Syntax.
-- Syntactic sets associated with IMP:
-- • numbers N, consisting of positive and negative integers with zero,
-- • truth values T = {true, false},
-- • locations Loc,
-- • arithmetic expressions,
-- • boolean expressions,
-- • commands Com.
--
-- Syntax of programming language IMP:
--
--     Numbers:
--
--         positiveDigit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
--         digit   ::= 0 | positiveDigit
--     Positive numbers, abreviated by "n":
--         positiveNumber ::= positiveDigit | n digit
--     Negative numbers:
--         negativeNumber ::= - positiveNumber
--     Integer numbers:
--         integerNumber ::= 0 | positiveNumber | negativeNumber
--
--     Locations (var Ids), abreviated by X:
--         location ::= letter | letter X | X digit
--
--     Expressions:
--
--     Arithmetic expressions, abreviated by "a":
--         arithmeticExpression ::= integerNumber | X | (a + a) | (a - a) | (a * a)
--     Boolean value:
--         booleanValue ::= True | False
--     Boolean expressions, abreviated by "b":
--         booleanExpression ::= booleanValue | (a = a) | (a <= a) | ¬b | (b & b) | (b | b) 
--     Expressions:
--         expression ::= arithmeticExpression | booleanExpression
--
--     IMP programs:
--
--     Simple commands:
--         simpleCommand ::= skip | (X := e) | (write s X) | (read s X)
--     Compoused commands, abreviated by "c":
--         compousedCommand ::= simpleCommand | (c ; c) | if b then c else c | while b do c | [c]
--     Declarations
--         declaration ::= var [location] 
--     impProgram ::= (declaration, compousedCommand)
--
-- Where:
--     n in positiveNumber
--     X in location, varId
--     s in String
--     a in arithmeticExpression
--     b in booleanExpression
--     e in expression (arithmetic or boolean)
--
-----------------------------------------------------------------------------------------
--
where
--
import Data.List (union) --, nub
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes (VarId) -- Value(..), Context
import GlobalFunctions (tabK) -- tab4, replaceInAssocL
import ImpExpSyntax
    (BoolE(..)
    ,showBoolE
    ,varsOfBexp ) --,showAorBexp,varsOfAexp
import ImpSimpStmSyntax
    (ImpSimpStm
    ,varsOfImpSimpStm ) -- SimpStm(..),showImpSimpStm,showSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Compound statements, instructions, based on simple statements s:
data CompStm s =                                -- Compound statements based on simple statements s
      Csimp s                                   -- Simple statements are compound statements
    | CseqCompos (CompStm s, CompStm s)         -- Sequential composition: c1;c2
    | CseqStmBlock [CompStm s]                  -- Block of sequential statements: c1;c2;...;ck
    | CifThenElse BoolE (CompStm s) (CompStm s) -- if b then c1 else c2
    | Cwhile BoolE (CompStm s)                  -- while b do cList
    deriving (Eq,Ord)
--
instance (Show s) => Show (CompStm s) where
    show = showCompStm ""
--
-- Compound statements for language IMP, based on simple statements for language IMP:
type ImpCompStm = CompStm ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data types:
--
showCompStm :: (Show s) => String->(CompStm s) -> String
showCompStm indent stm =
    case stm of
        Csimp simStm            -> indent ++ show simStm
        CseqCompos (c1,c2)      ->     (showCompStm indent c1) ++ ";\n"
                                    ++ (showCompStm indent c2)
        CseqStmBlock  cList     -> showSeqStmBlock indent cList
        CifThenElse b c1 c2     ->     indent ++ "if "++ showBoolE b ++ "\n" 
                                    ++ indent4 ++ "then {\n" 
                                    ++ showCompStm indent6 c1 ++ "}\n"
                                    ++ indent4 ++ "else {\n" 
                                    ++ showCompStm indent6 c2 ++ "}"
                                    where
                                    indent4 = indent ++ (tabK 4) 
                                    indent6 = indent ++ (tabK 6)
        Cwhile  b c             ->  indent ++ "while " ++ showBoolE b ++ " do {\n"
                                    ++ showCompStm indent8 c ++ "}"
                                    where
                                    --indent4 = indent ++ (tabK 4)
                                    indent8 = indent ++ (tabK 8)
--
showCompStmList :: (Show s) => String->[CompStm s] -> String
showCompStmList indent lcI = 
    case lcI of
        c:lc'   ->  showCompStm indent c  ++ ";\n"
                    ++ showCompStmList indent lc'
        []      -> ""
--
showSeqStmBlock :: (Show s) => String->[CompStm s] -> String
showSeqStmBlock indent seqStmList =  
       indent ++ "Begin\n"
    ++ showCompStmList indent seqStmList
    ++ indent ++ "End"
--     where
--     indent2 = indent ++ (tabK 4)
--
-----------------------------------------------------------------------------------------
--
-- Functions to calculate number of variables:
--
varsOfImpComm :: ImpCompStm -> [VarId]
-- Variables of compoused statements
varsOfImpComm stm = 
    case stm of
        Csimp sI                -> varsOfImpSimpStm sI
        CseqCompos (c1, c2)     -> (varsOfImpComm c1) `union` (varsOfImpComm c2)
        CseqStmBlock lcI        -> foldl1 (union) (map varsOfImpComm lcI)
        CifThenElse b c1 c2     ->  varsOfBexp b 
                                    `union` varsOfImpComm c1 
                                    `union` varsOfImpComm c2
--                                 `union` (foldl1 (union) (map varsOfImpComm cL1))
--                                 `union` (foldl1 (union) (map varsOfImpComm cL2))
        Cwhile b c              -> varsOfBexp b 
                                `union` varsOfImpComm c
--                                 `union` (foldl1 (union) (map varsOfImpComm cList))
--
-----------------------------------------------------------------------------------------
--
numOfStm :: (Num n) => CompStm s -> n
-- Number of statements in a command c
numOfStm cS =
    case cS of
        Csimp _                 -> 1
        CseqCompos (c1, c2)     -> (numOfStm c1) + (numOfStm c2) 
        CseqStmBlock lc         -> 1 + (sum $ map numOfStm lc)
        CifThenElse _ c1 c2     ->  1 
                                    + (numOfStm c1) 
                                    + (numOfStm c2)
--                                 + (sum $ map numOfStm cL1)
--                                 + (sum $ map numOfStm cL2)
        Cwhile  _ c             ->  1 
                                    + (numOfStm c) 
--                                     + (sum $ map numOfStm cList)
        -- _                   -> error $ "numOfStm: Not implemented, c= "++ show c
--
-----------------------------------------------------------------------------------------
--
