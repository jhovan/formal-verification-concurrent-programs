module ImpExpSyntax
    (ArthE(..), BoolE(..), ExpAB(..)
    ,showBoolE
    ,showArthE
    ,showAorBexp
    ,varsOfBexp
    ,varsOfAexp
    ,varsOfABexp
    )
--
-- Syntax of Expressions for IMP language (Winskel 1993)
-- mcb
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
-----------------------------------------------------------------------------------------
where
--
--
import Data.List (nub, union)
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes (VarId) -- Value(..), Context
--import GlobalFunctions (replaceInAssocL)
--
-----------------------------------------------------------------------------------------
--
-- Arithmetic Expressions:
data ArthE = Aint Int | Avar VarId                                  -- z, arith variable
          | Asum ArthE ArthE | Adif ArthE ArthE | Aprod ArthE ArthE -- +, -, *
          | Adiv ArthE ArthE    -- Integer division, n div m 
          | Amod ArthE ArthE    -- Module operation, n mod m
            deriving (Eq,Ord)   --(Show,Eq,Ord)
--
instance Show ArthE where
    show = showArthE
--
-- Boolean Expressions:
data BoolE = BT | BF | Bvar VarId                                   -- top,bot,boolVar
          | Bequ ArthE ArthE | Bleq ArthE ArthE | Bgt ArthE ArthE   -- n=m, n<=m, n>m
          | Bnot BoolE | Band BoolE BoolE | Bor BoolE BoolE         -- not, and, or
            deriving (Eq,Ord) --(Show,Eq,Ord)
--
instance Show BoolE where
    show = showBoolE
--
-- Expressions (arithmetic and boolean):
data ExpAB =  Earith ArthE  -- arith expression
            | Ebool BoolE   -- bool expression
            deriving (Eq,Ord) --(Show,Eq,Ord)
--
instance Show ExpAB where
    show = showAorBexp
-----------------------------------------------------------------------------------------
--
-- Functions to show data:
--
showArthE :: ArthE -> String
showArthE a = case a of
        Aint z  -> show z
        Avar v  -> v 
        Asum a1 a2  -> "("++ showArthE a1++ " + " ++ showArthE a2 ++")"
        Adif a1 a2  -> "("++ showArthE a1++ " - " ++ showArthE a2 ++")"
        Aprod a1 a2 -> "("++ showArthE a1++ " * " ++ showArthE a2 ++")"
        Adiv a1 a2  -> "("++ showArthE a1++ " div " ++ showArthE a2 ++")"
        Amod a1 a2  -> "("++ showArthE a1++ " mod " ++ showArthE a2 ++")"
--
showBoolE :: BoolE -> String
showBoolE b = case b of
        BT          -> "True"
        BF          -> "False"
        Bvar v      -> v
        Bequ a1 a2  -> "("++ showArthE a1++ " = " ++ showArthE a2++ ")"
        Bleq a1 a2  -> "("++ showArthE a1++ " <= " ++ showArthE a2++ ")"
        Bgt  a1 a2  -> "("++ showArthE a1++ " > " ++ showArthE a2++ ")"
        Bnot b1     -> "not "++ showBoolE  b1
        Band b1 b2  -> "("++ showBoolE b1++ " and " ++ showBoolE b2++ ")"
        Bor  b1 b2  -> "("++ showBoolE b1++ " or " ++ showBoolE b2++ ")"
--
showAorBexp :: ExpAB -> String
showAorBexp e = case e of
        Earith a  -> showArthE a
        Ebool b  -> showBoolE b
--
-----------------------------------------------------------------------------------------
--
-- Functions to calculate number of variables:
--
varsOfAexp :: ArthE -> [VarId]
--variables of an arith expressions
varsOfAexp a = nub lx
    where
    lx= case a of
        Aint _  -> [] 
        Avar v  -> [v] 
        Asum a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
        Adif a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
        Aprod a1 a2 -> varsOfAexp a1 `union` varsOfAexp a2
        Adiv a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
        Amod a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
--
varsOfBexp :: BoolE -> [VarId]
--variables of a boolean expressions
varsOfBexp b = nub lx
    where
    lx= case b of
        BT          -> []
        BF          -> []
        Bvar v      -> [v]
        Bequ a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2 
        Bleq a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
        Bgt  a1 a2  -> varsOfAexp a1 `union` varsOfAexp a2
        Bnot b1     -> varsOfBexp b1
        Band b1 b2  -> varsOfBexp b1 `union` varsOfBexp b2 
        Bor  b1 b2  -> varsOfBexp b1 `union` varsOfBexp b2
--
varsOfABexp :: ExpAB -> [VarId]
--variables of a expression
varsOfABexp e =
    case e of
        Earith a  -> varsOfAexp a
        Ebool b  -> varsOfBexp b
--
-----------------------------------------------------------------------------------------
--
