module ImpSimpStmSyntax
    (SimpStm(..)
    ,ImpSimpStm
    ,showImpSimpStm
    ,showSimpStm
    ,varsOfImpSimpStm )
-----------------------------------------------------------------------------------------
-- Sintaxis de instructiones simples del lenguaje Imp.
-- Author: mcb
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
--import GlobalFunctions (tabK) -- replaceInAssocL
import ImpExpSyntax
    (ExpAB(..)
    ,varsOfABexp )
--
-----------------------------------------------------------------------------------------
--
-- Simple statements, instructions, based on expressions e:
data SimpStm e =            -- Simple statements based on expressions e
      Sskip                 -- skip, null statement
    | Sassign VarId e       -- v := e
    | Swrite String VarId   -- write s X
    | Sread  VarId VarId    -- read b X, Intuición: leer del input en la variable X; b = laLecturaSeHizo.
    | Sidle                 -- idle. Make the program idle: x' = x for all x in V U {pc}.
    | SdobleAsgn (VarId,VarId) (e,e) -- Doble asignacion (v,w) ::= (e1,e2)
    deriving (Eq,Ord)
--
instance (Show e) => Show (SimpStm e) where
    show = showSimpStm "" -- (tabK 4)
--    
-- Simple statements for language IMP, based on expressions ExpAB:
type ImpSimpStm = SimpStm ExpAB
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data types:
--
showSimpStm :: (Show e) => String -> (SimpStm e) -> String
showSimpStm indent c =
    case c of
        Sskip                       -> indent ++ "skip"
        Sassign v e                 -> indent ++ v ++ " := " ++ show e
        Swrite s v                  -> indent ++ "write " ++ s ++ v
        Sread  b v                  -> indent ++ "read "  ++ b ++ " " ++ v
        Sidle                       -> indent ++ "idle"
        SdobleAsgn (v,w) (e1,e2)    -> indent ++ show (v,w) ++ " := " ++ show (e1,e2)
--
showImpSimpStm :: String -> ImpSimpStm -> String
showImpSimpStm indent c = showSimpStm indent c
-----------------------------------------------------------------------------------------
--
-- Functions to calculate number of variables:
--
varsOfImpSimpStm :: ImpSimpStm -> [VarId]
-- Variables of compoused statements
varsOfImpSimpStm sS = 
    case sS of
        Sskip       -> []               -- skip
        Sassign v e -> [v] 
                       `union` 
                       (varsOfABexp e)  -- v := e
        Swrite _ v  -> [v]              -- write _ v
        Sread  _ v  -> [v]              -- read _ v
        Sidle       -> []               -- idle
        SdobleAsgn (v,w) (e1,e2)    ->  [v,w] -- (v,w) ::= (e1,e2)
                                        `union` 
                                        (varsOfABexp e1)
                                        `union` 
                                        (varsOfABexp e2)
--
-----------------------------------------------------------------------------------------
--
