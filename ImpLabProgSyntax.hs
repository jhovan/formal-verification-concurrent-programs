module ImpLabProgSyntax
    (Lprog(..)
    ,LimpProg
    ,ProgPC(..)
    ,showProgPC
    ,showLImpProg
    ,showLProg )
-----------------------------------------------------------------------------------------
-- Syntax of labeled programs for the Imp language (Winskel 1993)
-- Author: mcb
--
--
--
-----------------------------------------------------------------------------------------
where
--
--import Data.List (nub, union)
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes (VarId) -- Value(..), Context
import GlobalFunctions (tabK) -- tab4
-- import ImpExpSyntax (BoolE(..), showBoolE )
import ImpSimpStmSyntax
    (ImpSimpStm)
import ImpProgSyntax
    (ProgName
    ,ProgVarList(..) 
    ,ImpVarType
    ,showProgVarList )
import ImpLabStmSyntax
    (LcompStm(..)
    ,showLcompStm
    ,labelsOfLabStm)
--
-----------------------------------------------------------------------------------------
--
-- Labeled programs, based on simple statements s:
newtype ProgPC = ProgPC (VarId,ImpVarType)
                      deriving (Eq) -- (Eq,Ord)
instance Show ProgPC where
    show = showProgPC "" -- no indentation
--
newtype Lprog s = Lprog (ProgName, ProgVarList, ProgPC, LcompStm s) 
                  deriving (Eq) -- (Eq,Ord)
instance (Show s) => Show (Lprog s) where
    show = showLProg "" -- no indentation
--
-- Labeled IMP programs, based on simple statements for language IMP:
type LimpProg = Lprog ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data:
--
showProgPC :: String -> ProgPC -> String
showProgPC indent (ProgPC (x,tx)) = indent ++ x ++ ": " ++ show tx ++ ";\n"
--
showLProg :: (Show s) => String-> Lprog s -> String
showLProg indent (Lprog (pName, lVarDec, pcL, labStm)) = 
       (show labOfStm) ++": " ++ indent ++ pName ++  " = {\n"
    ++ showProgVarList indent10 lVarDec
    ++ showProgPC indent14 pcL
    ++ showLcompStm indent4 labStm ++ "}\n"
    where 
    indent4     = indent ++ (tabK 4)
    indent10    = indent ++ (tabK 10)
    indent14    = indent ++ (tabK 14)
    labOfStm    = labelsOfLabStm labStm
--
showLImpProg :: LimpProg -> String
showLImpProg (Lprog (pName, vD, pcD, labStm)) = showLProg "" (Lprog (pName, vD, pcD, labStm))
--
-----------------------------------------------------------------------------------------
--
