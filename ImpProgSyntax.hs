module ImpProgSyntax
    (ImpProg
    ,Prog(..)
    ,ProgName
    ,ImpVarType(..)
    ,ProgVarList(..)
    ,showProg
    ,showProgVarList
    )
--
-----------------------------------------------------------------------------------------
--
-- Syntax of Imp programs
-- Author: mcb
--
--
-----------------------------------------------------------------------------------------
--
where
--
--import Data.List (union) --, nub
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes (VarId) -- Value(..), Context
import GlobalFunctions (tabK) -- ,tab4
import ImpSimpStmSyntax
    (ImpSimpStm)
import ImpCompStmSyntax
    (CompStm(..)
    ,showCompStm)
--
-----------------------------------------------------------------------------------------
--
-- Imp Programs:
--
-- Declaration of variables:
-- 
data ImpVarType=                -- Types of variables for Imp language.
          TimpBoolean           -- Boolean type for Imp language.
        | TimpRange (Int,Int)   -- Range type for Imp language.
        | TimpInt               -- Int type for Imp language. Para usar con nuXmv
        deriving (Eq)
--
instance Show ImpVarType where
    show = showImpVarType
--
newtype ProgVarList = ProgVarList [(VarId,ImpVarType)] 
                      deriving (Eq) -- (Eq,Ord)
--
instance Show ProgVarList where
    show = showProgVarList "" -- nullString means no indentation
--
-- Imp Programs, based on simple statements s:
type ProgName = String
newtype Prog s = Prog (ProgName, ProgVarList, CompStm s) -- Programas imperativos.
                 deriving (Eq) -- (Eq,Ord)
instance (Show s) => Show (Prog s) where
    show = showProg "" -- nullString means no indentation
--
-- IMP programs, based on simple statements for language IMP:
type ImpProg = Prog ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data:
--
showImpVarType :: ImpVarType -> String
showImpVarType t = 
    case t of
        TimpBoolean     -> "Boolean"                -- Boolean type
        TimpRange (i,j) -> show i ++ ".." ++ show j -- Range type
        TimpInt         -> "Int"                    -- Int type
--
showProgVarList :: String-> ProgVarList -> String
showProgVarList indent (ProgVarList lv) =  
    indent ++ "Var\n"
    ++ showVarTypeList (indent ++ (tabK 4)) lv
--
showVarTypeList :: String-> [(VarId,ImpVarType)] -> String
showVarTypeList indent lvt = 
    case lvt of
        [(x,tx)]        ->  indent ++ x ++ ": " ++ show tx ++ ";\n"
        (x,tx):(y,ty):l ->  indent ++ x ++ ": " ++ show tx ++ ";\n"
                            ++ showVarTypeList indent ((y,ty):l)
        []              ->  indent ++ "% There are no declared variables.\n"
--
showProg :: Show s => String-> Prog s -> String
showProg indent (Prog (pName, lVarDec, stm)) = 
       indent ++ pName ++  " = {\n"
    ++ showProgVarList indent2 lVarDec
    ++ showCompStm indent2 stm ++ "}\n"
    where 
    indent2 = indent ++ (tabK 4)
--
-----------------------------------------------------------------------------------------
--
