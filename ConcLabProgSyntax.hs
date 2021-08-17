module ConcLabProgSyntax
    (LconcProg(..)
    ,showLconcProg
    ,LconcImpProg
    )
-----------------------------------------------------------------------------------------
-- Syntax of Labeled concurrent programs
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
import GlobalTypes 
    (EntryLabel, ExitLabel) -- VarId, Value(..), Labels
import GlobalFunctions (tabK) 
import ImpSimpStmSyntax
    (ImpSimpStm)
import ImpProgSyntax
    (ProgName
    ) -- ,ProgVarList(..), showProgVarList 
-- import ImpLabStmSyntax (LcompStm(..), showLcompStm )
import ImpLabProgSyntax
    (Lprog(..)
    ,showLProg
    ,ProgPC(..)
    ,showProgPC
    ) -- ,LimpProg
--
--
-----------------------------------------------------------------------------------------
--
-- Labeled concurrent programs, based on Imp statements:
newtype LconcProg s = LconcProg (ProgName, ProgPC, EntryLabel, [Lprog s], ExitLabel)
                  deriving (Eq) -- (Eq,Ord)
instance (Show s) => Show (LconcProg s) where
    show = showLconcProg "" -- no indentation
--
-- Labeled IMP programs, based on simple statements for language IMP:
type LconcImpProg = LconcProg ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data:
--
showLprocessIds :: [Lprog s] -> String
-- Show process Ids of a list of processes (Imp programs).
showLprocessIds labProcessList = 
    case labProcessList of
        [Lprog(p1,_,_,_)]                       -> p1
        Lprog(p1,_,_,_): Lprog(p2,_,_,_):lpL    -> p1 ++ " || " ++ p2 ++ showLprocessIds lpL
        []                                      -> ""
--
--
showLProcList :: (Show s) => String-> [Lprog s] -> String
-- Show a list of labeled processes.
showLProcList indent labProcList = 
    case labProcList of
        [p]         -> showLProg indent p
        p:lpList    ->     showLProg indent p
                        ++ showLProcList indent lpList
        []          -> ""
--
showLconcProg :: Show s => String-> LconcProg s -> String
-- Show a labeled concurrent program.
showLconcProg indent (LconcProg (pName, progPC, entryL, labProcList, exitL)) = 
       indent ++ pName ++  " = {\n"
    ++ indent13 ++ "Var\n"
    ++ showProgPC indent16 progPC
    ++ show (entryL,exitL) ++ ": "
    ++ indent6  ++ "cobegin "
                ++ showLprocessIds labProcList 
                ++ " coend}\n"
    ++ indent13 ++ "where\n"
    ++ showLProcList indent6 labProcList
    where
    indent6  = indent ++ (tabK 6)
    indent13 = indent ++ (tabK 13)
    indent16 = indent ++ (tabK 16)
--
-----------------------------------------------------------------------------------------
--
