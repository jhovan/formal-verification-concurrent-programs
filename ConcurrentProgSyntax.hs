module ConcurrentProgSyntax
    (ConcurrentImpProg
    ,ConcProg(..)
    ,showConcProg
    ,showProcessIds
    )
-----------------------------------------------------------------------------------------
--
-- Syntax of concurrent programs based on the Imp language (Winskel 1993)
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
--import GlobalTypes (VarId) -- Value(..), Context
--import ImpCompStmSyntax (showCompStm) -- CompStm(..)
import GlobalFunctions (tabK) -- ,tab4
import ImpSimpStmSyntax (ImpSimpStm)
--
import ImpProgSyntax
    (ProgName
    ,Prog(..)
    ,showProg)
--
-----------------------------------------------------------------------------------------
--
-- Concurrent Imp programs:
-- Lo siguiente intentaba acortar el camino, 
-- pero puede complicar la implementacion posteriormente
-- newtype ConcProg s = ConcProg (ProgName, [(CompStm s,ProgVarList)]) -- Programas concurrentes.
--         deriving (Eq) -- (Eq,Ord)
--         -- P:= cobegin P1 || P2 || ... || Pn coend 
--         -- ConcProg ("P", [(P1,V1),(P2,V2),...,(Pn,Vn)])
--         -- where the Pi are processes (Compoused statements).
-- MEJOR:
newtype ConcProg s = ConcProg (ProgName, [Prog s]) -- Programas concurrentes.
        deriving (Eq) -- (Eq,Ord)
        -- Ver Clarke, Model Checking, 1999. pp.24-26
        -- El programa concurrente P
        -- P:= cobegin P1 || P2 || ... || Pn coend
        -- se representa con 
        -- ConcProg ("P", [Prog (P1,V1,c1), Prog (P2,V2,c2), ..., Prog (Pn,Vn,cn)])
        -- donde las (Pi,Vi,ci) son procesos, programas Imp.
instance (Show s) => Show (ConcProg s) where
    show = showConcProg "" -- nullString means no indentation
--
-- Concurrent programs, based on simple statements for language IMP:
type ConcurrentImpProg = ConcProg ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
showProcessIds :: [Prog s] -> String
-- Show process Ids of a list of processes (Imp programs).
showProcessIds processList = 
    case processList of
        [Prog(p1,_,_)]                  -> p1
        Prog(p1,_,_): Prog(p2,_,_):pL   -> p1 ++ " || " ++ p2 ++ showProcessIds pL
        []                              -> ""
--
showProcessList :: Show s => String -> [Prog s] -> String
-- Show a list of processes.
showProcessList indent processList =
    case processList of
        [Prog p1]           -> showProg indent (Prog p1)
        Prog p1: Prog p2:pL -> showProg indent (Prog p1) 
                            ++ showProg indent (Prog p2) 
                            ++ showProcessList indent pL
        []                  -> ""
--
showProcesses :: Show s => String -> [Prog s] -> String
-- Show processes of a concurrent program
showProcesses indent processList =
    case processList of
        []  ->  ""
        _:_ ->  indent ++ "where\n"
                ++ showProcessList indent processList
--
showConcProg :: Show s => String-> ConcProg s -> String
-- Show a concurrent program.
showConcProg indent (ConcProg (pName, processList)) = 
       indent ++ pName ++  " = {\n"
    ++ indent2  ++ "cobegin "
                ++ showProcessIds processList 
                ++ " coend}\n"
    ++ showProcesses indent2 processList
    where
    indent2 = indent ++ (tabK 4)
--
-----------------------------------------------------------------------------------------
--
