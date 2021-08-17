module ConcProgToLconcProg
    (concProgToLabConcProg)
-----------------------------------------------------------------------------------------
-- Transforma un program concurrente (basado en Imp) a un programa concurrente etiquetado.
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
import GlobalTypes (VarId,EntryLabel) -- Value(..), Context
import GlobalFunctions (pcStatic) -- ,replaceInAssocL
import ImpProgSyntax
    (ImpVarType(..)) -- Prog(..),ImpProg,ProgVarList(..),
import ImpLabProgSyntax (ProgPC(..))
import ConcurrentProgSyntax 
    (ConcurrentImpProg, ConcProg(..))
import ConcLabProgSyntax 
    (LconcProg(..)
    ,LconcImpProg
    ) -- ,showLconcProg
import ImpProgToLimpProg (impProgToLabImpProg)
--
-----------------------------------------------------------------------------------------
--
-- Function for transforming concurrent Imp programs to Labeled Imp programs:
--
concProgToLabConcProg :: EntryLabel->VarId -> ConcurrentImpProg -> LconcImpProg
-- Add labels to the instructions of a concurrent Imp program. Labeling begins with the label i.
concProgToLabConcProg i pcId (ConcProg (pName, procList)) = 
    LconcProg (pName, ProgPC pcL, i, labProcList, pcIdleValue) 
    where
    pcIdleValue = 0
    stmExit     = snd (last idxProcList)
    pcL         = (pcId,TimpRange (pcStatic,stmExit))
    idxProcList = zip procList [1..length procList]
    labProcList = [impProgToLabImpProg i (pcId++show j) proc | (proc,j) <- idxProcList]
--
-----------------------------------------------------------------------------------------
--
