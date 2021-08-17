module ImpLabStmSyntax
    (LcompStm(..)
    ,LimpStm
    ,showLcompStm
    ,labelsOfLabStm
    ,entryLabelOf
    ,exitLabelOf
    ,setExitLabelOf )
-----------------------------------------------------------------------------------------
-- Syntax of labeled Imp statements
-- Author: mcb
--
--
-----------------------------------------------------------------------------------------
--
where
--
--import Data.List (nub, union)
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes (EntryLabel, ExitLabel, Labels) --VarId, Value(..), Context
import GlobalFunctions (tabK) -- tab4
import ImpExpSyntax
    (BoolE(..)
    ,showBoolE ) --,ArthE(..), ExpAB(..)
import ImpSimpStmSyntax
    (ImpSimpStm) --,SimpStm(..))
--
-----------------------------------------------------------------------------------------
--
-- Labeled compound statements, instructions, based on simple statements s:
data LcompStm s = 
    -- Labeled Simple statements:
      LsimpleStm  (EntryLabel,s,ExitLabel) 
    -- Labeled sequential composition:
    | LseqCompos (EntryLabel,(LcompStm s,LcompStm s),ExitLabel)
    -- Labeled sequential-statement blocks:
    | LseqStmBlok (EntryLabel,[LcompStm s],ExitLabel)          
    -- Labeled if bE then sB1 else sB2:
    | LifThenElse ( EntryLabel,                                
                    (BoolE, LcompStm s, LcompStm s),
                    ExitLabel) 
    -- Labeled while b do a sequential-statement block:
    | LwhileStm   ( EntryLabel,                                
                    (BoolE, LcompStm s),
                    ExitLabel)
    deriving (Eq,Ord)
--
instance (Show s) => Show (LcompStm s) where
    show = showLcompStm  "" -- nullString means no indentation
--
type LimpStm = LcompStm ImpSimpStm
--
-----------------------------------------------------------------------------------------
--
-- Functions to show data:
--
showLcompStm :: (Show s) => String->LcompStm s -> String
showLcompStm indent labStm =
    case labStm of
        LsimpleStm (entryL, sS, exitL)      
            ->  show (entryL,exitL) ++ ": "
                ++ indent ++  show sS
        LseqCompos (_, (c1,c2), _)
            ->  -- show (entryL,exitL) ++ ": "
                   showLcompStm indent c1 ++ ";\n"
                ++ showLcompStm indent c2
        LseqStmBlok (_, cList, _)  
            ->  -- show (entryL,exitL) ++ ": "
                showLSeqStmBlockX indent cList
        LifThenElse (entryL, (b, c1, c2), exitL)      
            ->  show (entryL,exitL) ++ ": "
                ++ indent   ++ "if "++ showBoolE b ++ "\n" 
                ++ indent10 ++ "then {\n" 
                ++ showLcompStm indent10 c1 ++ "}\n"
                ++ indent10  ++ "else {\n" 
                ++ showLcompStm indent10 c2 ++ "}"
                where
                indent10 = indent ++ (tabK 10)
        LwhileStm (entryL, (b, c), exitL)
            ->  show (entryL,exitL) ++ ": "
                ++ indent ++ "while " ++ showBoolE b ++" do {\n"
                ++ showLcompStm indent4 c ++ "}"
                where
                indent4 = indent ++ (tabK 4)
--
---------------------------------------------------------------
--
showLcompStmList :: (Show s) => String-> [LcompStm s] -> String
showLcompStmList indent lcI = 
    case lcI of
        c:lc'   -> showLcompStm indent c  ++ ";\n"
                    ++ showLcompStmList indent lc'
        []      -> ""
--
showLSeqStmBlockX :: (Show s) => String->[LcompStm s] -> String
showLSeqStmBlockX indent seqStmList =  
--        indent++ "Begin \n"
       indent++ (tabK 7) ++ "Begin \n"
    ++ showLcompStmList indent seqStmList
    ++ indent++ (tabK 7) ++ "End"
--
-----------------------------------------------------------------------------------------
--
-- Functions to manage labels:
--
labelsOfLabStm :: LcompStm s -> Labels
-- Labels of a Labeled compoused statement.
labelsOfLabStm labStm =
    case labStm of
        LsimpleStm  (entryL,_,exitL)    -> (entryL,exitL)
        LseqCompos  (entryL,_,exitL)    -> (entryL,exitL)
        LseqStmBlok (entryL,_,exitL)    -> (entryL,exitL)
        LifThenElse (entryL,_,exitL)    -> (entryL,exitL)
        LwhileStm   (entryL,_,exitL)    -> (entryL,exitL)
        -- _                   -> error $ "labelsOfLabStm: Not implemented, labStm= "++ show labStm
--
entryLabelOf :: LcompStm s -> ExitLabel
-- Returns the entry label of a labeled statement.
entryLabelOf labStm = fst (labelsOfLabStm labStm)
--
exitLabelOf :: LcompStm s -> ExitLabel
-- Returns the exit label of a labeled statement.
exitLabelOf labStm = snd (labelsOfLabStm labStm)
--
setExitLabelOf :: LcompStm s->ExitLabel -> LcompStm s
setExitLabelOf labStm xL= 
    case labStm of
        LsimpleStm  (l,s,_)             -> LsimpleStm (l,s,xL)
        LseqCompos  (l,(lc1,lc2),_)     -> LseqCompos (l,(lc1,lc2'),xL)
                                            where
                                            lc2' = setExitLabelOf lc2 xL
        LseqStmBlok (l,labList,_)       -> LseqStmBlok (l,labList',xL)
                                            where
                                            labList'= setExitLabelOfList labList xL
        LifThenElse (l,(b,lc1,lc2),_)   -> LifThenElse (l,(b,lc1',lc2'),xL)
                                            where
                                            lc1'= setExitLabelOf lc1 xL
                                            lc2'= setExitLabelOf lc2 xL
        LwhileStm   (l,(b,lc),_)        -> LwhileStm (l,(b,lc'),xL)
                                            where
                                            lc' = setExitLabelOf lc xL
        -- _                   -> error $ "setExitLabelOf: Not implemented, labStm= "++ show labStm
--
setExitLabelOfList :: [LcompStm s]-> ExitLabel -> [LcompStm s]
setExitLabelOfList labStmL xL= 
    case labStmL of
        [lS]        -> [setExitLabelOf lS xL]
        lS:lS2:lL   -> lS : (setExitLabelOfList (lS2:lL) xL)
        []          -> []
-----------------------------------------------------------------------------------------
--
