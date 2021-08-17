module LimpCompStmToNuXmv (impCompStmToNextExpr)
-------------------------------------------------------------------------------
-- Transform a labeled compoused statement to a NuSMV expression
-- Constraints describeb by Clarke et al., C(...), are represented by next-expression of nuXmv.
-- See Clarke et al. Model Checking, 1999. pp.22-26
-- Author: Miguel Carrillo Barajas.
--
where
--
--import Data.List ((\\)) -- nub,delete,
--import qualified Data.Set as S (Set,fromList) -- (\\),toList,filter
--
-------------------------------------------------------------------------------
--
import GlobalTypes 
    (EntryLabel, ExitLabel) --(VarId, VarMN) -- Value(..)
--import GlobalFunctions ()
--import ImpExpSyntax (ExpAB(..), ArthE(..), BoolE(..))
import NuSmvSpecs 
    (NextExpr(..), SimpleExpr(..), SmvId) -- , idTOsmvId
import ImpExpToNuXmv 
    (bExpToNextExpr) -- expABtoNextExpr, aExpToNextExpr, aExpToSimpleExpr, bExpToSimpleExpr, 
import ImpSimpStmSyntax 
    (SimpStm(..)) -- ,ImpSimpStm
import ImpLabStmSyntax 
    (LimpStm
    ,LcompStm(..)
    --,EntryLabel, ExitLabel
    ,labelsOfLabStm
    ,entryLabelOf
    ,exitLabelOf
    ,setExitLabelOf)
import LimpSimpStmToNuXmv
    (impSimpStmToNextExpr
    ,keepVars) -- ,curr,next,pcX
--import IMPsemantics (evalAexp)
--
-------------------------------------------------------------------------------
--
labSeqStmListToNextExpr :: SmvId->[SmvId]->(EntryLabel, [LimpStm], ExitLabel) -> NextExpr
-- Clarke, Model Checking, pp. 21-22.
labSeqStmListToNextExpr pc vXlist (entryLab, labImpStmList, exitLab) = 
    case labImpStmList of
        (labC1:labC2:labImpStmList')
            ->  labC1nextExpr `NEor` labStmListNextExpr --NEtrue -- XXX
                where
                labC1nextExpr       = impCompStmToNextExpr pc vXlist labC1
                exitLabC1           = exitLabelOf labC1
                labStmListNextExpr  = labSeqStmListToNextExpr pc vXlist 
                                        (exitLabC1,(labC2:labImpStmList'),exitLab)
        [labImpStm] 
            ->  impCompStmToNextExpr pc vXlist labImpStm
        [] 
            ->  impSimpStmToNextExpr pc vXlist (LsimpleStm (entryLab,Sskip,exitLab))
--
impCompStmToNextExpr :: SmvId->[SmvId]->LimpStm -> NextExpr
-- Transform a labeled Imp statement to a nuXmv nextExpression.
-- Constraints describeb by Clarke et al., C(...), are represented by next-expression of nuXmv.
-- See Clarke et al., Model Checking, pp. 21-22.
impCompStmToNextExpr pc vXlist labImpStm = 
    case labImpStm of
        -- Labeled Simple statements:
        LsimpleStm stm 
                ->  impSimpStmToNextExpr pc vXlist (LsimpleStm stm)
        -- Labeled sequential composition:
        LseqCompos (l,(labP1,labP2),l')
                ->  if l /= l1 || l1' /= l'' || l2' /= l'
                       then error $ "impCompStmToNextExpr: wrong labels f P1;P2: " 
                                    ++ show (labP1,labP2)
                       else labC1nextExpr `NEor` labC2nextExpr
                    where
                    (l1,l1')    = labelsOfLabStm labP1
                    (l'',l2')   = labelsOfLabStm labP2
                    labC1nextExpr = impCompStmToNextExpr pc vXlist labP1
                    labC2nextExpr = impCompStmToNextExpr pc vXlist labP2
        -- Labeled sequential-statement blocks:
        LseqStmBlok (l,labImpStmList,l') 
                -> labSeqStmListToNextExpr pc vXlist (l, labImpStmList, l')
        -- Labeled if b then labP1 else labP2:
        LifThenElse (l, (b, labP1, labP2), l') -- Clarke p.22 C(l, if b then l1:P1 else l2:P2, l’)
            -> f1 `NEor` f2 `NEor` f3 `NEor` f4
            where
            l1  = entryLabelOf labP1
            (l2,_) = labelsOfLabStm labP2
            f1  = -- (pc=l & pc'=l1 & b & same(V))
                (pcVar `NEeq` (NEint l))        -- pc=l
                `NEand`                         -- and
                (pcNext `NEeq` (NEint l1))      -- next(pc)=l1
                `NEand`                         -- and
                (bExpToNextExpr b)              -- b
                `NEand`                         -- and
                (keepVars vXlist)               -- keepVars (vXlist)
            f2  = -- (pc=l & pc'=l2 & ¬b & same(V))
                (pcVar `NEeq` (NEint l))        -- pc=l
                `NEand`                         -- and
                (pcNext `NEeq` (NEint l2))      -- next(pc)=l2
                `NEand`                         -- and
                (NEnot (bExpToNextExpr b))      -- not(b)
                `NEand`                         -- and
                (keepVars vXlist)               -- keepVars (vXlist)
            labP1'   = setExitLabelOf labP1 l'  -- (l1,S1,_) --> (l1,S1,l’)
            labP2'   = setExitLabelOf labP2 l'  -- (l2,S2,_) --> (l2,S2,l’)
            f3      = impCompStmToNextExpr pc vXlist labP1'    -- C(l1,S1,l’)
            f4      = impCompStmToNextExpr pc vXlist labP2'    -- C(l2,S2,l’)
        -- Labeled while b do labP1:
        LwhileStm   (l, (b, labP1), l') -- Clarke p.22 C(l, while b do l1:P1 end while, l')
                -> f1 `NEor` f2 `NEor` f3   
                where
                l1      = entryLabelOf labP1
                labP1'  = setExitLabelOf labP1 l    -- P1 = P1 with l as the exit label.
                f1  = -- (pc=l & pc'=l1 & b & same(V))
                    (pcVar `NEeq` (NEint l))            -- pc=l
                    `NEand`                             -- and
                    (pcNext `NEeq` (NEint l1))          -- next(pc)=l1
                    `NEand`                             -- and
                    (bExpToNextExpr b)                  -- b
                    `NEand`                             -- and
                    (keepVars vXlist)                   -- keepVars (vXlist)
                f2 = -- (pc=l & pc'=l' & ¬b & same(V))
                    (pcVar `NEeq` (NEint l))            -- pc=l
                    `NEand`                             -- and
                    (pcNext `NEeq` (NEint l'))          -- next(pc)=l'
                    `NEand`                             -- and
                    (NEnot (bExpToNextExpr b))          -- ¬ b
                    `NEand`                             -- and
                    (keepVars vXlist)                   -- keepVars (vXlist)
                f3 = -- C(l1,P1,l) 
                    impCompStmToNextExpr pc vXlist labP1'
    where
    pcVar   = (NEvar pc)
    pcNext  = (NEnext (SEvar pc))
--
--
-----------------------------------------------------------------------------------------
-- --
