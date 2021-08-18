module ConcLabProgToNuXmv (concLabProgToNuXmv)
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
import GlobalTypes (VarId)
--     (EntryLabel, ExitLabel) --(VarMN) -- Value(..)
--import GlobalFunctions ()
--import ImpExpSyntax (ExpAB(..), ArthE(..), BoolE(..))
import NuSmvSpecs 
    (ModuleElement(..), NextExpr(..), SimpleExpr(..), SmvId, NuSMVmodel, VarType(..), 
    idTOsmvId, seAndList)
import ImpExpToNuXmv 
    (bExpToNextExpr) -- expABtoNextExpr, aExpToNextExpr, aExpToSimpleExpr, bExpToSimpleExpr, 
-- import ImpSimpStmSyntax 
--     (SimpStm(..)) -- ,ImpSimpStm
import ImpLabStmSyntax 
    (LimpStm
    ,LcompStm(..)
    ,labelsOfLabStm
    ,entryLabelOf
    ,exitLabelOf
    ,setExitLabelOf)
import LimpSimpStmToNuXmv
    (impSimpStmToNextExpr
    ,keepVars) -- ,curr,next,pcX
import ImpLabProgSyntax (ProgPC(..), Lprog(..))
import LimpProgToNuXmv (impTypeToXmvType, impVarDeclToXmvVarDecl) -- lImpProgToNuXmv
--
import ConcLabProgSyntax
import Data.List 

import ImpProgSyntax
    (Prog(..),ImpProg,ProgVarList(..),ImpVarType(..))
--
-------------------------------------------------------------------------------
--

pciList :: [Lprog s] -> [VarId]
-- Extracts a list of pc-Ids from a list of labeled Imp programs.
--pciList labProgList = [pcId | Lprog (_, _, ProgPC (pcId,pcT), _) <- labProgList]
pciList labProgList = [pcId | Lprog (_, _, ProgPC (pcId,_), _) <- labProgList]
--

initialStatesOf :: LconcProg s -> SimpleExpr
-- Given a labeled concurrent program 
--      P^L:= (l1, cobegin (l1,P1^L,l1') || (l2,P2^L,l2') || ... || (ln,Pn^L,ln') coend, ln'),
-- compute a Simple Expression of nuXmv representing 
-- a constraint, S_0(V,PC), that defines the initial states for P^L.
--
-- See Clarke et al., Model Checking, 1999. pp. 23,25.
-- S_0(V,PC) := pre(V) & pc = m & \bigwedge_{i=1}^n (pc_i = \bottom).
-- where: 
--      a) pc_i=\bottom indicates that process P_i has not been activated yet, 
--          and therefore cannot be executed from the current state.
--      b) Vi is the set of variables of process Pi, and V is the union of the Vi.
--      c) pc_i is the program counter of Pi, and PC is the set of all the pc_i.
-- Here, we assume pre(V)=True, i.e. there is no initial constraint for variables of P.
--
--initialStatesOf (LconcProg (pName, ProgPC (pcId,_), entryL, labProcList, exitL)) =
initialStatesOf (LconcProg (_, ProgPC (pcId,_), entryL, labProcList, _)) =
        preOfV                              -- pre(V)
        `SEand`                             -- and
        ((SEvar pc) `SEeq` (SEint m))       -- pc=m
        `SEand`                             -- and
        seAndL_pciEqBot                     -- andOver [(pc_i = \bottom) | i <-[1..n]
        where
        preOfV  = SEtrue
        pc      = idTOsmvId pcId
        m       = entryL
        bottom  = 0 
        seEQ x n = ((SEvar $ idTOsmvId x) `SEeq` (SEint n))
        seAndL_pciEqBot = seAndList [seEQ pci bottom | pci <- pciList labProcList ]   
--
concLabProgToNuXmv :: LconcImpProg -> NuSMVmodel
-- Transform a Labeled concurrent program to nuXmv model.
--concLabProgToNuXmv concP@(LconcProg (pName, ProgPC (pcId,_), entryL, labProcList, exitL)) = []
concLabProgToNuXmv concP@(LconcProg (pName, ProgPC (pcId,pcT), i, labProcList, pcIdleValue)) =
    [pModule, mainModule] -- [pModule, mainModule, ltlModule]
    where
    -- Module for program p (moduleName,moduleParameters,moduleElemList):
    pModule         = (pName,moduleParams,moduleElemList) 
    moduleParams    = [] -- Falta agregar a la sintaxis de Imp-programs: parametros del programa XXX
--     moduleElemList  = [pVarDecl, pcDecl, pInitConstr, pTransConstr]
    moduleElemList  = [pcDecl, pVarDecl, pInitConstr]
    pVarDecl        = impVarDeclToXmvVarDecl (progVarListUnion [(lVarDecl)| Lprog (_, lVarDecl, _, _) <- labProcList])
--     (entryLab,_)    = labelsOfLabStm labStm
    pcDecl          = VarDecl (union [(pcId, impTypeToXmvType pcT)] [(pci, impTypeToXmvType pcT)|pci <- pciList labProcList]) 
    pInitConstr     = InitConstr  (initialStatesOf concP)  -- INIT pc=entryLab
--     pTransConstr    = TransConstr (labStmToXmvConstraint pcId lVarDecl labStm)
    -- Module main (moduleName,moduleParameters,moduleElemList):
    mainModule      = (mainName,mainParams,mainElemList) -- XXXX
    mainName        = "main"
    mainParams      = [] -- Falta agregar a la sintaxis de Imp-programs: parametros del programa XXX
    mainElemList    = [mainVarDecl, mainInitConstr, mainTransConstr] -- XXXX
    mainVarDecl     = VarDecl [(pInstanceId,pInstanceType)] -- XXXX
    pInstanceId     = idTOsmvId (pName) -- ++ "Instance")
    pInstanceType   = TypeModule pName []   -- TypeModule SmvId [SmvId] -- XXXX
    mainInitConstr  = InitConstr SEtrue     -- True means no constraints
    mainTransConstr = TransConstr NEtrue    -- True means no constraints
    -- ltlModule    = ...
    --
--


-- union of ProgVarList List
progVarListUnionAux :: [ProgVarList] -> [(VarId,ImpVarType)] 
progVarListUnionAux l = 
    case l of
        [] -> []
        (ProgVarList varList):xs -> union varList (progVarListUnionAux xs)

progVarListUnion :: [ProgVarList] -> ProgVarList
progVarListUnion l = ProgVarList (progVarListUnionAux l)





--
-- labSeqStmListToNextExpr :: SmvId->[SmvId]->(EntryLabel, [LimpStm], ExitLabel) -> NextExpr
-- -- Clarke, Model Checking, pp. 21-22.
-- labSeqStmListToNextExpr pc vXlist (entryLab, labImpStmList, exitLab) = 
--     case labImpStmList of
--         (labC1:labC2:labImpStmList')
--             ->  labC1nextExpr `NEor` labStmListNextExpr --NEtrue -- XXX
--                 where
--                 labC1nextExpr       = impCompStmToNextExpr pc vXlist labC1
--                 exitLabC1           = exitLabelOf labC1
--                 labStmListNextExpr  = labSeqStmListToNextExpr pc vXlist 
--                                         (exitLabC1,(labC2:labImpStmList'),exitLab)
--         [labImpStm] 
--             ->  impCompStmToNextExpr pc vXlist labImpStm
--         [] 
--             ->  impSimpStmToNextExpr pc vXlist (LsimpleStm (entryLab,Sskip,exitLab))
-- --
-- impCompStmToNextExpr :: SmvId->[SmvId]->LimpStm -> NextExpr
-- -- Transform a labeled Imp statement to a nuXmv nextExpression.
-- -- Constraints describeb by Clarke et al., C(...), are represented by next-expression of nuXmv.
-- -- See Clarke et al., Model Checking, pp. 21-22.
-- impCompStmToNextExpr pc vXlist labImpStm = 
--     case labImpStm of
--         -- Labeled Simple statements:
--         LsimpleStm stm 
--                 ->  impSimpStmToNextExpr pc vXlist (LsimpleStm stm)
--         -- Labeled sequential composition:
--         LseqCompos (l,(labP1,labP2),l')
--                 ->  if l /= l1 || l1' /= l'' || l2' /= l'
--                        then error $ "impCompStmToNextExpr: wrong labels f P1;P2: " 
--                                     ++ show (labP1,labP2)
--                        else labC1nextExpr `NEor` labC2nextExpr
--                     where
--                     (l1,l1')    = labelsOfLabStm labP1
--                     (l'',l2')   = labelsOfLabStm labP2
--                     labC1nextExpr = impCompStmToNextExpr pc vXlist labP1
--                     labC2nextExpr = impCompStmToNextExpr pc vXlist labP2
--         -- Labeled sequential-statement blocks:
--         LseqStmBlok (l,labImpStmList,l') 
--                 -> labSeqStmListToNextExpr pc vXlist (l, labImpStmList, l')
--         -- Labeled if b then labP1 else labP2:
--         LifThenElse (l, (b, labP1, labP2), l') -- Clarke p.22 C(l, if b then l1:P1 else l2:P2, l’)
--             -> f1 `NEor` f2 `NEor` f3 `NEor` f4
--             where
--             l1  = entryLabelOf labP1
--             (l2,_) = labelsOfLabStm labP2
--             f1  = -- (pc=l & pc'=l1 & b & same(V))
--                 (pcVar `NEeq` (NEint l))        -- pc=l
--                 `NEand`                         -- and
--                 (pcNext `NEeq` (NEint l1))      -- next(pc)=l1
--                 `NEand`                         -- and
--                 (bExpToNextExpr b)              -- b
--                 `NEand`                         -- and
--                 (keepVars vXlist)               -- keepVars (vXlist)
--             f2  = -- (pc=l & pc'=l2 & ¬b & same(V))
--                 (pcVar `NEeq` (NEint l))        -- pc=l
--                 `NEand`                         -- and
--                 (pcNext `NEeq` (NEint l2))      -- next(pc)=l2
--                 `NEand`                         -- and
--                 (NEnot (bExpToNextExpr b))      -- not(b)
--                 `NEand`                         -- and
--                 (keepVars vXlist)               -- keepVars (vXlist)
--             labP1'   = setExitLabelOf labP1 l'  -- (l1,S1,_) --> (l1,S1,l’)
--             labP2'   = setExitLabelOf labP2 l'  -- (l2,S2,_) --> (l2,S2,l’)
--             f3      = impCompStmToNextExpr pc vXlist labP1'    -- C(l1,S1,l’)
--             f4      = impCompStmToNextExpr pc vXlist labP2'    -- C(l2,S2,l’)
--         -- Labeled while b do labP1:
--         LwhileStm   (l, (b, labP1), l') -- Clarke p.22 C(l, while b do l1:P1 end while, l')
--                 -> f1 `NEor` f2 `NEor` f3   
--                 where
--                 l1      = entryLabelOf labP1
--                 labP1'  = setExitLabelOf labP1 l    -- P1 = P1 with l as the exit label.
--                 f1  = -- (pc=l & pc'=l1 & b & same(V))
--                     (pcVar `NEeq` (NEint l))            -- pc=l
--                     `NEand`                             -- and
--                     (pcNext `NEeq` (NEint l1))          -- next(pc)=l1
--                     `NEand`                             -- and
--                     (bExpToNextExpr b)                  -- b
--                     `NEand`                             -- and
--                     (keepVars vXlist)                   -- keepVars (vXlist)
--                 f2 = -- (pc=l & pc'=l' & ¬b & same(V))
--                     (pcVar `NEeq` (NEint l))            -- pc=l
--                     `NEand`                             -- and
--                     (pcNext `NEeq` (NEint l'))          -- next(pc)=l'
--                     `NEand`                             -- and
--                     (NEnot (bExpToNextExpr b))          -- ¬ b
--                     `NEand`                             -- and
--                     (keepVars vXlist)                   -- keepVars (vXlist)
--                 f3 = -- C(l1,P1,l) 
--                     impCompStmToNextExpr pc vXlist labP1'
--     where
--     pcVar   = (NEvar pc)
--     pcNext  = (NEnext (SEvar pc))
--
--
-----------------------------------------------------------------------------------------
-- --
