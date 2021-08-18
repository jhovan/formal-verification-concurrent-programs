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
import GlobalTypes (VarId, EntryLabel, ExitLabel)
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
import ImpLabProgSyntax (ProgPC(..), Lprog(..), LimpProg(..))
import LimpProgToNuXmv (impTypeToXmvType, impVarDeclToXmvVarDecl, labStmToXmvConstraint) -- lImpProgToNuXmv

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

progPcList :: [Lprog s] -> [ProgPC]
progPcList labProgList = [pc | Lprog (_, _, pc, _) <- labProgList]




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
    moduleElemList  = [pcDecl, pVarDecl, pInitConstr, pTransConstr]
    pVarDecl        = impVarDeclToXmvVarDecl (progVarListUnion [(lVarDecl)| Lprog (_, lVarDecl, _, _) <- labProcList])
--     (entryLab,_)    = labelsOfLabStm labStm
    pcDecl          = VarDecl (union [(pcId, impTypeToXmvType pcT)] [(pci, impTypeToXmvType pcTi)|ProgPC (pci,pcTi) <- progPcList labProcList]) 
    pInitConstr     = InitConstr  (initialStatesOf concP)  -- INIT pc=entryLab
    pTransConstr    = TransConstr (labConcProgConstraint i pcId labProcList pcIdleValue)
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

varIdListFromProgVarList :: ProgVarList -> [VarId]
varIdListFromProgVarList (ProgVarList l) = 
    case l of
        [] -> []
        ((varId,_)):xs -> union [varId] (varIdListFromProgVarList (ProgVarList xs))


-- constrains

f1Aux :: [LimpProg] -> NextExpr
f1Aux l =
    case l of
        [] -> NEtrue
        (Lprog (_, _, ProgPC (pcId,_), LseqCompos (i,(_,_),_))):xs -> ((NEnext (SEvar pcId)) `NEeq` (NEint i)) 
                                                                   `NEand` 
                                                                    (f1Aux xs)

f2Aux1 :: [LimpProg] -> NextExpr
f2Aux1 l =
    case l of
        [] -> NEtrue
        (Lprog (_, _, ProgPC (pcId,_), LseqCompos (_, _, l'))):xs -> ((NEvar pcId) `NEeq` (NEint l')) 
                                                `NEand` 
                                                (f2Aux1 xs)

f2Aux2 :: [LimpProg] -> NextExpr
f2Aux2 l =
    case l of
        [] -> NEtrue
        (Lprog (_, _, ProgPC (pcId,_), _)):xs -> ((NEnext (SEvar pcId)) `NEeq` (NEint 0)) 
                                                `NEand` 
                                                (f2Aux2 xs)

f3Aux :: [LimpProg] -> [VarId] -> [VarId] -> NextExpr
f3Aux l variables pcs =
    case l of
        [] -> NEfalse
        (Lprog (_, lVarDecl, ProgPC (pcId,_), labStm)):xs -> ((labStmToXmvConstraint pcId lVarDecl labStm)
                                                            `NEand` 
                                                            (keepVars (variables \\ (varIdListFromProgVarList lVarDecl))) 
                                                            `NEand` 
                                                            (keepVars (pcs \\ [pcId])) 
                                                            )
                                                            `NEor` 
                                                            (f3Aux xs variables pcs)

labConcProgConstraint :: EntryLabel -> VarId -> [LimpProg] -> ExitLabel -> NextExpr
labConcProgConstraint l pcId labProcList l' = f1 `NEor` f2 `NEor` f3 
    where
    -- pc = l ^ pc1' = l1 ^ .. ^ pcn'=ln ^ pc' = Bottom
    f1 = ((NEvar pcId) `NEeq` (NEint l)) 
        `NEand` 
        (f1Aux labProcList)
        `NEand` 
        ((NEnext (SEvar pcId)) `NEeq` (NEint 0))  
    -- pc = Bottom ^ pc1 = l1' ^ .. ^ pcn=ln' ^ pc' = l' ^ (conjuncion de pci'= Bottom para todo i)
    f2 = ((NEvar pcId) `NEeq` (NEint 0)) 
        `NEand` 
        (f2Aux1 labProcList)
        `NEand` 
        ((NEnext (SEvar pcId)) `NEeq` (NEint l')) 
        `NEand` 
        (f2Aux2 labProcList)
    -- Disyuncion de (C(li,pi,li') ^ same (V\Vi) ^ same (PC\{pci})) para todos los i
    f3 = (f3Aux labProcList (varIdListFromProgVarList (progVarListUnion [(lVarDecl)| Lprog (_, lVarDecl, _, _) <- labProcList])) (pciList labProcList))

                                            


