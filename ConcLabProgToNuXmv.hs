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
import GlobalTypes
    (VarId, EntryLabel(..), ExitLabel(..))
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
import ImpProgSyntax
    (Prog(..),ImpProg,ProgVarList(..),ImpVarType(..))
import LimpProgToNuXmv (impTypeToXmvType, impVarDeclToXmvVarDecl) -- lImpProgToNuXmv
--
import ConcLabProgSyntax

import Data.List 
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
    moduleElemList  = [pcDecl, pVarDecl, pInitConstr, pTransConstr]
--     pVarDecl        = impVarDeclToXmvVarDecl lVarDecl
    pVarDecl        = impVarDeclToXmvVarDecl (progVarListUnion [(lVarDecl)| Lprog (_, lVarDecl, _, _) <- labProcList])
--     (entryLab,_)    = labelsOfLabStm labStm
    pcDecl          = VarDecl (union [(pcId, impTypeToXmvType pcT)] [(pci, impTypeToXmvType pcT)|pci <- pciList labProcList]) 
    pInitConstr     = InitConstr  (initialStatesOf concP)  -- INIT pc=entryLab
    pTransConstr    = TransConstr (constraint pcId i labProcList pcIdleValue)
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
    -- C(l, cobegin l1:P1: l1' || ... || ln:Pn:ln' coend, l')
--

------------------------------------
---
------------------------------------
progVarListUnionAux :: [ProgVarList] -> [(VarId,ImpVarType)] 
progVarListUnionAux l = 
    case l of
        [] -> []
        (ProgVarList varList):xs -> union varList (progVarListUnionAux xs)

progVarListUnion :: [ProgVarList] -> ProgVarList
progVarListUnion l = ProgVarList (progVarListUnionAux l)

nePciList :: [Lprog s] -> [NextExpr]
nePciList labProgList = [NEvar pc | Lprog (_, _, ProgPC (pc,_), _) <- labProgList]

nextPciList :: [Lprog s] -> [NextExpr]
nextPciList labProgList = [NEnext (SEvar pc) | Lprog (_, _, ProgPC (pc,_), _) <- labProgList]

entryLabelList :: [Lprog s] -> [NextExpr]
entryLabelList labProgList = [NEint l | Lprog (_, _, _, LseqCompos (l, _, _)) <- labProgList]

exitLabelList :: [Lprog s] -> [NextExpr]
exitLabelList labProgList = [NEint l | Lprog (_, _, _, LseqCompos (_, _, l)) <- labProgList]

neqListToNexpr :: [NextExpr] -> [NextExpr] -> NextExpr
neqListToNexpr l1 [] = NEtrue
neqListToNexpr [] l2 = NEtrue
neqListToNexpr l1 l2 = ((head l1) `NEeq` (head l2)) `NEand` (neqListToNexpr (tail l1) (tail l2))

allBottom :: [NextExpr] -> NextExpr
allBottom [] = NEtrue
allBottom l = ((head l) `NEeq` (NEint 0)) `NEand` (allBottom (tail l))

constraint :: SmvId -> EntryLabel -> [Lprog s] -> ExitLabel -> NextExpr
constraint pc l0 labProgList lf = 
    f1 `NEor` f2 --`NEor` f3
    where
    -- (pc = l) ^ (pc1' = l1) ^ ... ^ (pcn' = ln) ^ (pc' = bottom)                         -- //////////////////////////
    f1 = ((NEvar pc) `NEeq` (NEint l0))                                                    -- pc = l
         `NEand`                                                                           -- and
         (neqListToNexpr (nextPciList labProgList) (entryLabelList labProgList))           -- next(pci) = li for all i
         `NEand`                                                                           -- and
         ((NEnext (SEvar pc)) `NEeq` (NEint 0))                                            -- next(pc) = bottom
    -- (pc = bottom) ^ (pc1 = l1') ^ ... ^ (pcn = ln') ^ (pc' = l') ^ CONJ(pci' = bottom)  -- //////////////////////////
    f2 = ((NEvar pc) `NEeq` (NEint 0))                                                     -- pc = bottom
         `NEand`                                                                           -- and
         (neqListToNexpr (nePciList labProgList) (exitLabelList labProgList))              -- pci = next(li) for all i
         `NEand`                                                                           -- and
         ((NEnext (SEvar pc)) `NEeq` (NEint lf))                                           -- next(pc) = next(l)
         `NEand`                                                                           -- and
         (allBottom (nextPciList labProgList))                                             -- CONJ(next(pci) = bottom)
    --                                                                                     -- //////////////////////////
    -- f3 = NEtrue     
