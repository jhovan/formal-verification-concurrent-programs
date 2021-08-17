module LimpProgToNuXmv (lImpProgToNuXmv, impTypeToXmvType)
-------------------------------------------------------------------------------
-- Transform a labeled Imp program to a NuXmv model
-- Author: Miguel Carrillo Barajas.
--
-------------------------------------------------------------------------------
where
--
--import Data.List ((\\)) -- nub,delete,
--import qualified Data.Set as S (Set,fromList) -- (\\),toList,filter
--
-------------------------------------------------------------------------------
--
import GlobalTypes 
    (VarId,EntryLabel) -- VarMN, Value(..)
--import GlobalFunctions (pcStatic)
import NuSmvSpecs 
    (NextExpr(..)
    ,SmvId
    ,ModuleElement(..) 
    ,SimpleExpr(..)
    ,idTOsmvId )
import ImpProgSyntax
    (ImpVarType(..)
    ,ProgVarList(..)
    )
import ImpLabStmSyntax 
    (LimpStm
    ,labelsOfLabStm) -- ,ExitLabel,exitLabelOf,LcompStm(..),Labels,entryLabelOf,setExitLabelOf
import ImpLabProgSyntax
    (Lprog(..)
    ,LimpProg
    ,ProgPC(..))
import NuSmvSpecs 
    (NuSMVmodel
    ,VarType(..) ) -- ,NuSMVmodule
import LimpCompStmToNuXmv 
    (impCompStmToNextExpr)
--
-------------------------------------------------------------------------------
--
lImpProgToNuXmv :: LimpProg -> NuSMVmodel
-- Labeled Imp program to nuXmv model
lImpProgToNuXmv (Lprog (pName, lVarDecl, ProgPC (pcId,pcT), labStm)) = 
    [pModule, mainModule] -- [pModule, mainModule, ltlModule]
    where
    -- Module for program p (moduleName,moduleParameters,moduleElemList):
    pModule         = (pName,moduleParams,moduleElemList) 
    moduleParams    = [] -- Falta agregar a la sintaxis de Imp-programs: parametros del programa XXX
    moduleElemList  = [pVarDecl, pcDecl, pInitConstr, pTransConstr]
    pVarDecl        = impVarDeclToXmvVarDecl lVarDecl
    (entryLab,_)    = labelsOfLabStm labStm
    pcDecl          = VarDecl     [(pcId,impTypeToXmvType pcT)]
    pInitConstr     = InitConstr  (pcInitConstr pcId entryLab)  -- INIT pc=entryLab
    pTransConstr    = TransConstr (labStmToXmvConstraint pcId lVarDecl labStm)
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
pcInitConstr :: SmvId -> EntryLabel -> SimpleExpr
pcInitConstr pcId entryLab = -- pc=entryLab:
    ((SEvar pcId) `SEeq` (SEint entryLab))  
--
impVarDeclToXmvVarDecl :: ProgVarList -> ModuleElement
impVarDeclToXmvVarDecl (ProgVarList pVarDecl)  = VarDecl (impVarListToXmvVarlist pVarDecl)
--
impTypeToXmvType :: ImpVarType -> VarType
impTypeToXmvType t = 
    case t of
        TimpBoolean     -> TypeBoolean      -- Boolean type
        TimpRange (i,j) -> TypeRange i j    -- Range type
        TimpInt         -> TypeInt          -- Int type
--
impVarListToXmvVarlist :: [(VarId,ImpVarType)] -> [(SmvId, VarType)]
impVarListToXmvVarlist lvT =
    case lvT of
        [(x,tx)]            ->  [(idTOsmvId x, impTypeToXmvType tx)]
        (x,tx) : yTy : lvT'  ->    (idTOsmvId x, impTypeToXmvType tx) 
                                : (impVarListToXmvVarlist (yTy : lvT'))
        []                  ->  []
--
labStmToXmvConstraint :: SmvId->ProgVarList->LimpStm -> NextExpr
labStmToXmvConstraint pcId (ProgVarList xTxList) labStm = 
    (impCompStmToNextExpr pcId xList labStm)    -- Constraints for labeled statement
    where
    xList = [x | (x,_) <- xTxList]
--
-----------------------------------------------------------------------------------------
--
