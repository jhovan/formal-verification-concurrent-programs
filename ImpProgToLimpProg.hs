module ImpProgToLimpProg
    (commToLComm, impProgToLabImpProg )
-----------------------------------------------------------------------------------------
-- Transforma un programa del lenguaje Imp a un programa etiquetado.
-- Author: mcb
--
--
-----------------------------------------------------------------------------------------
where
--import Data.List (nub, union)
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
import GlobalTypes 
    (VarId,EntryLabel) -- Value(..), Context
import GlobalFunctions (pcStatic) -- ,replaceInAssocL
import ImpSimpStmSyntax
    (SimpStm(..))
import ImpCompStmSyntax
    (CompStm(..) )
import ImpProgSyntax
    (Prog(..),ImpProg,ProgVarList(..),ImpVarType(..))
import ImpLabStmSyntax
    (LcompStm(..)
    ,exitLabelOf,setExitLabelOf)
import ImpLabProgSyntax
    (Lprog(..),LimpProg,ProgPC(..))
--
-----------------------------------------------------------------------------------------
--
-- Functions for transforming IMP programs to Labeled IMP programs:
--
simpToLSimp :: EntryLabel->(SimpStm e) -> LcompStm (SimpStm e)
-- Add a label to a simple statement of language IMP, sStm, starting at label i.
simpToLSimp i sStm =
    case sStm of
        Sidle       ->  -- idle statement pc=bottom:=0. Clarke, Model Checking,1999, p.21
                        LsimpleStm (0,Sidle,0) 
        _           -> LsimpleStm (i,sStm,i+1)
--
commToLComm :: EntryLabel->CompStm (SimpStm e) -> LcompStm (SimpStm e)
-- Add a label to a compound statement of language IMP, c, starting at label i.
commToLComm i cStm =
    case cStm of
        Csimp sStm              -> (simpToLSimp i sStm) --LsimpleStm (i,sS,i+1)
        CseqCompos (c1, c2)     -> LseqCompos (i,(c1',c2'),k) --c1;c2
                                    where
                                    c1' = commToLComm i c1
                                    j   = exitLabelOf c1'
                                    c2' = commToLComm j c2
                                    k   = exitLabelOf c2'
        CseqStmBlock cL         -> LseqStmBlok (i,cL',k)
                                    where
                                    cL' = cListToLcList i cL
                                    k   = exitLabelOf (last cL')
        CifThenElse b c1 c2     -> LifThenElse (i,(b, c1'', c2'),k) 
                                    where 
                                    c1' = commToLComm (i+1) c1
                                    c1''= setExitLabelOf c1' k
                                    j   = exitLabelOf c1'
                                    c2' = commToLComm j c2
                                    k   = exitLabelOf c2'
        Cwhile  b c             -> LwhileStm (i, (b,c'), k)
                                    where
                                    c'  = (commToLComm (i+1) c)
                                    k   = (exitLabelOf c')
        -- _                       -> error $ "commToLComm: Not implemented, c= "++ show c
--
cListToLcList :: EntryLabel -> [CompStm (SimpStm e)] -> [LcompStm (SimpStm e)]
-- Add labels to a list of compound statements of language IMP, lc, starting at label i.
cListToLcList i lc =
    case lc of
        []      -> []
        c:lc'   -> labC : cListToLcList j lc'
                    where 
                    labC    = (commToLComm i c)
                    j       = exitLabelOf labC
--
pcIdleValue :: EntryLabel
pcIdleValue = 0
--
labeledIdle :: LcompStm (SimpStm e)
labeledIdle = commToLComm pcIdleValue (Csimp Sidle)   
--
impProgToLabImpProg :: EntryLabel->VarId -> ImpProg -> LimpProg
-- Add a label to the instructions of a IMP program starting at label i.
impProgToLabImpProg i pcId (Prog (pName, ProgVarList varList, stm)) = 
    Lprog (pName, ProgVarList varList, ProgPC pcL, lStm0SeqLidle)
        where
        lStm0SeqLidle   = LseqCompos (i,(lStm0,labeledIdle),pcIdleValue) -- stm ; idle
        lStm            = commToLComm i stm
        lStm0           = setExitLabelOf lStm pcIdleValue
        stmExit         = exitLabelOf lStm - 1
        pcL             = (pcId,TimpRange (pcStatic,stmExit))
--
-----------------------------------------------------------------------------------------
--
