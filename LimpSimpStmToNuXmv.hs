module LimpSimpStmToNuXmv (impSimpStmToNextExpr, curr, next, keepVars) --)
-------------------------------------------------------------------------------
-- Transforms simple statements, of the Imp language, into NuSMV next-expressions.
-- Constraints describeb by Clarke et al., C(...), are represented by next-expression of nuXmv.
-- See Clarke et al. Model Checking, 1999. pp.22-26
-- Author: Miguel Carrillo Barajas.
--
-------------------------------------------------------------------------------
where
--
import Data.List ((\\)) -- nub,delete,
--import qualified Data.Set as S (Set,fromList) -- (\\),toList,filter
--
-------------------------------------------------------------------------------
--
--import GlobalTypes (VarId, VarMN) -- Value(..)
--import GlobalFunctions (pcStatic)
--import ImpExpSyntax (ExpAB(..), ArthE(..), BoolE(..))
import NuSmvSpecs 
    (NextExpr(..), SmvId
    ,SimpleExpr(..) -- for using SEvar in NEnext
    ,idTOsmvId
    )
import ImpExpToNuXmv 
    (expABtoNextExpr) -- aExpToNextExpr, bExpToNextExpr, aExpToSimpleExpr, bExpToSimpleExpr, 
import ImpSimpStmSyntax (SimpStm(..))
import ImpLabStmSyntax (LcompStm(..), LimpStm)
--import IMPsemantics (evalAexp)
--
-------------------------------------------------------------------------------
--
curr :: SmvId -> NextExpr
-- (curr vId) is a shorthand expressing the current value of the variable identified by vId.
curr vId = (NEvar vId)
--
next :: SmvId -> NextExpr
-- (next vId) is a shorthand expressing the next value of the variable identified by vId.
next vId = (NEnext (SEvar vId))
--
keepVars :: [SmvId] -> NextExpr
-- keepVars(vXlist)= bigAnd_{v in vXlist} (v'=v). Clarke, Model Checking, p.21
keepVars vXlist = 
    case vXlist of
        vX:wX:lvX   -> ((next vX) `NEeq` (curr vX)) -- vX'=vX
                       `NEand`                      -- and
                       (keepVars (wX:lvX))
        [vX]        -> (next vX) `NEeq` (curr vX)   -- vX'=vX
        []          -> NEtrue
--
impSimpStmToNextExpr :: SmvId->[SmvId]->LimpStm -> NextExpr
-- Clarke, Model Checking, pp. 21-22.
-- Uso de variables con "prima" y model checking, también ver: Huth & Ryan Logic in computer science.
impSimpStmToNextExpr pcId vXlist sS = 
    case sS of
        LsimpleStm (l,Sskip,l')         -> -- C(l,skip,l')= (pc=l & pc'=l' & same(V))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                (keepVars vXlist)           -- keepVars (vXlist)
        LsimpleStm (l,Sassign v e,l')   ->  -- C(l,v:=e,l')= (pc=l & pc'=l' & v'=e & same(V-{v}))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                ((next  v) `NEeq` (expABtoNextExpr e))  -- next(v)=e
                `NEand`                                 -- and
                (keepVars (vXlist \\ [idTOsmvId v]))    -- keepVars (vXlist-[v])
        LsimpleStm (l,SdobleAsgn (v,w) (e,f),l') -- Doble assign, (v,w) := (e,f)
                ->  -- C(l,(v,w) ::= (e,f),l') = (pc=l & pc'=l' & v'=e & w'=f & same(V-{v,w}))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                ((next  v) `NEeq` (expABtoNextExpr e))  -- next(v)=e
                `NEand`                     -- and
                ((next  w) `NEeq` (expABtoNextExpr f))  -- next(w)=f
                `NEand`                     -- and
                (keepVars (vXlist \\ [idTOsmvId v, idTOsmvId w])) -- keepVars (vXlist-[v,w])
        LsimpleStm (l,Swrite _ _,l')    ->  -- C(l,write _ _,l')= (pc=l & pc'=l' & same(V))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                (keepVars vXlist)           -- keepVars (vXlist)
                -- Ahora lo que sigue es incorrecto:
        LsimpleStm (l,Sread b v,l')     ->  -- C(l,read _ _,l')= (pc=l & pc'=l' & same(V))
                -- Lo correcto es : C(l,read b v,l')= (pc=l & pc'=l' & same(V))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                --(keepVars vXlist)           -- keepVars (vXlist)
                (keepVars (vXlist \\ [idTOsmvId b, idTOsmvId v])) -- keepVars (vXlist-{b,v})
        LsimpleStm (l,Sidle,l')         ->  -- C(l,idle,l')= (pc=l & pc'=l' & same(V))
                (pc `NEeq` (NEint l))       -- pc=l
                `NEand`                     -- and
                (pc' `NEeq` (NEint l'))     -- next(pc)=l'
                `NEand`                     -- and
                (keepVars vXlist)           -- keepVars (vXlist)
        _                               ->  -- Only simple statements of Imp.
                        error $ "impSimpStmToNextExpr: "
                            ++ show sS ++ " is not a simple statement of Imp language"
    where
    pc  = curr pcId
    pc' = next pcId
--
-----------------------------------------------------------------------------------------
--
