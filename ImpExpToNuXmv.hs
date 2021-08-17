module ImpExpToNuXmv (aExpToSimpleExpr, bExpToSimpleExpr, aExpToNextExpr, bExpToNextExpr, expABtoNextExpr)
-- Expressions of IMP languange to NuXmv expressions
-- Author: Miguel Carrillo Barajas.
--
where
--
--import Data.List (nub) -- delete,
--import qualified Data.Set as S (Set,fromList) -- (\\),toList,filter
--
-------------------------------------------------------------------------------
--
import NuSmvSpecs (SimpleExpr(..), NextExpr(..), idTOsmvId)
import ImpExpSyntax (ArthE(..), BoolE(..), ExpAB(..))
--import GlobalTypes (VarId, VarMN) -- Value(..)
--import IMPsemantics (evalAexp)
--
-------------------------------------------------------------------------------
--
aExpToSimpleExpr :: ArthE -> SimpleExpr
aExpToSimpleExpr ae =
    case ae of
        Aint n      -> SEint n
        Avar v      -> SEvar (idTOsmvId v) 
        Asum a1 a2  -> SEadd (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Adif a1 a2  -> SEdif (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Aprod a1 a2 -> SEmul (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Adiv a1 a2  -> SEdiv (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Amod a1 a2  -> SEmod (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
--
bExpToSimpleExpr :: BoolE -> SimpleExpr
bExpToSimpleExpr be = 
    case be of
        BT          -> SEtrue
        BF          -> SEfalse
        Bvar v      -> SEvar (idTOsmvId v)
        Bequ a1 a2  -> SEeq (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Bleq a1 a2  -> SEleq (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Bgt  a1 a2  -> SEgt (aExpToSimpleExpr a1) (aExpToSimpleExpr a2)
        Bnot b1     -> SEnot (bExpToSimpleExpr b1)
        Band b1 b2  -> SEand (bExpToSimpleExpr b1) (bExpToSimpleExpr b2)
        Bor  b1 b2  -> SEor (bExpToSimpleExpr b1) (bExpToSimpleExpr b2)
--
aExpToNextExpr :: ArthE -> NextExpr
aExpToNextExpr ae =
    case ae of
        Aint n      -> NEint n
        Avar v      -> NEvar (idTOsmvId v)
        Asum a1 a2  -> NEadd (aExpToNextExpr a1) (aExpToNextExpr a2)
        Adif a1 a2  -> NEdif (aExpToNextExpr a1) (aExpToNextExpr a2)
        Aprod a1 a2 -> NEmul (aExpToNextExpr a1) (aExpToNextExpr a2)
        Adiv a1 a2  -> NEdiv (aExpToNextExpr a1) (aExpToNextExpr a2)
        Amod a1 a2  -> NEmod (aExpToNextExpr a1) (aExpToNextExpr a2)
--
bExpToNextExpr :: BoolE -> NextExpr
bExpToNextExpr be = 
    case be of
        BT          -> NEtrue
        BF          -> NEfalse
        Bvar v      -> NEvar (idTOsmvId v)
        Bequ a1 a2  -> NEeq (aExpToNextExpr a1) (aExpToNextExpr a2)
        Bleq a1 a2  -> NEleq (aExpToNextExpr a1) (aExpToNextExpr a2)
        Bgt  a1 a2  -> NEgt (aExpToNextExpr a1) (aExpToNextExpr a2)
        Bnot b1     -> NEnot (bExpToNextExpr b1)
        Band b1 b2  -> NEand (bExpToNextExpr b1) (bExpToNextExpr b2)
        Bor  b1 b2  -> NEor (bExpToNextExpr b1) (bExpToNextExpr b2)
--
expABtoNextExpr :: ExpAB -> NextExpr
expABtoNextExpr eAB = 
    case eAB of
        Earith aE   -> aExpToNextExpr aE
        Ebool bE    -> bExpToNextExpr bE
--
-----------------------------------------------------------------------------------------
--
