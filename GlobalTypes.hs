module GlobalTypes
-- 
-- mcb
--
where
-------------------------------------------------------------------------------
--
--import Data.List (delete) -- nub
--
-------------------------------------------------------------------------------
--
type VarId  = String -- Variable Id
--
type VarMN  = (VarId,Int,Int) -- (VarId,min,max). VarID with minimum and maximum values
--
data Value  = Vint Int | Vbool Bool | Vnothing deriving (Eq,Ord)
--
instance Show Value where
    show v= case v of
                 Vint n     -> show n
                 Vbool b    -> show b
                 Vnothing   -> ""
--
type Context = [(VarId,[Value])] -- List of pairs (Variable,[Value]) 
--
-------------------------------------------------------------------------------
--
-- Types for labels:
type EntryLabel = Int
type ExitLabel  = Int
type Labels = (EntryLabel,ExitLabel)
--

