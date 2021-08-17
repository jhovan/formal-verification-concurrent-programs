module NuSmvSpecs 
    (SmvId, SimpleExpr(..), NextExpr(..), CTL(..), LTL(..), NuSMVmodule, NuSMVmodel, ModuleElement(..), VarType(..), Assignment(..), idTOsmvId, setTypeRangelIn, setTypeBooleanIn, setTypeModuleIn, 
    neOrList, neAndList,
    seOrList, seAndList, 
    neExpKeepVarL)
-------------------------------------------------------------------------------
-- Functions to build NuSMV specs.
-- See NuSmvToString for functions to export NuSMV specs to a text file.
-- Author Miguel Carrillo Barajas.
--
-------------------------------------------------------------------------------
where
--
--import Data.List (nub) -- delete,
--import qualified Data.Set as S (Set,fromList) -- (\\),toList,filter
--
-------------------------------------------------------------------------------
--
-- BEGIN Data and types for a subset of NuSMV specifications: -----------------
--
type SmvId = String -- NuSMV ids 
--
-- Types of variables
data VarType= 
      TypeBoolean 
    | TypeRange Int Int 
    | TypeInt                   -- See [*1] below
    | TypeModule SmvId [SmvId] 
    deriving (Show,Eq)
--
-- From NuSMV 2.6 user manual:
-- [*1] The domain of the integer type is any Whole Number, positive or negative.
-- Although the integer type is used to represent integer enum type when explaining the NUXMV type system,
-- there are important differences which are needed to keep in mind.  
-- First, using integer is not allowed in certain Model Checking engines and algorithms. 
-- Second, at the moment, there are implementation-dependent constraints on the integer enum type, 
-- as integer numbers can only be in the range−232+ 1 to 232−1 
-- (more accurately,these values are equivalent to the C/C++ macros INTMIN+1 and INTMAX).
--
-- Types of assignments:
-- From NuSMV 2.6 user manual, pp.28-29:
data Assignment=  VarA  SmvId SimpleExpr    --       v := exp
                | InitA SmvId SimpleExpr    -- init(v) := exp
                | NextA SmvId NextExpr      -- next(v) := exp
                deriving (Show,Eq)
--
data SimpleExpr= -- Simple expressions refer to the current state, without next.
-- From NuSMV 2.6 user manual, p.21:
-- Simple expressions are expressions built only from the values of variables in the current state. 
-- Therefore, the simple expression cannot have a next operation inside.
-- Simple expressions can be used to specify sets of states, e.g. an initial set of states.
    -- Constants:
      SEfalse                       -- FALSE 
    | SEtrue                        -- TRUE
    | SEint Int                     -- integer constants
    | SEvar SmvId                   -- variables
    -- Boolean expressions:
    | SEeq SimpleExpr SimpleExpr    -- (v=n)
    | SEleq SimpleExpr SimpleExpr   -- (v<=n)
    | SEgt SimpleExpr SimpleExpr    -- (v>n)
    | SEin SmvId Int Int            -- v in {n..m} 
    | SEnot SimpleExpr              -- not
    | SEor SimpleExpr SimpleExpr    -- or 
    | SEand SimpleExpr SimpleExpr   -- and 
    | SEimp SimpleExpr SimpleExpr   -- implication
    | SEiff SimpleExpr SimpleExpr   -- iff
    -- Arithmetic expressions:
    | SEadd SimpleExpr SimpleExpr   -- addition 
    | SEdif SimpleExpr SimpleExpr   -- difference
    | SEmul SimpleExpr SimpleExpr   -- multiplication
    | SEdiv SimpleExpr SimpleExpr   -- integer division
    | SEmod SimpleExpr SimpleExpr   -- integer remainder, module operation
    --
    | SEcase [(SimpleExpr,SimpleExpr)]  --   
    | SElistV [Int]                     --
    | SElistB [Bool]                    --
    | SElistR (Int,Int)                 --
    deriving (Show,Eq)
--
-----------------------------------------------------------------------------------------
--
data NextExpr= --Next expressions relate current and next state variables.
-- From NuSMV 2.6 user manual, p.21:
-- The next expression relates current and next state variables to express transitions in the FSM. 
-- The next expression can have next operation inside.
-- Note that the next operator cannot be applied twice, i.e. next(next(a)) is not allowed.
-- Here we allow to apply "next" only to variables, "next(v)", we do not allow "next(basic_expr)".
    -- Constants:
      NEfalse                       -- FALSE 
    | NEtrue                        -- TRUE
    | NEint Int                     -- integer constants
    | NEvar SmvId                   -- variables
    | NEdef SmvId                   -- define Id
    -- Boolean expressions:
    | NEeq NextExpr NextExpr    -- (v=n)
    | NEleq NextExpr NextExpr   -- (v<=n)
    | NEgt NextExpr NextExpr    -- (v>n)
    | NEin SmvId Int Int        -- v in {n..m} 
    | NEnot NextExpr            -- not
    | NEor NextExpr NextExpr    -- or 
    | NEand NextExpr NextExpr   -- and 
    | NEimp NextExpr NextExpr   -- implication
    | NEiff NextExpr NextExpr   -- iff
    -- Arithmetic expressions:
    | NEadd NextExpr NextExpr   -- addition 
    | NEdif NextExpr NextExpr   -- difference
    | NEmul NextExpr NextExpr   -- multiplication
    | NEdiv NextExpr NextExpr   -- integer division
    | NEmod NextExpr NextExpr   -- integer remainder, module operation
    --
    | NEcase [(NextExpr,NextExpr)]  --   
    | NElistV [Int]                 --
    | NElistB [Bool]                --
    | NElistR (Int,Int)             --
    --
    | NEnext SimpleExpr -- next(simpleExp), but we procure use only next(variable)
    --
    | NEcondNextL [(NextExpr,NextExpr)]                 -- To print [(condExp,nextExp)] by rows
    | NEnCondPcondNextL [(NextExpr,NextExpr,NextExpr)]  -- To print [(pCond,nCond,nextExp)] by rows
    deriving (Show,Eq)
--
data ModuleElement= -- Module elements.
      VarDecl [(SmvId, VarType)]        -- Variables declared with VAR 
    | DefineDecl [(NextExpr,NextExpr)]  -- [(neId,neExp)]. DEFINE neId:=neExp;. neExp without NEnext
    | AssignConstr [Assignment]         -- Necessary for NuSMV semantics using ASSIGN init(v) next(v) 
    | InitConstr  SimpleExpr            -- Necessary for NuSMV semantics using INIT 
    | TransConstr NextExpr              -- Necessary for NuSMV semantics using TRANS
    | Comment String                    -- To insert comments between module elements
    | CtlSpec CTL                       -- CTL formulas declared with CTLSPEC 
    -- | LtlSpec LTL                       -- LTL formulas declared with LTLSPEC 
    deriving (Show,Eq)
--
type NuSMVmodule= (String,[SmvId],[ModuleElement]) -- (moduleName,parameters,moduleElements)
--
type NuSMVmodel= [NuSMVmodule] -- NuSMVmodel= list of NuSMV modules
--
-- END   Data and types for a subset of NuSMV specifications: -----------------
--
-- CTL formulas ---------------------------------------------------------------
--
type EnumExp= Int -- Simple Enum expressions
--
-- CTL formulas:
data CTL = FalseF | TrueF           --false, true
    | LitP SmvId | LitN SmvId       --literals 
    --Comparison formulas, v==n, v/=n, v<=n, v>n.
    | Eqf  SmvId EnumExp            -- v == n
    | Neqf SmvId EnumExp            -- v /= n 
    | Leqf SmvId EnumExp            -- v <= n
    | Gtf  SmvId EnumExp            -- v >  n
    --
    | Orf CTL CTL | Andf CTL CTL    -- Or, And
    | EXf CTL     | AXf CTL         -- EX, AX
    | EUf CTL CTL | AUf CTL CTL     -- EU, AU
    | ERf CTL CTL | ARf CTL CTL     -- ER, AR
    --  Other CTL formulas:     
    | Notf CTL                      -- Negation, not present in NNF
    | Impf CTL CTL                  -- implication g imp h= !g | h
    | Eqvf CTL CTL                  -- equivalence g eqv h= (g imp h) & (h imp g)
    | Xorf CTL CTL                  -- exclusive or, g xor h= (g | h) & (!(g & h)) 
    | EFf CTL | AFf CTL             -- EF, AF 
    | EGf CTL | AGf CTL             -- EG, AG 
    | EWf CTL CTL | AWf CTL CTL     -- Weak Until, g W h= (g U h) | G g     See Huth-Ryan, LICS
    -- Other operators:
    | Reachablef CTL        -- reachable(g)     = (EF g) 
    | Stablef CTL           -- stable(g)        = (AG g) 
    | Steadyf CTL           -- steady(g)        = (EG g)
    | Checkpointf CTL CTL   -- checkpoint(g, h) = (!E[!g U h]) 
    | Loopf CTL CTL         -- loop(g, h)       = (AG ((g -> (EF h)) & (h -> (EF g))) ) approx. loop 
    | Oscilf CTL            -- oscil(g)         = loop(g,!(g)) approximates the oscillation property
    deriving (Show,Eq,Ord)
--
-------------------------------------------------------------------------------
--
-- LTL formulas ---------------------------------------------------------------
--
data LTL = LFalseF | LTrueF           --false, true
    | LLitP SmvId | LLitN SmvId       --literals 
    --Comparison formulas, v==n, v/=n, v<=n, v>n.
    | LEqf  SmvId EnumExp            -- v == n
    | LNeqf SmvId EnumExp            -- v /= n 
    | LLeqf SmvId EnumExp            -- v <= n
    | LGtf  SmvId EnumExp            -- v >  n
    --
    | LOrf LTL LTL | LAndf LTL LTL  -- Or, And
    | LXf LTL                       -- X
    | LUf LTL LTL                   -- U
    | LRf LTL LTL                   -- R
    --  Other LTL formulas:     
    | LNotf LTL                      -- Negation, not present in NNF
    | LImpf LTL LTL                  -- implication g imp h= !g | h
    | LEqvf LTL LTL                  -- equivalence g eqv h= (g imp h) & (h imp g)
    | LXorf LTL LTL                  -- exclusive or, g xor h= (g | h) & (!(g & h)) 
    | LFf LTL                   -- F
    | LGf LTL                   -- G
    | LWf LTL LTL               -- Weak Until, g W h= (g U h) | G g   See Huth-Ryan, LICS
    -- Other operators:
    | LReachablef LTL        -- reachable(g)     = (F g) 
    | LStablef LTL           -- stable(g)        = (G g) 
    | LCheckpointf LTL LTL   -- checkpoint(g, h) = (g R !h) 
    | LLoopf LTL LTL         -- loop(g, h)       = (G ((g -> (F h)) & (h -> (F g))) ) approx. loop 
    | LOscilf LTL            -- oscil(g)         = loop(g,!(g)) approximates the oscillation property
    deriving (Show,Eq,Ord)
--
-------------------------------------------------------------------------------
--
nuSMVkeyWords :: [SmvId]
-- From NuSMV 2.6 user manual, pp.6-7:
nuSMVkeyWords = [
    "MODULE", "DEFINE", "MDEFINE", "CONSTANTS", "VAR", "IVAR", "FROZENVAR",
    "INIT", "TRANS", "INVAR", "SPEC", "CTLSPEC", "LTLSPEC", "PSLSPEC", "COMPUTE",
    "NAME", "INVARSPEC", "FAIRNESS", "JUSTICE", "COMPASSION", "ISA", "ASSIGN",
    "CONSTRAINT", "SIMPWFF", "CTLWFF", "LTLWFF", "PSLWFF", "COMPWFF", "IN", "MIN",
    "MAX", "MIRROR", "PRED", "PREDICATES", 
    "process", "array", "of", "boolean", "integer", "real", 
    "word", "word1", "bool", "signed", "unsigned", "extend", "resize", "sizeof", "uwconst", "swconst", 
    "EX", "AX", "EF", "AF", "EG", "AG", "E", "F", "O", "G", "H", "X", "Y", "Z", "A", "U", "S", "V", "T", 
    "BU", "EBF", "ABF", "EBG", "ABG", 
    "case", "esac", "mod", "next",
    "init", "union", "in", "xor", "xnor", "self", "TRUE", "FALSE", "count", "abs", "max", "min" ]
--
-------------------------------------------------------------------------------
--
idTOsmvId :: SmvId->SmvId
-- Make varIds different from NuSMV reserved keywords by prefixing them with '_'
idTOsmvId x= 
    if x `notElem` nuSMVkeyWords
        then x
        --else '_':x -- prefix x with '_'
        else error $ "idTOsmvId: "++ x ++ " is a NuSMV reserved keyword. Advice: use "++ ('_':x)
--
setTypeBooleanIn :: [SmvId] -> [(SmvId, VarType)]
--Set type 'TypeBoolean' in a list of varIds.
--Precondition: elements of smvIdL are valid ids in NuSMV
setTypeBooleanIn smvIdL = zip smvIdL (repeat TypeBoolean)
--
setTypeRangelIn :: [SmvId]->Int->Int -> [(SmvId, VarType)]
--Set type 'TypeRange m n' in a list of varIds
--Precondition: elements of smvIdL are valid ids in NuSMV
--SE PUEDE USAR: minBound and maxBound al llamar a setTypeRangelIn
setTypeRangelIn smvIdL m n = zip smvIdL (repeat $ TypeRange m n)
--
setTypeModuleIn :: [(SmvId,[SmvId])] -> [(SmvId, VarType)]
--Set type 'TypeModule' in a list of (fsmNames,parameters). 
--Precondition: elements of nameParamList contain valid ids in NuSMV
setTypeModuleIn nameParamList = 
    [(fsm,TypeModule fsm globV) | (fsm,globV) <- nameParamList]
--
-- Functions for NEor and NEand over lists: -----------------------------------
neOrList :: [NextExpr] -> NextExpr
neOrList expL = case expL of
                    [e]     -> e
                    e:eL    -> (e `NEor` neOrList eL)
                    []      -> NEfalse
--
neAndList :: [NextExpr] -> NextExpr
neAndList expL = case expL of
                    [e]     -> e
                    e:eL    -> (e `NEand` neAndList eL)
                    []      -> NEtrue
--
-- Functions for SEor and SEand over lists: -----------------------------------
seOrList :: [SimpleExpr] -> SimpleExpr
seOrList expL = case expL of
                    [e]     -> e
                    e:eL    -> (e `SEor` seOrList eL)
                    []      -> SEfalse
--
seAndList :: [SimpleExpr] -> SimpleExpr
seAndList expL = case expL of
                    [e]     -> e
                    e:eL    -> (e `SEand` seAndList eL)
                    []      -> SEtrue
--
-- Function to keep the value of variables in a list: -------------------------
neExpKeepVarL :: [SmvId] -> NextExpr
neExpKeepVarL varList = 
    case varList of
        [v]     -> ((NEnext (SEvar v)) `NEeq` (NEvar v)) -- next(v) = v
        v:lv    -> ((NEnext (SEvar v)) `NEeq` (NEvar v)) -- next(v) = v
                   `NEand`                               --     and   
                   (neExpKeepVarL lv)                    --     ...
        []      -> NEtrue
--
--
