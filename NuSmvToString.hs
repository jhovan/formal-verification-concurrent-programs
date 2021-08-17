module NuSmvToString
    (nuSmvModelTOstring
    ,nextExprTOstr
    )
-------------------------------------------------------------------------------
-- Functions to transform NuSMV specs to strings for generating a NuSMV file.
-- See NuSmvToString for functions to build NuSMV specs to a NuSMV file.
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
import NuSmvSpecs (SmvId, SimpleExpr(..), NextExpr(..), CTL(..), NuSMVmodule, NuSMVmodel, ModuleElement(..), VarType(..), Assignment(..))
--
-------------------------------------------------------------------------------
--
showSepStringL :: String->[String] -> String
--Show elements of list of strings l separated by sep
showSepStringL sep l = case l of
                            []    -> ""
                            [r]   -> r
                            r:lr  -> r ++ sep ++ showSepStringL sep lr
--
--Test:
--putStrLn $ showSepStringL ", " ["a","b","c","d"] 
--
-- Begin Print NuSMV Modules --------------------------------------------------
--
showCTLnusmv :: CTL -> String
-- Convert a CTL formula to a string with NuSMV syntax
showCTLnusmv phi= case phi of
    TrueF       -> "TRUE"   -- NuSMV syntax
    FalseF      -> "FALSE"  -- NuSMV syntax
    (LitP v)    -> v 
    (LitN v)    -> "!"++v 
    --
    Eqf v c     -> "("++v++ " = "  ++ show c ++")"
    Neqf v c    -> "("++v++ " != " ++ show c ++")"
    Leqf v c    -> "("++v++ " <= " ++ show c ++")"
    Gtf v c     -> "("++v++ " > "  ++ show c ++")"
    --
    (Orf f g)   -> "("++ showCTLnusmv f ++ " | "++ showCTLnusmv g ++")"
    (Andf f g)  -> "("++ showCTLnusmv f ++ " & "++ showCTLnusmv g ++")"
    --
    (EXf f)     -> "EX "++ showCTLnusmv f
    (AXf f)     -> "AX "++ showCTLnusmv f
    --
    (EUf f g)   -> "E["++  showCTLnusmv f ++" U "++ showCTLnusmv g ++"]"
    (AUf f g)   -> "A["++  showCTLnusmv f ++" U "++ showCTLnusmv g ++"]"
    --
    (ERf f g)   -> "!A["++ showCTLnusmv (Notf f)++" U "++ showCTLnusmv (Notf g) ++"]"  -- NuSMV syntax
    (ARf f g)   -> "!E["++ showCTLnusmv (Notf f) ++" U "++ showCTLnusmv (Notf g) ++"]"  -- NuSMV syntax
    --
    (EFf f)     -> "EF "++ showCTLnusmv f
    (AFf f)     -> "AF "++ showCTLnusmv f
    (EGf f)     -> "EG "++ showCTLnusmv f
    (AGf f)     -> "AG "++ showCTLnusmv f
    (Notf f)    -> "!"  ++ showCTLnusmv f
    (Impf f g)  -> "("  ++ showCTLnusmv f ++" -> " ++ showCTLnusmv g ++")"
    --Other operators
    (Eqvf f g)  -> "("++  showCTLnusmv f ++" <-> "++ showCTLnusmv g ++")"  -- equivalence, f <-> g 
    (Xorf f g)  -> "("++  showCTLnusmv f ++" xor "++ showCTLnusmv g ++")"  -- exclusive or, f or g 
    (EWf f g)   -> "E["++ showCTLnusmv f ++" W "++   showCTLnusmv g ++"]"  -- weak until
    (AWf f g)   -> "A["++ showCTLnusmv f ++" W "++   showCTLnusmv g ++"]"  -- weak until
    (Reachablef g)  -> "reachable("++   showCTLnusmv g ++ ")"         -- reachable(g)=EF(g) 
    (Stablef g)     -> "stable("++      showCTLnusmv g ++ ")"         -- stable(g)=AG(g) 
    (Steadyf g)     -> "steady("++      showCTLnusmv g ++ ")"         -- steady(g)=EG(g) 
    (Checkpointf g h)   -> "checkpoint("++ showCTLnusmv g ++ ","++ showCTLnusmv h ++ ")" 
                        -- checkpoint(g,h)= !E[!g U h] 
    (Loopf g h)     -> "loop("++  showCTLnusmv g ++","++ showCTLnusmv h ++ ")"
                    -- loop(g,h)=AG((g-> (EF h)) & (h-> (EF g))) 
    (Oscilf g)      -> "oscil("++ showCTLnusmv g ++ ")"
                    -- oscil(g)= loop(g,!g), approx. oscillation 
    --_           -> error $ "showCTLnusmv: not defined in this case: "++ show phi
--
simpleExprTOstr :: SimpleExpr -> String
--SimpleExpr to String, with a separation in ORs 
simpleExprTOstr f= case f of
    SEfalse      -> "FALSE"
    SEtrue       -> "TRUE"
    SEint n      -> show n
    SEvar v      -> v
    --
    SEeq g h     -> "("++ simpleExprTOstr g++  " = "++ simpleExprTOstr h ++ ")" -- g=h
    SEleq g h    -> "("++ simpleExprTOstr g++  " <= "++ simpleExprTOstr h ++ ")" -- g=h
    SEgt g h     -> "("++ simpleExprTOstr g++  " > "++ simpleExprTOstr h ++ ")" -- g=h
    SEin v n m   -> "("++ v++  " in {"++  show n ++ " .. "++ show m ++ "})" -- v in {n .. m} 
    SEnot g      -> "!"++ simpleExprTOstr g
    SEor g h     -> "("++ simpleExprTOstr g
                    -- ++ "\n\t" -- Separation of OR
                    ++" | " ++ simpleExprTOstr h ++")"
    SEand g h    -> "("++ simpleExprTOstr g ++" & "  ++ simpleExprTOstr h ++")"
    SEimp g h    -> "("++ simpleExprTOstr g ++ " -> "++ simpleExprTOstr h ++")"
    SEiff g h    -> "("++ simpleExprTOstr g ++" <-> "++ simpleExprTOstr h ++")"
    --
    SEadd g h    -> "("++ simpleExprTOstr g ++" + "  ++ simpleExprTOstr h ++")"
    SEdif g h    -> "("++ simpleExprTOstr g ++" - "  ++ simpleExprTOstr h ++")"
    SEmul g h    -> "("++ simpleExprTOstr g ++" * "  ++ simpleExprTOstr h ++")"
    SEdiv g h    -> "("++ simpleExprTOstr g ++" / "  ++ simpleExprTOstr h ++")"
    SEmod g h    -> "("++ simpleExprTOstr g ++" mod "  ++ simpleExprTOstr h ++")"
    --
    SElistV lv       -> "{"++ intListTOstr lv ++"}" --we use (ElistV [n]) for a value n
    SElistB lb       -> "{"++ boolListTOstr lb ++"}"
    SElistR (n1,n2)  -> show n1 ++ ".." ++ show n2
    SEcase lce       -> "case\n"++ seCaseListTOstr lce 1 ++ "esac"
    --    _        -> error $ "simpleExprTOstr: not defined in this case: " ++ show f
--
nextExprTOstr :: Int->NextExpr -> String
nextExprTOstr i f = case f of -- i= indentation
    NEfalse      -> indent i++ "FALSE"
    NEtrue       -> indent i++ "TRUE"
    NEint n      -> indent i++ show n
    NEvar v      -> indent i++ v
    NEdef v      -> indent i++ v
    --
    NEeq g h     -> indent i++ "("++ nextExprTOstr 0 g++  " = "++ nextExprTOstr 0 h ++ ")" -- g=h
    NEleq g h    -> indent i++ "("++ nextExprTOstr 0 g++  " <= "++ nextExprTOstr 0 h ++ ")" -- g=h
    NEgt g h     -> indent i++ "("++ nextExprTOstr 0 g++  " > "++ nextExprTOstr 0 h ++ ")" -- g=h
    NEin v n m   -> indent i++ "("++ v++  " in {"++  show n ++ " .. "++ show m ++ "})" -- v in {n .. m} 
    NEnot g      -> indent i++ "!"++ "("++ nextExprTOstr 0 g ++")"
    NEor g h     -> indent i
                    ++ "("++ nextExprTOstr 0 g ++ "\n"
                    ++ " | " ++ nextExprTOstr 0 h ++ ")"
    NEand g h    -> indent i++ "("++ nextExprTOstr 0 g ++" & "  ++ nextExprTOstr 0 h ++")"
    NEimp g h    -> indent i++ "("++ nextExprTOstr 0 g ++ " -> "++ nextExprTOstr 0 h ++")"
    NEiff g h    -> indent i++ "("++ nextExprTOstr 0 g ++" <-> "++ nextExprTOstr 0 h ++")"
    --
    NEadd g h    -> indent i++ "("++ nextExprTOstr 0 g ++" + "  ++ nextExprTOstr 0 h ++")"
    NEdif g h    -> indent i++ "("++ nextExprTOstr 0 g ++" - "  ++ nextExprTOstr 0 h ++")"
    NEmul g h    -> indent i++ "("++ nextExprTOstr 0 g ++" * "  ++ nextExprTOstr 0 h ++")"
    NEdiv g h    -> indent i++ "("++ nextExprTOstr 0 g ++" / "  ++ nextExprTOstr 0 h ++")"    
    NEmod g h    -> indent i++ "("++ nextExprTOstr 0 g ++" mod "  ++ nextExprTOstr 0 h ++")"    
    --
    NElistV lv      -> indent i++ "{"++ intListTOstr lv ++"}" --we use (ElistV [n]) for a value n
    NElistB lb      -> indent i++ "{"++ boolListTOstr lb ++"}"
    NElistR (n1,n2) -> indent i++ show n1 ++ ".." ++ show n2
    NEcase lce      -> indent i++ "case\n"++ neCaseListTOstr lce i ++ "esac"
    --
    NEnext sExp     -> indent i++ "next("++ simpleExprTOstr sExp ++")"
    --
    NEcondNextL ceL         -> neCondExpListTOstrI ceL i
    NEnCondPcondNextL npxL  -> nCondPcondNextListTOstrI npxL i
    --    _           -> error $ "nextExprTOstr: not defined in this case: " ++ show f
--
nuSmvModelTOstring ::NuSMVmodel -> String
-- nuSmvModelTOstring
nuSmvModelTOstring moduleList = 
       "-- Model" ++ " modelNameXXX" ++ " modelParamsXXX" ++ "\n"
    ++ "-- Text of this nuXmv model was automatically generated by nuSmvModelTOstring.\n"
    ++ replicate 80 '-' ++ "\n" -- end comment
    ++ concatMap nuSMVmoduleTOstr moduleList
--
nuSMVmoduleTOstr ::NuSMVmodule -> String
-- nuSMVmoduleTOstr
nuSMVmoduleTOstr (name,paramL,body) = 
       "MODULE "++name ++paramsStr++ "\n"
    ++ "-- Text of this NuSMV module was automatically generated by nuSMVmoduleTOstr.\n"
    ++ modElementListTOstring body
    ++ "-- End module " ++ name ++ ".\n"
    ++ replicate 80 '-' ++ "\n" -- end module
    where
    paramsStr = if paramL==[]
                   then ""
                   else " ("++ showSepStringL ", " paramL ++")"
--
modElementListTOstring :: [ModuleElement] -> String
--List of module elements of NuSMV to string
-- modElementListTOstring []    = ""
-- modElementListTOstring (e:le)= moduleElementTOstring e ++ modElementListTOstring le
modElementListTOstring = foldr ((++) . moduleElementTOstring) "" 
--
--moduleElementTOstring
moduleElementTOstring :: ModuleElement -> String
moduleElementTOstring e= case e of
    VarDecl lv      -> "VAR\n"      ++ varDeclTOstr lv ++ "\n"
    DefineDecl lvd  -> "DEFINE\n"   ++ defDecListTOstr 0 lvd ++ "\n"
    AssignConstr la -> "ASSIGN\n"   ++ assignConstrTOstr la ++ "\n\n"
    InitConstr  phi -> "INIT\n"     ++ simpleExprTOstr phi ++ "\n\n"
    TransConstr phi -> "TRANS\n"    ++ nextExprTOstr 0 phi ++ "\n\n"
    Comment commStr -> commStr
    CtlSpec f       -> "CTLSPEC\n"  ++ showCTLnusmv f ++ "\n\n" 
    --_               -> error ("moduleElementTOstring: not defined in this case: "++(show e))
--
varDeclTOstr :: [(SmvId, VarType)] -> String
-- List of variable declarations of NuSMV to string
-- varDeclTOstr []= ""
-- varDeclTOstr (v:lv)= varTOstr v ++  varDeclTOstr lv
varDeclTOstr = foldr ((++) . varTOstr) "" 
--
--varTOstr
varTOstr :: (SmvId, VarType) -> String
varTOstr (v,t)= case t of
    TypeBoolean                 -> v++ " : boolean;\n"
    TypeRange n m               -> v++ " : "++ show n ++ ".."++ show m ++ ";\n"
    TypeInt                     -> v++ " : integer;\n" -- See sec. 2.1.4 of nuXmv user manual
    TypeModule modName globV    -> v++ " : "++ modName++ paramStr ++ ";\n"
                where
                paramStr= if globV==[]
                            then ""
                            --else " ("++ showSepStringL ", " (map idTOsmvId globV) ++")"
                            else " ("++ showSepStringL ", " globV ++")"
--    _                           -> error ("varTOstr: not defined in this case: "++(show t))
--
varDefTOstr :: Int->(NextExpr,NextExpr) -> String
-- Note that (NuSMV 2.6 User Manual, p.14): 
-- "Define expressions may contain next operators; Normal rules apply: No nested next operators."
varDefTOstr i varDef = 
    case varDef of
        (NEdef v,def)   -> indent i ++ v ++ " :=\n"
                            ++ nextExprTOstr (i+1) def ++";\n" --i= indentation
        _               -> error $ "varDefTOstr: invalid Define Id: "++ show (fst varDef)
--
defDecListTOstr :: Int->[(NextExpr,NextExpr)] -> String
defDecListTOstr i varDefList = 
    case varDefList of
        [vd]    -> varDefTOstr i vd
        vd:lvd  -> varDefTOstr i vd ++ defDecListTOstr i lvd
        []      -> "\n"
--
assignConstrTOstr :: [Assignment] -> String
-- List of assignment constraints of NuSMV to string
-- assignConstrTOstr []= ""
-- assignConstrTOstr (a:la)= assignmentTOstr a ++ assignConstrTOstr la
assignConstrTOstr = foldr ((++) . assignmentTOstr) "" 
--
--assignmentTOstr
assignmentTOstr :: Assignment -> String
assignmentTOstr a= case a of
    VarA  v e -> v++" := " ++ simpleExprTOstr e ++ ";\n"
    InitA v e -> "init("++v++") := " ++ simpleExprTOstr e ++ ";\n"
    NextA v e -> "next("++v++") := " ++ nextExprTOstr 0 e ++ ";\n"
--    _         -> error ("assignmentTOstr: not defined in this case: "++(show a))
--
intListTOstr :: [Int] -> String
intListTOstr [] =""
intListTOstr (v:lv)= case lv of
                        []  -> show v
                        _   -> show v ++ "," ++ intListTOstr lv
--
boolListTOstr :: [Bool] -> String
boolListTOstr [] =""
boolListTOstr (b:lb)= case lb of
                        []  -> showBool b
                        _   -> showBool b ++ "," ++ boolListTOstr lb
--
showBool :: Bool -> String
showBool b= if b then "TRUE" else "FALSE" -- NuSMV needs capital letters
--
--
seCaseListTOstr :: [(SimpleExpr,SimpleExpr)]->Int -> String
seCaseListTOstr []       i= indent i -- "" -- i= indentation, number of ' ' or '\t'
seCaseListTOstr (ce:lce) i= indent i ++ seCondExprTOstrI ce i ++ seCaseListTOstr lce i
--
seCondExprTOstrI :: (SimpleExpr,SimpleExpr)->Int -> String
seCondExprTOstrI (f,e) i=  simpleExprTOstr f ++ " : \n"
                      ++ indent (i+1)++ simpleExprTOstr e ++ ";\n" --i= indentation, number of ' ' or '\t'
--
neCaseListTOstr :: [(NextExpr,NextExpr)]->Int -> String
neCaseListTOstr []       i= indent i -- "" -- i= indentation, number of ' ' or '\t'
neCaseListTOstr (ce:lce) i= neCondExprTOstrI ce i ++ neCaseListTOstr lce i
--
neCondExprTOstrI :: (NextExpr,NextExpr)->Int -> String
neCondExprTOstrI (f,e) i=  nextExprTOstr i f ++ ":\n"
                        ++ nextExprTOstr (i+1) e ++ ";\n" --i= indentation, number of ' ' or '\t'
--
neCondExpTOandStrI :: (NextExpr,NextExpr)->Int -> String
-- i= indentation, number of ' ' or '\t'
neCondExpTOandStrI (f,e) i
    =  "("++ nextExprTOstr i f ++ " & " ++ condComment++ "\n" 
        ++ nextExprTOstr (i+1) e ++")"  ++ nextComment++ "\n" 
    where
    condComment = " -- cond"
    nextComment = " -- next"
--
neCondExpListTOstrI :: [(NextExpr,NextExpr)]->Int -> String
neCondExpListTOstrI condExpL i =
    case condExpL of
        [ce]        -> neCondExpTOandStrI ce i
        (ce:lce)    -> neCondExpTOandStrI ce i 
                        ++indent i ++ "| " 
                        ++ neCondExpListTOstrI lce i
        []          -> "TRUE" -- [] = no constraint = TRUE
--
indent :: Int -> String
indent i= replicate (4*i) ' ' -- from List of modules, i= indentation, number of ' ' or '\t'
--
neNegPosNextTOandStrI :: (NextExpr,NextExpr,NextExpr)->Int -> String
-- i= indentation, number of ' ' or '\t'
neNegPosNextTOandStrI (n,p,x) i
    =   indent i++ "("++
        nextExprTOstr 0 n     ++ " & " ++ negCondComment++ "\n"
     ++ nextExprTOstr i p     ++ " & " ++ posCondComment++ "\n" 
     ++ nextExprTOstr (i+1) x ++ ")"   ++ nextComment ++ "\n"
    where
    negCondComment  = " -- Ncond"
    posCondComment  = " -- Pcond"
    nextComment     = " -- Next"
--
nCondPcondNextListTOstrI :: [(NextExpr,NextExpr,NextExpr)]->Int -> String
nCondPcondNextListTOstrI nCondPcondNextL i =
    case nCondPcondNextL of
        [npx]       -> neNegPosNextTOandStrI npx i 
        (npx:lnpx)  -> neNegPosNextTOandStrI npx i 
                        ++indent i ++ "| \n" 
                        ++ nCondPcondNextListTOstrI lnpx i
        []          -> indent i ++ "TRUE" -- [] = no constraint = TRUE
--
-- -- End Print NuSMV Modules -------------------------------------------------
--
