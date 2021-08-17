module TestsImpProg
-----------------------------------------------------------------------------------------
-- Tests for trasfroming programs of the Imp Language to models of nuXmv
-- Author: Miguel Carrillo Barajas.
--
-----------------------------------------------------------------------------------------
where
--import Data.List (nub, union)
--import qualified Data.Set as S (fromList,Set) --,(\\),toList,filter
--
-----------------------------------------------------------------------------------------
--import GlobalTypes (VarId) -- Value(..), Context
--import GlobalFunctions (replaceInAssocL)
import ImpExpSyntax 
    (ExpAB(..),ArthE(..),BoolE(..) )
import ImpSimpStmSyntax
    (SimpStm(..) )
import ImpCompStmSyntax
    (CompStm(..) )
import ImpProgSyntax
    (Prog(..),ImpVarType(..),ProgVarList(..),ImpProg)
import ImpLabProgSyntax
    (LimpProg)
import ImpProgToLimpProg
    (impProgToLabImpProg,commToLComm)
import NuSmvSpecs
    (NuSMVmodel,SmvId
    ,idTOsmvId)
import NuSmvToString
    (nextExprTOstr,nuSmvModelTOstring)
import LimpCompStmToNuXmv
    (impCompStmToNextExpr)
import LimpProgToNuXmv 
    (lImpProgToNuXmv)
--
-----------------------------------------------------------------------------------------
--
--import ConcurrentProgSyntax
-- For Tests of concurrent programs, see TestsConcProg.hs.
--
-----------------------------------------------------------------------------------------
--
-- Examples and Tests:
--
ejemP0 :: ImpProg
-- 
ejemP0 = 
    Prog ("P0"
        ,ProgVarList [] -- Var
        ,(Csimp Sskip)  -- skip;
        )
--
lEjemP0 :: LimpProg
lEjemP0 = impProgToLabImpProg 1 pc ejemP0

xmvEjemP0 :: NuSMVmodel
xmvEjemP0 = lImpProgToNuXmv lEjemP0
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP0
-- *TestsImpProg> lEjemP0
-- *TestsImpProg> writeFile "ejemP0.txt" $ show ejemP0
-- *TestsImpProg> writeFile "labEjemP0.txt" $ show (impProgToLabImpProg 1 ejemP0)
-- *TestsImpProg> :!cat ejemP0.txt
-- *TestsImpProg> :!cat labEjemP0.txt
-- *TestsImpProg> writeFile "ejemP0.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv lEjemP0)
-- *TestsImpProg> :!cat ejemP0.xmv
-- Use nuXmv to generate a dot file of transitions:
-- ejecutar nuXm en modo interactivo sobre el archivo xmv:
-- $ nuXmv -quiet -int ejemP0.xmv 
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "ejemP0.dot"
-- ...
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf ejemP0.dot > ejemP0.pdf
-- $
--

--
range09 :: ImpVarType
range09 = TimpRange (0,9)

ejemP1 :: ImpProg
ejemP1 = 
    Prog ("P1" 
        ,ProgVarList            -- Var  
            [("x",range09)      -- x: 0..9
            ,("y",range09)      -- y: 0..9
            ,("z",range09) ]    -- z: 0..9
        ,(Csimp (Sassign "x" (Earith (Aint 3))))     -- x:= 3;
        )
--
lEjemP1 :: LimpProg
lEjemP1 = impProgToLabImpProg 1 pc ejemP1
--
writeP1 :: IO ()
writeP1 = writeFile "ejemP1.txt" $ show ejemP1 
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP1
-- *TestsImpProg> lEjemP1
-- *TestsImpProg> writeFile "ejemP1.txt" (show ejemP1)
-- *TestsImpProg> :!cat ejemP1.txt
-- *TestsImpProg> writeFile "labEjemP1.txt" $ showLImpProg (impProgToLabImpProg 1 ejemP1)
-- *TestsImpProg> writeFile "labEjemP1.txt" $ show (impProgToLabImpProg 1 ejemP1)
-- *TestsImpProg> :!cat labEjemP1.txt
--

lEjemP1ToNextExpr_a :: String
-- Ejemplo, labeledStatement --> nuXmv, convertir: 
--      la instruccion "x:=0", etiquetada como "(1, x:=0, 2)",
--      a una expresion "next" de NuXmv.
-- Recordando, impCompStmToNextExpr :: SmvId->[SmvId]->LimpStm -> NextExpr
lEjemP1ToNextExpr_a = nextExprTOstr indent nextExpOfStm1
    where
    compStm1        = Csimp (Sassign "x" (Earith (Aint 0))) -- x:= 0;
    labCompStm1     = commToLComm 1 compStm1                -- (1, x:=0, 2)
    nextExpOfStm1   = impCompStmToNextExpr (idTOsmvId "pc") [idTOsmvId "x"] labCompStm1
    indent          = 2
--
ejemP2 :: ImpProg
ejemP2 = 
    Prog ("P2"
        ,ProgVarList            -- Var  
            [("x",range09)      -- x: 0..9
            ,("y",range09)      -- y: 0..9
            ,("z",range09) ]    -- z: 0..9
        ,CseqStmBlock [                                  -- Begin
            (Csimp Sskip)                               -- skip;
            ,(Csimp (Sassign "x" (Earith (Aint 3))))    -- x:= 3;
            ]                                           -- End
        )
--
lEjemP2 :: LimpProg
lEjemP2 = impProgToLabImpProg 1 pc ejemP2
--
writeP2 :: IO ()
writeP2 = writeFile "ejemP2.txt" $ show ejemP2
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP2
-- *TestsImpProg> lEjemP2
-- *TestsImpProg> writeFile "ejemP2.txt" $ show ejemP2
-- *TestsImpProg> writeFile "labEjemP2.txt" $ showLImpProg (impProgToLabImpProg 1 ejemP2)
-- *TestsImpProg> writeFile "labEjemP2.txt" $ show (impProgToLabImpProg 1 ejemP2)
-- *TestsImpProg> :!cat ejemP2.txt
-- *TestsImpProg> :!cat labEjemP2.txt
--
ejemP3 :: ImpProg
ejemP3 = 
    Prog ("P3"
        ,ProgVarList            -- Var  
            [("x",range09)      -- x: 0..9
            ,("y",range09)      -- y: 0..9
            ,("z",range09) ]    -- z: 0..9
        ,CseqStmBlock [                                 -- Begin
             Csimp (Sassign "x" (Earith (Aint 0)))      -- x:= 0;
            ,Csimp (Sassign "y" (Earith (Aint 1)))      -- y:= 1;
            ,Csimp (Sassign "z" (Earith (Avar "x")))    -- z:= x;
            ,Csimp (Sassign "x" (Earith (Avar "y")))    -- x:= y;
            ,Csimp (Sassign "y" (Earith (Avar "z")))    -- y:= z;
            ])                                          -- End
--
lEjemP3 :: LimpProg
lEjemP3 = impProgToLabImpProg 1 pc ejemP3
--
writeP3 :: IO ()
writeP3 = writeFile "ejemP3.txt" $ show ejemP3
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP3
-- *TestsImpProg> lEjemP3
-- *TestsImpProg> writeFile "ejemP3.txt" $ show ejemP3
-- *TestsImpProg> writeFile "labEjemP3.txt" $ showLImpProg (impProgToLabImpProg 1 ejemP3)
-- *TestsImpProg> :!cat ejemP3.txt
-- *TestsImpProg> :!cat labEjemP3.txt
--
ejemP4 :: ImpProg
ejemP4 = 
    Prog ("P4"
        ,ProgVarList            -- Var  
            [("x",range09)      -- x: 0..9
            ,("y",range09)      -- y: 0..9
            ,("z",range09) ]    -- z: 0..9
        ,CseqStmBlock [                                     -- Begin
            Csimp (Sassign "x" (Earith (Aint 0)))           -- x:= 0;
            ,Csimp (Sassign "y" (Earith (Aint 1)))          -- y:= 1;
            ,CifThenElse (Bequ (Avar "x") (Avar "y"))       -- if x==y
                (Csimp (Sassign "z" (Earith (Avar "x"))))   --    then {z:= x};
                (Csimp (Sassign "z" (Earith (Avar "y"))))   --    else {z:= y};
            ])                                              -- End
--
lEjemP4 :: LimpProg
lEjemP4 = impProgToLabImpProg 1 pc ejemP4
--
writeP4 :: IO ()
writeP4 = writeFile "ejemP4.txt" $ show ejemP4
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP4
-- *TestsImpProg> lEjemP4
-- *TestsImpProg> writeFile "ejemP4.txt" $ show ejemP4
-- *TestsImpProg> writeFile "labEjemP4.txt" $ show (impProgToLabImpProg 1 ejemP4)
-- *TestsImpProg> :!cat ejemP4.txt
-- *TestsImpProg> :!cat labEjemP4.txt
--
ejemP5 :: ImpProg
ejemP5 = 
    Prog ("P5"                                                          -- Program name
    ,ProgVarList                                                        -- Var  
        [("x",range09)]                                                 -- x: 0..9
    ,CseqStmBlock                                                       -- % Block of statements:
        [                                                               -- Begin
        Csimp (Sassign "x" (Earith (Aint 0)))                           -- x:= 0;
        ,Cwhile (Bleq (Avar "x") (Aint 3))                              -- While x <= 3 do 
            (Csimp (Sassign "x" (Earith (Asum (Avar "x") (Aint 1)))))   -- {x:= x+1}
        ])                                                              -- End
--
lEjemP5 :: LimpProg
lEjemP5 = impProgToLabImpProg 1 pc ejemP5
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP5
-- *TestsImpProg> lEjemP5
-- *TestsImpProg> writeFile "ejemP5.txt" $ show ejemP5
-- *TestsImpProg> writeFile "labEjemP5.txt" $ show (impProgToLabImpProg 1 ejemP5)
-- *TestsImpProg> :!cat ejemP5.txt
-- *TestsImpProg> :!cat labEjemP5.txt
--
-----------------------------------------------------------------------------------------
--
-- Test lImpProgToNuXmv:
--
pc :: SmvId
pc = (idTOsmvId "pc")
--
xmvModelEjemP5 :: NuSMVmodel
xmvModelEjemP5 = lImpProgToNuXmv lEjemP5
--
xmvEjemP5 :: String
xmvEjemP5 = nuSmvModelTOstring (lImpProgToNuXmv lEjemP5)
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP5
-- *TestsImpProg> writeFile "labEjemP5.txt" $ show (impProgToLabImpProg 1 ejemP5)
-- *TestsImpProg> :!cat labEjemP5.txt
-- *TestsImpProg> writeFile "EjemP5.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv lEjemP5)
-- *TestsImpProg> :!cat EjemP5.xmv
--
-----------------------------------------------------------------------------------------
-- Ejemplo de las notas del inicio del curso:
--
-- 2 Un ejemplo
-- Un ejemplo sencillo de lo que podemos hacer en el curso.
-- 2.1 Pseudo-código:
--         VAR
--         x, y, z: 0..1; % el dominio de x, y es {0,1}.
--         % inicializar:
--         x:= 0; % i1
--         y:= 1; % i2
--         z:= 0; % i3
--         % intercambiar valores de x ,y
--         z:= x; % i4
--         x:= y; % i5
--         y:= z; % i6
-- Pensando que intuitivamente la ejecución de un programa transforma estados
-- del programa (sistema).
-- Los estados consisten de tuplas de valores de las variables):
-- EstadosDelPrograma := {(b1, b2, b3) | b1, b2, b3 ∈ {0, 1}}
-- Pensando que, en la tupla (b1, b2, b3), b1 corresponde a un valor de x.
-- Pensando que, en la tupla (b1, b2, b3), b2 corresponde a un valor de y.
-- Pensando que, en la tupla (b1, b2, b3), b3 corresponde a un valor de z.
-- Estados iniciales: todos, los 8 posibles estados.
--
range01 :: ImpVarType
range01 = TimpRange (0,1)

ejemP6 :: ImpProg
-- Representacion interna (en Haskell) del programa:
--                 Program P6:
--                 Var 
--                     x: 0..1;
--                     y: 0..1;
--                     z: 0..1;
--                 Begin 
--                     x := 0;
--                     y := 1;
--                     z := 0;
--                     z := x;
--                     x := y;
--                     y := z;
--                 End;
ejemP6 =                                            -- Un programa tiene tres componentes:
    Prog ("P6"                                      -- 1er componente: Nombre del programa
        ,ProgVarList            -- Var              -- 2do componente: Declaracion de variables.
            [("x",range01)      -- x: 0..1          
            ,("y",range01)      -- y: 0..1
            ,("z",range01) ]    -- z: 0..1
        -- Para un programa con sintaxis del lenguaje IMP:
        ,CseqStmBlock [                                 -- Begin % 3er componente: Una instruccion compuesta.
            Csimp (Sassign "x" (Earith (Aint 0)))       -- x:= 0; % Earith: Expresion (arit o bool) del lenguaje IMP
            ,Csimp (Sassign "y" (Earith (Aint 1)))      -- y:= 1;
            ,Csimp (Sassign "z" (Earith (Aint 0)))      -- z:= 0; % Sassign: instruccion de asignacion "S": Simple
            ,Csimp (Sassign "z" (Earith (Avar "x")))    -- z:= x;
            ,Csimp (Sassign "x" (Earith (Avar "y")))    -- x:= y;
            ,Csimp (Sassign "y" (Earith (Avar "z")))    -- y:= z;
            ])                                          -- End
--
lEjemP6 :: LimpProg
lEjemP6 = impProgToLabImpProg 1 pc ejemP6  -- programa Imp a programa etiquetado Imp 
--
writeP6 :: IO ()
writeP6 = writeFile "ejemP6.txt" $ show ejemP6
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP6
-- *TestsImpProg> lEjemP6
-- *TestsImpProg> writeFile "ejemP6.txt" $ show ejemP6
-- *TestsImpProg> writeFile "labEjemP6.txt" $ show (impProgToLabImpProg 1 ejemP6)
-- *TestsImpProg> :!cat ejemP6.txt
-- *TestsImpProg> :!cat labEjemP6.txt
-- *TestsImpProg> writeFile "ejemP6.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv lEjemP6)
-- *TestsImpProg> :!cat ejemP6.xmv
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int ejemP6.xmv 
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "ejemP6.dot"
-- Fair transitions: 20 (2^4.32193) out of 64 (2^6)
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf ejemP6.dot > ejemP6.pdf
-- $
--

ejemP7 :: ImpProg
-- Test para la instruccion de lectura de Imp, Sread.
ejemP7 = 
    Prog ("P7" 
        ,ProgVarList                -- Var  
            [ ("x",range01)          -- x: 0..1
             ,("b",TimpBoolean) ]   -- b: boolean
        ,CseqStmBlock [             -- Begin % 3er componente: Una instruccion compuesta.
             Csimp (Sassign "x" (Earith (Aint 0)))  -- x:= 0; 
            ,Csimp (Sassign "b" (Ebool BF))         -- b:= False; 
            ,Csimp (Sread "b" "x" )]                -- x <- read(x), b:= lecturaExitosa?;
        )
--
writeP7 :: IO ()
writeP7 = writeFile "ejemP7.txt" $ show ejemP7 
--
lEjemP7 :: LimpProg
lEjemP7 = impProgToLabImpProg 1 pc ejemP7
--
xmvEjemP7 :: String
xmvEjemP7 = nuSmvModelTOstring (lImpProgToNuXmv lEjemP7)
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemP7
-- *TestsImpProg> lEjemP7
-- *TestsImpProg> writeFile "ejemP7.txt" (show ejemP7)
-- *TestsImpProg> :!cat ejemP7.txt
-- *TestsImpProg> writeFile "labEjemP7.txt" $ show lEjemP7

-- *TestsImpProg> :!cat labEjemP7.txt
-- *TestsImpProg> writeFile "ejemP7.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv lEjemP7)
-- *TestsImpProg> :!cat ejemP7.xmv
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int ejemP7.xmv 
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "ejemP7.dot"
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf ejemP7.dot > ejemP7.pdf
-- $
--
--
-- Ejemplo con asignación doble.
ejemPdobleAsg :: ImpProg
ejemPdobleAsg = 
    Prog ("ejemPdobleAsg"                               -- Program name
    ,ProgVarList                                        -- Var  
        [("x", range01)                                 -- x: 0..1
        ,("y", range01)]                                -- y: 0..1
    ,(Csimp (SdobleAsgn ("x","y") 
            ((Earith (Avar "y")),(Earith (Avar "x"))) ))-- (x,y) := (y,x)
    )                                                   -- End
--
-- *TestsImpProg> writeFile "ejemPdobleAsg.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv (impProgToLabImpProg 1 pc ejemPdobleAsg))
-- *TestsImpProg> :!cat ejemPdobleAsg.xmv
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int ejemPdobleAsg.xmv 
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "ejemPdobleAsg.dot"
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf ejemPdobleAsg.dot > ejemPdobleAsg.pdf
-- $

ejemSuccHR :: ImpProg
-- Ejemplo programa Succ de x, descrito por Huth y Ryan, "Logic in computer science".
-- Examples 4.13 Huth-Ryan p. 277
-- 1. We show that $\vdash_{par} {y = 5} x := y + 1 {x = 6}$ is valid
ejemSuccHR = -- Ejemplo Succ de Huth y Ryan
    Prog ("ejemSuccHR"                              -- Program name
    ,ProgVarList                                    -- Var  
        [("x", range01)                             -- x: 0..1
        ,("y", range01)]                            -- y: 0..1
    ,Csimp (Sassign "x"                             -- x:= 
           (Earith (Asum (Avar "y") (Aint 1))))     --     y + 1;
    )                                               -- End
--
-- LTLSPEC                    -- En todas las trayetorias que comienzan en un edo inicial:
-- (P6.y=5)                   -- Si y=5
--     -> (                            -- entonces
--         F                           -- en algun estado futuro:
--                 (P6.pc=0     -- final del programa
--                  & P6.x=6)   -- x=6
--         )

ejemPcollatzHR :: ImpProg
-- Ejemplo programa Collatz de x, de Huth y Ryan "Logic in computer science".
-- Examples 4.13 Huth-Ryan p. 295
-- Program Collatz, the challenge of ﬁnding suitable termination variants E:
-- Def[p.293]. A variant is an expression e such that: 
--     the value of e is decremented every time the body of the while-stm is executed. 
--     and when the value of e is 0, the while-statement terminates.
--     Example: for the program Fac1 of Example 4.2, a suitable variant is x − z. 
-- p. 295: Program Collatz
--         c = x;
--         while (c != 1) {
--         if (c % 2 == 0) { c = c / 2; }
--         else { c = 3*c + 1; }
--         }
ejemPcollatzHR = -- Ejemplo programa Collatz de Huth y Ryan, p.295
    Prog ("ejemPcollatzHR"                           -- Program name
    ,ProgVarList                                    -- Var  
        [("x", range01)                             -- x: 0..1
        ,("c", range01)]                            -- c: 0..1
    ,CseqStmBlock                                   -- Program statement(s)
        [Csimp (Sassign "c" (Earith (Avar "x")))    -- c:= x;
        ,Cwhile                                     -- while
            (Bnot (Bequ c (Aint 1)))                --      (not c=1) do:
            (CifThenElse                            -- if
                ((c `Amod` (Aint 2))                --  (c mod 2 
                    `Bequ` (Aint 0))                --   == 0)
                (Csimp                              -- then 
                    (Sassign "c" (Earith            --  c := 
                        (c                          --      c
                         `Adiv` (Aint 2)) )))       --      div 2;
                (Csimp                              -- else 
                    (Sassign "c" (Earith            --  c := 
                        ((  (Aint 3) `Aprod` c)     --      3 * c 
                            `Asum` (Aint 1))))) )   --      + 1; 
        ]
    )                                               -- End
    where c = (Avar "c")
--
writePcollatzHR :: IO ()
writePcollatzHR = writeFile "ejemPcollatzHR.txt" $ show ejemPcollatzHR 
--
lEjemPcollatzHR :: LimpProg
lEjemPcollatzHR = impProgToLabImpProg 1 pc ejemPcollatzHR
--
xmvEjemPcollatzHR :: String
xmvEjemPcollatzHR = nuSmvModelTOstring (lImpProgToNuXmv lEjemPcollatzHR)
--
-- Tests:
-- ghci> :l TestsImpProg.hs
-- *TestsImpProg> ejemPcollatzHR
-- *TestsImpProg> lEjemPcollatzHR
-- *TestsImpProg> writeFile "ejemPcollatzHR.txt" (show ejemPcollatzHR)
-- *TestsImpProg> :!cat ejemPcollatzHR.txt
-- *TestsImpProg> writeFile "labEjemPcollatzHR.txt" $ show lEjemPcollatzHR

-- *TestsImpProg> :!cat labEjemPcollatzHR.txt
-- *TestsImpProg> writeFile "ejemPcollatzHR.xmv" $ nuSmvModelTOstring (lImpProgToNuXmv lEjemPcollatzHR)
-- *TestsImpProg> :!cat ejemPcollatzHR.xmv
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int ejemPcollatzHR.xmv 
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "ejemPcollatzHR.dot"
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf ejemPcollatzHR.dot > ejemPcollatzHR.pdf
-- $
--
--

-----------------------------------------------------------------------------------------
--
-- For Tests of concurrent programs, see TestsConcProg.hs.
--
-----------------------------------------------------------------------------------------
--
--
