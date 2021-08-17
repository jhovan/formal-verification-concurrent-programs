module TestsConcProg
--
-- Tests concurrent programs, and examples of programs Succ and Collatz from Huth and Ryan
-- Author: Miguel Carrillo Barajas.
--
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
    (impProgToLabImpProg)
import NuSmvSpecs
    (NuSMVmodel, SmvId
    ,idTOsmvId)
import NuSmvToString
    (nuSmvModelTOstring)
import LimpProgToNuXmv 
    (lImpProgToNuXmv)
--
--
import ConcurrentProgSyntax
import ConcLabProgSyntax (LconcImpProg)
import ConcProgToLconcProg
import ConcLabProgToNuXmv (concLabProgToNuXmv)
--
-----------------------------------------------------------------------------------------
--
-- Examples and Tests:
--
--
-----------------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
--
-- Test concurrent programs:
--
-- Some types variables
--
range01 :: ImpVarType
range01 = TimpRange (0,1)
--
range09 :: ImpVarType
range09 = TimpRange (0,9)
--
pc :: SmvId
pc = (idTOsmvId "pc")
--
--
-------------------------------------------------------------------------------
--
-- Programa concurrente de la tarea 4 que usa procesos (pogramas Imp) impProcess1 y impProcess2:
-- concProg1 := cobegin Process1 || Process2 coend.
--
-- De forma auxiliar, definimos dos programas Imp, "Process1" y "Process2",
-- que forman parte de la definición del programa concurrente "concProg1":

impProcess1 :: ImpProg
impProcess1 = Prog ("Process1"
                    ,ProgVarList [("x", range09)]
                    ,CseqCompos (
                        Csimp (Sassign "x" (Earith (Aint 1)))
                        ,Csimp (Sassign "x" (Earith (Aint 2)))
                        ) )

labImpProcess1 :: LimpProg
labImpProcess1 = impProgToLabImpProg 1 (pc++"1") impProcess1

xmvImpProcess1 :: NuSMVmodel
xmvImpProcess1 = lImpProgToNuXmv labImpProcess1

impProcess1TOxmvFile :: IO ()
impProcess1TOxmvFile = writeFile "impProcess1.xmv" (nuSmvModelTOstring xmvImpProcess1)

-- Tests:
-- $ ghci
-- *TestsConcProg> :l TestsConcProg.hs
-- *TestsConcProg> impProcess1TOxmvFile
-- *TestsConcProg> impProcess1
-- *TestsConcProg> labImpProcess1

impProcess2 :: ImpProg
impProcess2 = Prog ("Process2"
                    ,ProgVarList [("y", range09)]
                    ,CseqCompos (
                        Csimp (Sassign "y" (Earith (Aint 1)))
                        ,Csimp (Sassign "y" (Earith (Aint 2)))
                        ) )

labImpProcess2 :: LimpProg
labImpProcess2 = impProgToLabImpProg 1 (pc++"2") impProcess2

xmvImpProcess2 :: NuSMVmodel
xmvImpProcess2 = lImpProgToNuXmv labImpProcess2

impProcess2TOxmvFile :: IO ()
impProcess2TOxmvFile = writeFile "impProcess2.xmv" (nuSmvModelTOstring xmvImpProcess2)

-- Tests:
-- $ ghci
-- *TestsConcProg> :l TestsConcProg.hs
-- *TestsConcProg> impProcess2TOxmvFile
-- *TestsConcProg> impProcess2
-- *TestsConcProg> labImpProcess2

---------------------------------------
-- Programa concurrente de la tarea 4 que usa procesos (pogramas Imp) impProcess1 y impProcess2:
--
concProg1 :: ConcProg (SimpStm ExpAB)
-- An example of concurrent programming (Tarea 4).
-- concProg1 := cobegin Process1 || Process2 coend.
concProg1 =
    ConcProg (
    "concProg1"
    ,[  impProcess1
        ,impProcess2
    ])
--
-- Tests:
-- $ ghci
-- *> :l TestsConcProg.hs
-- *TestsConcProg> concProg1
--
labConcProg1 :: LconcImpProg
labConcProg1 = concProgToLabConcProg 1 pc concProg1
--
-- Tests:
-- $ ghci
-- *> :l TestsConcProg.hs
-- *TestsConcProg> labConcProg1
--
--
xmvConcProg1 :: NuSMVmodel
xmvConcProg1 =  concLabProgToNuXmv labConcProg1
--
concProg1TOxmvFile :: IO ()
concProg1TOxmvFile = writeFile "concProg1.xmv" (nuSmvModelTOstring xmvConcProg1)
--
-- Tests:
-- $ ghci
-- *TestsConcProg> :l TestsConcProg.hs
-- *TestsConcProg> concProg1TOxmvFile
-- *TestsConcProg> 
--
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int fileName.xmv
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "fileName.dot"
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf fileName.dot > fileName.pdf
-- $
--
-------------------------------------------------------------------------------
--
mutualExcCGP :: ConcProg (SimpStm ExpAB)
-- An example of concurrent program to illustrate mutual exclusion of two processes, P0 and P1.
-- from: Clarke, Grumberg, and Peled, Model Checking, 1999, sec 2.3, pp. 24-26.
-- Process P0 has a program counter pc_0, and process P1 has a program counter pc_1.
-- When the value of the program counter of a process Pi is CRi,
-- the process Pi is in its critical region, pc_i=CR_i. 
-- Both processes are not allowed to be in their critical regions at the same time. 
-- When the program counter of Pi is NCi, pc_i=NCi, the process Pi is in its noncritical region. 
-- In this case, pc_i=NCi, process Pi waits until turn=i 
-- in order to gain exclusive entry into the critical region CRi.
mutualExcCGP =
    ConcProg (
    "mutualExcCGP"
    ,[ -- PENDIENTE XXX
--     Prog ("P0"
--         ,ProgVarList [("x", range09)]
--         ,CseqCompos (
--             Csimp (Sassign "x" (Earith (Aint 1)))
--             ,Csimp (Sassign "x" (Earith (Aint 2)))
--             ) )
--     ,Prog ("P1"
--         ,ProgVarList [("y", range09)]
--         ,CseqCompos (
--             Csimp (Sassign "y" (Earith (Aint 1)))
--             ,Csimp (Sassign "y" (Earith (Aint 2)))
--             ) )
    ])
--
-- Tests:
-- $ ghci
-- *> :l TestsConcProg.hs
-- *TestsConcProg> mutualExcCGP
--
-- Use nuXmv to generate a dot file of transitions:
-- $ nuXmv -quiet -int fileName.xmv
-- nuXmv > go
-- nuXmv > print_fair_transitions -v -f dot -o "fileName.dot"
-- nuXmv > quit
-- $ 
-- $ dot -Tpdf fileName.dot > fileName.pdf
-- $
--
