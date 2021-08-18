{-----------------------------------------------------------------
 - >> Author: Johann Gordillo
 - >> Email:  jgordillo@ciencias.unam.mx
 - >> Date:   08/17/2021
 -----------------------------------------------------------------
 - Universidad Nacional Autónoma de México
 - Facultad de Ciencias
 -
 - Seminario de Verificación Formal [2021-2]
 -
 - Tarea 4
 -----------------------------------------------------------------}

module ConcProg1 where

import GlobalTypes
    (EntryLabel(..), ExitLabel(..))
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

-- Rango de 0 a 9.
range09 :: ImpVarType
range09 = TimpRange (0, 9)
     
-- Program Counter.
pc :: SmvId
pc = (idTOsmvId "pc")

-- Implementación del siguiente programa concurrente:
-- cobegin {x := 1; x := 2} || {y := 1; y := 2} coend
-- Donde {...} es un bloque de instrucciones secuenciales.
concProg1 =
    ConcProg ("concProg1"
        ,[
            Prog ("thread1"
                ,ProgVarList [("x", range09)]
                ,CseqCompos (
                    Csimp (Sassign "x" (Earith (Aint 1)))
                    ,Csimp (Sassign "x" (Earith (Aint 2)))
                )
            )
            ,Prog ("thread2"
                ,ProgVarList [("y", range09)]
                ,CseqCompos (
                    Csimp (Sassign "y" (Earith (Aint 1)))
                    ,Csimp (Sassign "y" (Earith (Aint 2)))
                )
            )
        ]
    )

labConcProg1 :: LconcImpProg
labConcProg1 = concProgToLabConcProg 1 pc concProg1

xmvConcProg1 :: NuSMVmodel
xmvConcProg1 =  concLabProgToNuXmv labConcProg1
--
concProg1TOxmvFile :: IO ()
concProg1TOxmvFile = writeFile "concProg1-johann.xmv" (nuSmvModelTOstring xmvConcProg1)