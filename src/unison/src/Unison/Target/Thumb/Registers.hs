{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.Thumb.Registers
    (registerArray, registerAtoms, regClasses, registers,
     subRegIndexType, infRegClassUsage, infRegClassBound,
     reserved, callerSaved, calleeSaved, hardwareRegisters) where

import Data.List
import qualified Data.Map as M

import Unison
import Unison.Target.Thumb.ThumbRegisterDecl
import Unison.Target.Thumb.SpecsGen.ThumbRegisterClassDecl
--import Unison.Target.Thumb.Common

-- | Register array

registerArray  =
            [RegisterClass GPR, RegisterClass CCR, InfiniteRegisterClass M32,
            InfiniteRegisterClass RM32]

-- | Register atoms of 1-width registers

registerAtoms ra | ra `elem` concatMap (registers . RegisterClass)
          [GPR, RGPR, GPRnopc, CCR, TcGPR, GPRsp, TGPR] = (ra, ra)
registerAtoms R0_3   = (R0, R3)
registerAtoms R4_7   = (R4, R7)
registerAtoms R8_11   = (R8, R11)
registerAtoms r = error ("unmatched: registerAtoms (cortex-M0) " ++ show r)

-- | Register classes
regClasses =
    map RegisterClass
    [GPR, RGPR, GPRnopc, TcGPR, GPRsp, CCR, TGPR, ALL, CS, CSL, CSH, CRS ] ++
    map InfiniteRegisterClass [M32, M32t, M64, M128, M512, RM32, RM64]



-- | Individual registers of each register class
registers (RegisterClass GPR) =
    [R0, R1, R2, R3, R4, R5, R6, R7, R8,
     R9, R10, R11, R12, SP, LR, PC]

registers (RegisterClass TGPR) =
    [R0, R1, R2, R3, R4, R5, R6, R7]

registers (RegisterClass RGPR) =
  registers (RegisterClass GPR) \\ [PC, SP]

registers (RegisterClass GPRnopc) =
  registers (RegisterClass GPR) \\ [PC]

registers (RegisterClass TcGPR) = [R0, R1, R2, R3, R12]

registers (RegisterClass GPRsp) = [SP]

registers (RegisterClass ALL) =
  registers (RegisterClass GPR)

registers (RegisterClass CS) = [R4_7, R8_11]

registers (RegisterClass CSL) = [R4_7]

registers (RegisterClass CSH) = [R8_11]

registers (RegisterClass CRS) = [R0_3]

registers (RegisterClass CCR) = [CPSR]

registers rc = error ("unmatched: registersCM0 " ++ show rc)


-- | Index type (low/high/copy) of subregisters

subRegIndexType _ subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass rc)
  | rc == M32  = 1
  | rc == M32t = 1
  | rc == M64  = 2
  | rc == M128 = 4
  | rc == M512 = 16
  | rc == RM32 = 1
  | rc == RM64 = 2

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound (InfiniteRegisterClass rc)
  | rc == M32t = Just 32
  | otherwise = Nothing

-- | Registers whose value cannot be moved around

reserved = [SP, LR, PC]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
callerSaved = [R0_3, R12]


-- | Registers that are preserved across calls
calleeSaved = [R4_7, R8_11]

instance Read ThumbRegister where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show ThumbRegister where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show ThumbRegister"

regStrings = M.fromList $
  [(R0, "r0"),
   (R1, "r1"),
   (R2, "r2"),
   (R3, "r3"),
   (R4, "r4"),
   (R5, "r5"),
   (R6, "r6"),
   (R7, "r7"),
   (R8, "r8"),
   (R9, "r9"),
   (R10, "r10"),
   (R11, "r11"),
   (R12, "r12"),
   (SP, "sp"),
   (LR, "lr"),
   (PC, "pc")] ++
--  regStringsWithIndex "s" SPR ++
--  regStringsWithIndex "d" DPR ++
  [(R0_3, "r0_3"),
   (R4_7, "r4_7"),
   (R8_11, "r8_11")] ++
 -- [(D0_7, "d0_7"),
 --  (D8_15, "d8_15")] ++
  [(CPSR, "cpsr")]  ++
  [--(FPSCR_NZCV, "fpscr_nzcv"),
   (ITSTATE, "itstate"),
   (PRED, "pred")
   --(FPSCR, "fpscr")
   ] -- ++
--  regStringsWithIndex "q" QPR ++
 -- [(F0, "f0")]

--regStringsWithIndex pre rc =
--  [(r, pre ++ show idx) | (r, idx) <- zip (registers  (RegisterClass rc)) [0..]]

hardwareRegisters = [R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12]
