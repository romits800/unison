{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.Thumb.OperandInfo (operandInfo) where

import Unison
import Unison.Target.Thumb.SpecsGen.ThumbRegisterClassDecl
import Unison.Target.Thumb.SpecsGen.ThumbInstructionDecl
import qualified Unison.Target.Thumb.SpecsGen as SpecsGen

-- | Gives information about the operands of each instruction

operandInfo i
  -- The generated operandInfo is wrong for these instructions
  | i `elem` [TTAILJMPd, TTAILJMPdND] =
    ([BoundInfo, BoundInfo, TemporaryInfo (RegisterClass CCR) 0 False],
     [])
  | otherwise = SpecsGen.operandInfo i
