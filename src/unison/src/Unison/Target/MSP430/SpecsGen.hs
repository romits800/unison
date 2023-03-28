{-|
Copyright   :  Copyright (c) 2020, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.MSP430.SpecsGen (module X) where
  import Unison.Target.MSP430.SpecsGen.ReadWriteInfo as X
  import Unison.Target.MSP430.SpecsGen.OperandInfo as X
  import Unison.Target.MSP430.SpecsGen.ReadOp as X
  import Unison.Target.MSP430.SpecsGen.ShowInstance()
  import Unison.Target.MSP430.SpecsGen.AllInstructions as X
  import Unison.Target.MSP430.SpecsGen.Itinerary as X
  import Unison.Target.MSP430.SpecsGen.InstructionType as X
  import Unison.Target.MSP430.SpecsGen.AlignedPairs as X
  import Unison.Target.MSP430.SpecsGen.Parent as X
  import Unison.Target.MSP430.SpecsGen.Size as X
