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
module Unison.Target.MSP430.MSP430RegisterDecl (MSP430Register (..)) where

data MSP430Register =
    R0 |
    R1 |
    R2 |
    R3 |
    R4 |
    R5 |
    R6 |
    R7 |
    R8 |
    R9 |
    R10|
    R11|
    R12|
    R13|
    R14|
    R15|
    SR |
    SP 
    deriving (Eq, Ord)
