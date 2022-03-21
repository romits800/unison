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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.SecAugment (run) where

import System.FilePath
import Control.Monad

import Unison.Base()
import Unison.Driver
import Unison.Parser
import Unison.Tools.Lint (invokeLint)

--import Unison.Transformations.RunTargetTransforms
import Unison.Tools.SecAugment.AddRegisterCopies
import Unison.Transformations.RenameTemps
import Unison.Transformations.RenameOperations
import Unison.Transformations.RenameMOperands
import Unison.Transformations.ReorderInstructions
import Common.Util
import qualified Unison.ParseSecurityPolicies as PSP

run (uniFile, debug, intermediate, lint, lintPragma, altUniFile, policy) uni target =
  do
    secPolicy <- maybeStrictReadFile policy
    let f = parse target uni
        policies =  case secPolicy of
                      (Just pfile) -> PSP.parse pfile
                      Nothing -> []
        (altF, partialAltFs) = applyTransformations
                               (secAugmenterTransformations policies)
                               target f
        baseName = takeBaseName uniFile
    when debug $
      putStr (toPlainText partialAltFs)
    when intermediate $
      mapM_ (writeIntermediateFile "sec.uni" baseName) partialAltFs
    emitOutput altUniFile (show altF)
    when lint $
      invokeLint altF target
      

secAugmenterTransformations policy =
    [(addRegisterCopies policy, "addRegisterCopies", True),
     (renameTemps, "renameTemps", True),
     (renameMOperands, "renameMOperands", True),
     (renameOperations, "renameOperations", True),
     (reorderInstructions, "reorderInstructions", True)
    ]

-- augmentPragmas =
--     [("lint",
--       "--nocostoverflow=false")]

-- augmentPragmaTools = map fst augmentPragmas
