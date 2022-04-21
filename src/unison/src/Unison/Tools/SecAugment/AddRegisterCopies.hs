{-|
Copyright   :  Copyright (c) 2022, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Tools.SecAugment.AddRegisterCopies (addRegisterCopies)
       where

import Unison.ParseSecurityPolicies
import Unison.Transformations.SecurityTypeInference

import qualified Data.Map as Map
-- import Data.Maybe

import Unison.Base
import Unison.Util
import Unison.Target.API

addRegisterCopies policies f @ Function {fCode = code} target =
  let
    (pmap, _, _, _, _, _, _, _, _, _, _) = inferSecurityTypes target f policies
    escf = addSecurityCopy target
    rands = map getT $ filter isRandomT $ map snd $ Map.toList pmap
    f' = foldl escf f rands
  in f'


isRandomT :: Policy String -> Bool
isRandomT (Random ('t':_)) = True
isRandomT _ = False

getT :: Policy String -> Integer
getT (Random ('t':t)) = read t
getT _ = error "getT: This should not be possible."
