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
module Unison.Transformations.FinalizeOperations
       (finalizeOperations) where

import Data.List

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Constructors
import Unison.Target.API

finalizeOperations f @ Function {fCode = bcode} target =
    let iNop          = nop target
        id            = newId (linearizeCode bcode)
        bcode'        = map removeBarrierBundles bcode
        bcode''       = map removeVirtualBundles bcode'
        (_, bcode''') = mapAccumL (fillEmptyBundles iNop) id bcode''
    in f {fCode = bcode'''}


{-
removeEmptyBundles b @ Block {bCode = code} =
    let code' = filter (not . isEmptyBundle) code
    in b {bCode = code'}

isEmptyBundle (Bundle []) = True
isEmptyBundle _ = False
-}

removeVirtualBundles b @ Block {bCode = code} =
    let code'  = filter (not . isDelimiterBundle) code
        -- Romy: No nops for kills
        --code''  = filter (not . isVirtualBundle) code'
        code''' = concatMap filterNonVirtual code'
    in b {bCode = code'''}

isVirtualBundle (Bundle is) | all isVirtual is = True
isVirtualBundle _ = False

isDelimiterBundle (Bundle is) | any isDelimiter is && all isVirtual is = True
isDelimiterBundle _ = False

filterNonVirtual (Bundle is) = [Bundle (filter (not . isVirtual) is)]
filterNonVirtual o @ SingleOperation {} = filter (not . isVirtual) [o]

removeBarrierBundles b @ Block {bCode = code} =
  let code' = filter (not . isBarrierBundle) code
  in b {bCode = code'}

isBarrierBundle (Bundle is) | any isFun is && all isVirtual is = True
isBarrierBundle _ = False

fillEmptyBundles iNop id b @ Block {bCode = code} =
    let (id', code') = mapAccumL (fillEmptyBundle iNop) id code
    in (id', b {bCode = code'})

fillEmptyBundle iNop id (Bundle []) =
    (id + 1, Bundle [mkSingleOperation id iNop])
fillEmptyBundle _ id b = (id, b)
