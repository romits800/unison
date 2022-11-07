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
module Unison.Tools.Linearize.KeepDeadInstrs (keepDeadInstrs) where

import Unison.Base
import Unison.Util
import Unison.Constructors


keepDeadInstrs f @ Function {fCode = code} _target =
    let 
        fcode = flatten code
        deads = collectDeads fcode []
        deads' = map (\t -> (t, bLab $ tempBlock code t)) deads
        f' = foldl sinkDef f deads'
    in f'


--- TODO
collectDeads [] acc = reverse acc
collectDeads (opr @ SingleOperation {}:code) acc =
    let
        ts = oDefs opr
        us = map (\t -> users t code) ts
        ds = filter (\(_t,u) -> null u) $ zip ts us
        ds' = map fst ds
    in collectDeads code (ds' ++ acc)
collectDeads (_:code) acc = collectDeads code acc


sinkDef f @ Function {fCode = code, fCongruences = fc} (d, b) = 
    let bs = exitBlockIds code
        nt = mkTemp $ newTempIndex $ flatten code
        code' = foldl (applyToBlock (addToOut [nt])) code bs
        code'' = foldl (applyToBlock (addToIn [nt])) code' bs
        code''' = applyToBlock (addToOut [d]) code'' b
    in f {fCode = code''', fCongruences = (d, nt):fc}

