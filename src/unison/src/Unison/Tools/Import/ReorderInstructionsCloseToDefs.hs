{-|
Copyright   :  Copyright (c) 2022, KTH
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>

-}
module Unison.Tools.Import.ReorderInstructionsCloseToDefs (reorderInstructionsCloseToDefs) where

import Data.List     -- for `sortBy`

{-
procedure:
  1. start from bottom and move everything that can be moved downwards
  2. start from top and move everything that can be moved upwards
-}

import Unison

import Unison.Target.API
import Unison.Target.Query

import qualified Unison.Graphs.DG as DG

import Data.Map (Map)
import qualified Data.Map as Map

--- k: the number of clusters
type AccType = Map Int Integer

reorderInstructionsCloseToDefs:: Show i => Show r => Ord s => Ord r => Ord i =>
                Function i r ->
                TargetWithOptions i r rc s -> Function i r
reorderInstructionsCloseToDefs f @ Function {fCode = code} target =
  let
    (_, bs) = mapAccumL (revReorderBlock target)
                          (Map.empty) code

    (_, bs') = mapAccumL (reorderBlock target)
                          (Map.empty) bs
  in f{ fCode = bs'}


reorderBlock :: Show i => Show r => Ord s => Ord r => Ord i =>
               TargetWithOptions i r rc s -> AccType ->
               Block i r -> (AccType, Block i r)
reorderBlock target acc block @ Block {bCode = code} =
  let
    -- Generate Dependency Graph
    dg            = genDg block target
    -- Number of clusters
    (acc',code')   = reord (acc, []) dg code
  in (acc', block {bCode=code'})-- error $ show (km, km', km'', code', nb)

revReorderBlock :: Show i => Show r => Ord s => Ord r => Ord i =>
               TargetWithOptions i r rc s -> AccType ->
               Block i r -> (AccType, Block i r)
revReorderBlock target acc block @ Block {bCode = code} =
  let
    -- Generate Dependency Graph
    dg            = genDg block target
    -- Number of clusters
    (acc',code')   = reord2 (acc, []) dg (reverse code)
  in (acc', block {bCode= reverse code'})-- error $ show (km, km', km'', code', nb)



genDg :: Show i => Show r => Ord s => Ord r => Ord i =>
          Block i r -> TargetWithOptions i r rc s -> DGraph i r
genDg bl target = 
  let rwlf  = readWriteLatency target
      rm    = resourceManager target
      oif   = operandInfo target
      dg    = DG.fromBlock rwlf rm oif bl
  in dg



reord :: Show i => Show r => 
                   (AccType, [BlockOperation i r]) -> DGraph i r -> 
                   [BlockOperation i r] -> (AccType, [BlockOperation i r])
reord (acc,nops) _ [opout] = (acc, (reverse nops) ++ [opout])
reord (acc,nops) dg (op @ SingleOperation {oId = oid}:ops) =
  let
      hsucs  = DG.sucs dg (fromIntegral oid)
      acc'   = Map.insert (fromIntegral oid) 1 acc
      (acc'', ms) = movSucs (acc',Map.empty) dg hsucs
      (mops, ops') = findOps ms ops ([], [])
  in 
    reord (acc'', op:nops) dg (mops ++ ops')
reord _ _ _ = error "reord: Bundles not supported."


findOps _ [] (mops, ops') = (reverse mops, reverse ops')
findOps ms (op @ SingleOperation {oId = oid}:ops) (mops, ops') = 
    case Map.lookup (fromIntegral oid) ms of
        Nothing  -> findOps ms ops (mops, op:ops')
        Just _   -> findOps ms ops (op:mops, ops')
findOps _ _ _ = error "findOps: Bundles not supported."


reord2 :: Show i => Show r => 
                   (AccType, [BlockOperation i r]) -> DGraph i r -> 
                   [BlockOperation i r] -> (AccType, [BlockOperation i r])
reord2 (acc,nops) _ [opout] = (acc, (reverse nops) ++ [opout])
reord2 (acc,nops) dg (op @ SingleOperation {oId = oid}:ops) =
  let
      hprecs  = DG.precs dg (fromIntegral oid)
      acc'   = Map.insert (fromIntegral oid) 1 acc
      (acc'', ms) = movPrecs (acc',Map.empty) dg hprecs
      (mops, ops') = findOps ms ops ([], [])
  in 
    reord2 (acc'', op:nops) dg (mops ++ ops')
reord2 _ _ _ = error "reord: Bundles not supported."



isInMap i mp = case Map.lookup i mp of
    Nothing -> False
    _ -> True

movSucs acc _ [] = acc
movSucs (mp,ms) dg (h:rest) =
    let      
        hprecs = DG.precs dg h 
      -- if all preds are already scheduled, then move this
    in case all (\i -> isInMap i mp) hprecs  of
       False -> movSucs (mp, ms) dg rest
       True -> 
            let --nmp = Map.insert h 1 mp -- insert to the map
                nms = Map.insert h 1 ms
            in movSucs (mp, nms) dg rest
    

movPrecs acc _ [] = acc
movPrecs (mp,ms) dg (h:rest) =
    let      
        hsucs = DG.sucs dg h 
      -- if all preds are already scheduled, then move this
    in case all (\i -> isInMap i mp) hsucs  of
       False -> movPrecs (mp, ms) dg rest
       True -> 
            let --nmp = Map.insert h 1 mp -- insert to the map
                nms = Map.insert h 1 ms
            in movPrecs (mp, nms) dg rest
 
