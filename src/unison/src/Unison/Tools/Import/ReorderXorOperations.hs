{-|
Copyright   :  Copyright (c) 2022, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Tools.Import.ReorderXorOperations (reorderXorOperations)
       where

import Unison.ParseSecurityPolicies
import Unison.Transformations.SecurityTypeInference

import qualified Data.Map as Map
-- import Data.Maybe

import Unison.Base
import Unison.Util
import Unison.Target.API

import Unison.Constructors

reorderXorOperations policies f @ Function {fCode = code} target =
  let
    (pmap, _, _, _, _, _, _, _, _, _) = inferSecurityTypes target f policies
    code' = foldl (reorderXorOperationsBl target pmap) [] code
  in f {fCode = code'}

reorderXorOperationsBl target pmap accCode (b @ Block {bCode = code}) =
  let code' = reorderXorOperationsOp target pmap code [] 
  in accCode ++ [b {bCode = code'}]


reorderXorOperationsOp _ _ [] accCode = foldl (\acc x -> x : acc) [] accCode
reorderXorOperationsOp target pmap
  (SingleOperation {
      oId = oid1,
      oOpr = Natural {
          oNatural = Linear {
              oIs = [i],
              oUs = (t1 @ Temporary {tId = tid1}):
                    (t2 @ Temporary {tId = tid2}):roUs,
              oDs = [(t3 @ Temporary {tId = tid3}),
                     (t4 @ Temporary {})]}}}:
   o2:
   SingleOperation {
      oId = oid2,
      oOpr = Natural {
          oNatural = Linear {
              oIs = [i'],
              oUs = (t1' @ Temporary {tId = tid1'}):
                    (t2' @ Temporary {tId = tid2'}):roUs',
              oDs = [(t3' @ Temporary {}),
                     (t4' @ Temporary {})]}}}:rest)
  accCode | isXor target i && isXor target i'
                                  && (not $ isRandom tid1 pmap)
                                  && (not $ isRandom tid2 pmap)
                                  && (isRandom tid1' pmap
                                      || isRandom tid2' pmap)
                                  && (isSecret tid1 pmap
                                      || isSecret tid2 pmap)
                                  && (tid3 == tid1' || tid3 == tid2') = 
  -- one destination is equal to on of the source of the second
  -- None of the sources of the first is random
  -- One of the destination sources is random
  let
    -- Random
    (tr,tnr) = if isRandom tid1' pmap then (t1',t2') else (t2',t1')
    -- Secret
    (ts,tns) = if isSecret tid1 pmap then (t1,t2) else (t2,t1)
    o1' = mkLinear oid1 [i] (ts:tr:roUs) [t3,t4]
    o3' = mkLinear oid2 [i] (tnr:tns:roUs') [t3',t4']
  in reorderXorOperationsOp target pmap rest (o3':o2:o1':accCode)
reorderXorOperationsOp target pmap (o1:code) accCode =
  -- error $ show $ Map.toList pmap
  reorderXorOperationsOp target pmap code (o1:accCode)

updateIndexes (oid, pid, tid) code =
  let oid' = max oid (newOprIndex code)
      pid' = max pid (newOperIndex code)
      tid' = max tid (newTempIndex code)
  in (oid', pid', tid')


isRandom t pmap =
  case Map.lookup ('t':(show t)) pmap of
       Nothing -> error "This should not be nothing"
       Just (Random _) -> True
       Just _ -> False


isSecret t pmap =
  case Map.lookup ('t':(show t)) pmap of
       Nothing -> error "This should not be nothing"
       Just (Secret _) -> True
       Just _ -> False
