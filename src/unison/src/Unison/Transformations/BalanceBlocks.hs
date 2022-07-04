{-|
Copyright   :  Copyright (c) 2022, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Transformations.BalanceBlocks (balanceBlocks)
       where

-- import Unison.ParseSecurityPolicies
import Unison.Transformations.SecurityTypeInference

import qualified Data.Map as Map

import Unison.Base
import Unison.Util
import Unison.Target.API
import Unison.Constructors
import Unison.Predicates

balanceBlocks policies f @ Function {fCode = _} target =
  let
    types = inferSecurityTypes target f policies
    bbs = Map.toList $ fBbs types
    bbsToBalance = filter (\(_,p) -> [] `elem` p) bbs
    bbsToBalance' = map fst bbsToBalance
    f' = foldl (balanceOneBlock target) f bbsToBalance' 
  in f'


balanceOneBlock target f @ Function {fCode = code} bid =
  let
    bblock = findBlock code bid 
    f'   = addBalancingBlock target bblock f
  in f'


-- getMinimumPath [] min = min
-- getMinimumPath (p:rest) [] = getMinimumPath rest p
-- getMinimumPath ([]:rest) min = getMinimumPath rest min
-- getMinimumPath (p:rest) min | length min > length p = getMinimumPath rest p
-- getMinimumPath (p:rest) min | otherwise = getMinimumPath rest min


mkNewBlock :: Frequency -> BlockId -> [BlockOperation i r] -> Block i r
mkNewBlock freq bid blcode = mkBlock bid (mkBlockAttributes False False False
                                        (Just freq) False Nothing True) blcode

mkBranchInstruction oid bid target = branchInstruction target bid oid

-- bblock refers to the block before - That block should jump to the new block and
--                                     the new block should jump to the destination of
--                                     the previous block
addBalancingBlock target bblock @ Block {bLab = obid, bCode = bcode} f @ Function {fCode = code} =
  let bid  = newBlockIndex code -- new index for block
      freq = blockFreq bblock
      oid  = newId code
      label = getBranch bcode
      bcode' = replaceBranch bcode bid []
      code' = replaceBlock code (bblock {bCode = bcode'}) []
      -- bout = blockOut bcode'
      oin = mkIn oid []
      o    = mkBranchInstruction (oid+1) label target
      oout = mkOut (oid+2) []
      o2   = mkBranchInstruction (oid+3) label target
      nbl  = mkNewBlock freq bid [oin, o, oout]
      code'' = insertBlock code' nbl (label-1) o2 []
      code''' = mapToOperationInBlocks (applyToPhiOps obid bid) code''
  in f {fCode = code'''}


getBranch [] = error "getBranch: There should be a branch here."
getBranch (SingleOperation {oId = _oid,
                            oOpr = Natural { oNatural = Branch { oBranchUs = ops }}
                           } : _) | hasLabel ops = getLabel ops
getBranch (_ : rest) = getBranch rest


replaceBranch [] _bid acc = reverse acc
replaceBranch (obr @
               SingleOperation {oId = _oid,
                                oOpr = Natural {oNatural = br @ Branch { oBranchUs = ops }}
                               } : rest) bid acc
  | hasLabel ops =
      let newops = replaceLabel ops bid []
          newbr  = obr { oOpr = Natural {oNatural = br { oBranchUs = newops}}}
      in reverse acc ++ (newbr:rest)
replaceBranch (o : rest) bid acc = replaceBranch rest bid (o:acc)


hasLabel [] = False
hasLabel (BlockRef {}:_) = True
hasLabel (_:rest) = hasLabel rest


getLabel [] = error "getLabel: This should not happen."
getLabel (BlockRef {blockRefId = bid }:_) = bid 
getLabel (_:rest) = getLabel rest


replaceLabel [] _bid _acc = error "replaceLabel: There should be a branch here"
replaceLabel (br @ BlockRef {blockRefId = _obid}:rest) bid acc =
  let newo = br {blockRefId = bid}
  in reverse acc ++ (newo:rest)
replaceLabel (o:rest) bid acc = replaceLabel rest bid (o:acc)


replaceBlock [] _bblock _acc = error "replaceBlock: This should not happen."
replaceBlock (Block {bLab = obid}:rest) (bblock @ Block {bLab = nbid}) acc
  | obid == nbid =
      reverse acc ++ (bblock:rest)
replaceBlock (b:rest) bblock acc = replaceBlock rest bblock (b:acc)

insertBlock [] _nbl _label _o _acc = error "insertBlock: This should not happen."
insertBlock (b @ Block {bLab = obid, bCode = bcode}:rest) nbl label o acc
  | obid == label =
      let bcode' = take ((length bcode) - 1) bcode ++ [o, last bcode] 
      in reverse acc ++ [b {bCode = bcode'}, nbl] ++ rest
insertBlock (b:rest) nbl label o acc = insertBlock rest nbl label o (b:acc)


applyToPhiOps oldl newl o
    | isPhi o = mapToOperandIf isBlockRef (replaceBlockRef oldl newl) o
    | otherwise = o

replaceBlockRef oldl newl (BlockRef l) | l == oldl = mkBlockRef newl
replaceBlockRef _oldl _newl b = b
