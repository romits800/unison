{-|
Copyright   :  Copyright (c) 2022, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Transformations.CopyBlocks (copyBlocks)
       where

-- import Unison.ParseSecurityPolicies
import Unison.Transformations.SecurityTypeInference

import qualified Data.Map as Map

import Unison.Base
import Unison.Util
import Unison.Target.API
import Unison.Constructors
import Unison.Predicates

copyBlocks policies gfMulImpl f @ Function {fCode = _} target =
  let
    types = inferSecurityTypes target f policies gfMulImpl
    bbs = map remOh (Map.toList $ fBbs types)
    bbsToCopy = filter (\(_,p) -> (has2Elems p) && ([] `elem` p)) bbs
    bbsToCopy' = map (\(f1,f2) -> (f1,head $ filter (\i -> i /= []) f2)) bbsToCopy
  in case bbsToCopy' of 
    [] -> f
    _  -> let f' = foldl (copyOneBlock target) f bbsToCopy' 
          in copyBlocks policies gfMulImpl f' target

remOh (v,lst) = (v, map (map fst) lst)


has2Elems (_:_:[]) = True
has2Elems _ = False

-- functions for extending the pattern thing I am using
-- Check if two sets are one subset of the other
-- Thought: why not making them Haskell Sets and then check
isSubset _ [] = True
isSubset [] _ = True
isSubset (a:resta) (b:restb) | a == b = isSubset resta restb
isSubset (_a:_) (_b:_) = False

-- get the difference between the subsets
-- remember to check both sides..
getSubset bs [] = bs 
getSubset [] bs = bs
getSubset (a1:[]) (b1:b2:restb) | a1 == b1 = (b1:b2:restb)
getSubset (b1:b2:restb) (a1:[]) | a1 == b1 = (b1:b2:restb)
getSubset (a:resta) (b:restb) | a == b = getSubset resta restb
getSubset (_a:_) (_b:_) = error "getSubset: this should not happen."




copyOneBlock target f @ Function {fCode = code} (bid,[bid']) =
  let
    bblock = findBlock code bid
    bblock' = findBlock code bid'
    f'   = addCopiedBlock target bblock  bblock' f
  in f'
copyOneBlock _target f @ Function {fCode = _code} _ = f

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
addCopiedBlock target bblock @ Block {bLab = obid, bCode = bcode} Block {bCode = tcBcode} f @ Function {fCode = code} =
  let bid    = newBlockIndex code -- new index for block
      freq   = blockFreq bblock `div` 2  -- TODO(Romy): This need to be fixed
      (tid, oid, _pid) = newIndexes $ flatten code
      label  = getBranch bcode
      -- replace the empty jump loop to jump to the new branch
      bcode' = replaceBranch bcode bid []
      code'  = replaceBlock code (bblock {bCode = bcode'}) []
      -- bout = blockOut bcode'
      oin = mkIn oid []
      ((_,oid'),os) = copyBlock target tcBcode [] (tid,oid+1)
      oout = mkOut (oid'+1) []
      o    = mkBranchInstruction (oid'+2) label target
      o2   = mkBranchInstruction (oid'+3) label target
      nbl  = mkNewBlock freq bid $ [oin] ++ os ++ [o, oout]
      code''  = insertBlock code' nbl (label-1) o2 []
      code''' = mapToOperationInBlocks (applyToPhiOps obid bid) code''
  in f {fCode = code'''}

-- At this level (Import) we don't have operand ids
-- Also no copies
copyBlock _ [] nbcode (tid,oid) = ((tid,oid), reverse nbcode) 
copyBlock target (Bundle {bundleOs= bs}:bcode) nbcode (tid,oid) = 
    let (ids', bs') = copyBlock target bs [] (tid,oid)
        nb = mkBundle bs'
    in copyBlock target bcode (nb:nbcode) ids'
-- Here there are also Phis.. TODO
-- Fun
--    Fun {
--      oFunctionUs :: [Operand r],
--      oFunctionDs :: [Operand r]
---    }
copyBlock target (o @ SingleOperation {oOpr = Virtual (Fun { oFunctionUs = urs,
                                                             oFunctionDs = dests}) }:bcode) nbcode (tid,oid) = 
    let 
        (tid',dests') = copyDests tid dests []
        -- Update future uses of the replaced dest 
        bcode' = foldl (\bc -> \(t1', t1) -> map (mapToModelOperand (replaceTemp t1 t1')) bc) bcode $ zip dests' dests
        nb = o { oId = oid, 
                 oOpr = Virtual (Fun {oFunctionUs = urs, oFunctionDs = dests'})} --mkFun oid urs dests'
        -- Find all uses for each of the dests'
        us = map (\t -> users t bcode') dests'
        -- Get all replaced tmps that do not have any use
        ds = map fst $ filter (\(_t,u) -> null u) $ zip dests' us
        -- mk kill ops
        (oid',kills) = foldl (\(oid,ops) -> \d -> (oid+1, (mkKill oid [VirtualInstruction] [d]):ops)) (oid+1, []) ds
        --bcode' = foldl (\bc -> \(t1',t1) -> map (mapToModelOperand (replaceTemp t1 t1')) bc) bcode (zip dests' dests)
    in copyBlock target bcode' (kills ++ [nb] ++ nbcode) (tid',oid')
-- Kills
copyBlock target (SingleOperation {oOpr = opr @ (Virtual (Kill {}))}:bcode) nbcode (tid,oid) = 
    let op' = mkSingleOperation oid opr
    in copyBlock target bcode (op':nbcode) (tid,oid+1)
copyBlock target (SingleOperation {oOpr = Virtual (_)}:bcode) nbcode ids = 
    copyBlock target bcode nbcode ids
copyBlock target (SingleOperation {oId = oldoid,
                                   oOpr= Natural {oNatural = Linear {
                                                                oIs = ins,
                                                                oUs = ops,
                                                                oDs = dests
                                                                }}}:bcode) nbcode (tid,oid) = 
    let 
        (tid',dests') = copyDests tid dests []
        -- Update future uses of the replaced dest 
        bcode' = foldl (\bc -> \(t1', t1) -> map (mapToModelOperand (replaceTemp t1 t1')) bc) bcode $ zip dests' dests
        bcode'' = map (replaceOp oldoid oid) bcode'
        nb = mkLinear oid ins ops dests'
        -- Find all uses for each of the dests'
        us = map (\t -> users t bcode'') dests'
        -- Get all replaced tmps that do not have any use
        ds = map fst $ filter (\(_t,u) -> null u) $ zip dests' us
        -- mk kill ops
        (oid',kills) = foldl (\(oid,ops) -> \d -> (oid+1, (mkKill oid [VirtualInstruction] [d]):ops)) (oid+1, []) ds
        --bcode' = foldl (\bc -> \(t1',t1) -> map (mapToModelOperand (replaceTemp t1 t1')) bc) bcode (zip dests' dests)
    in copyBlock target bcode'' (kills ++ [nb] ++ nbcode) (tid',oid')
copyBlock target (SingleOperation {oOpr= Natural {oNatural = Branch {}}}:bcode) nbcode ids = 
    copyBlock target bcode nbcode ids
copyBlock target (SingleOperation {oId = oldoid, 
                                   oOpr= opr @ Natural {oNatural = _}}:bcode) nbcode (tid,oid) = 
    let op' = mkSingleOperation oid opr
        bcode' = map (replaceOp oldoid oid) bcode
    in copyBlock target bcode' (op':nbcode) (tid,oid+1)
copyBlock _target (SingleOperation {oOpr = Copy {}}:_) _nbcode _ids = 
    error "copyBlock: Copies are not supported."

-- MaptoModelOperand applies to both
replaceTemp Temporary {tId = tid1} t2 Temporary {tId = tid} | tid == tid1 = t2
replaceTemp _t1 _t2 t | otherwise = t

replaceOp oldOid newOid o @ SingleOperation {oAs = as @ Attributes {aCall = Just coid}} | oldOid == coid = o { oAs = as {aCall = Just newOid}}
replaceOp _ _ o = o



copyDests tid [] acc = (tid,reverse acc)
copyDests tid (t@ Temporary {tId = _}:ops) acc = copyDests (tid+1) ops (t {tId = tid}:acc)
copyDests _tid (MOperand {}:_) _acc = error "copyDests: MOperand should not be here."
copyDests tid (t:ops) acc = copyDests tid ops (t:acc)


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
