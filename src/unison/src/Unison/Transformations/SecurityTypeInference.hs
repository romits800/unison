{-|
Copyright   :  Copyright (c) 2021, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Transformations.SecurityTypeInference (StateTuple,
                                                     inferSecurityTypes,
                                                     isMaybeSecret,
                                                     updateBSupps,
                                                     updateBUnqs,
                                                     updateBDoms,
                                                     updatePmapID)
       where

import Unison
import Unison.Target.API

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S

import Unison.ParseSecurityPolicies
import MachineIR

import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.BCFG as BCFG

data KnownOperations = KOAnd | KOOr | KOXor | KOGmul | KOOther
  deriving Show

-- TODO
-- data StateTuple r = StateTuple {
--   pMap  :: Map String (Policy String),          -- pmap
--   iNmap :: Map String (Policy String),          -- inmap
--   sUpp  :: Map String (Map String String),
--   uNq   :: Map String (Map String String),
--   dOm   :: Map String (Map String String),
--   xOr   :: Map String Bool,
--   m2O   :: Map String (Map Integer [String]),
--   c2O   :: Map String (Map Integer [String]),
--   p2P   :: Map Integer [Integer],
--   p2T   :: Map Integer [Operand r],
--   aRgs  :: Map String (KnownOperations, [String], [String]),
--   bBs   :: Map (Integer, Integer) (Policy String),
--   fLag  :: FlagsType
--   } deriving (Eq, Ord)

data FlagsType = FlagsType {
  isFixedPoint :: Bool,
  hasPhis :: Bool
  }

type StateTuple r = (Map String (Policy String),          -- pmap
                     Map String (Policy String),
                     Map String (Map String String),
                     Map String (Map String String),
                     Map String (Map String String),
                     Map String Bool,                     -- xor
                     Map String (Map Integer [String]),   -- m2o
                     Map String (Map Integer [String]),   -- c2o
                     Map Integer [Integer],               -- p2p
                     Map Integer [Operand r],
                     Map String (KnownOperations, [String], [String]),
                     Map (Integer, Integer) (Policy String),
                     FlagsType)


inferSecurityTypes target f @ Function {fCode = _} ipolicies =
  let --inpol           = showPolicies ([],[],[]) policies
      policies        = getPoliciesF f $ createPolicyMap ipolicies Map.empty
      pmap            = createPolicyMap policies Map.empty -- try to find a way to copy
      inmap           = createPolicyMap policies Map.empty
      supp            = createTSetMap policies Map.empty
      unq             = createRandomTSetMap policies Map.empty
      dom             = createRandomTSetMap policies Map.empty
      xor             = createXorMap policies Map.empty
      m2o             = Map.empty -- mem to operations
      c2o             = Map.empty -- copies to operations
      args            = Map.empty -- arguments of each operation
      bbs             = Map.empty -- basic-block relations
      -- ToDo:: Check if something else is required
      flag            = FlagsType { isFixedPoint = True, hasPhis = False }
      -- Get adjacent
      bif             = branchInfo target
      apf             = alignedPairs target
      bcfg            = BCFG.fromFunction bif f
      sg              = SG.fromFunction (Just apf) f
      congr           = sort $ map sort $ SG.sameOperandPartitions sg
      pp              = map S.fromList congr
      adjacent        = sort $ BCFG.eqvNeighborTemps bcfg pp
      p2p             = createP2p adjacent Map.empty -- adjacent operands

      p2t             = Map.empty -- o2t operands to temps
      
      t               = (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag)
      t'              = inferSecurityTypesI target t f
      -- t''             = inferTypes target t' f
  in t'


inferSecurityTypesI target t f =
  case inferTypes target t f of
    (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, FlagsType { hasPhis = True }) -> 
      let types = (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, FlagsType { isFixedPoint = True, hasPhis = False })
      in inferSecurityTypesI target types f
    types -> types


createPolicyMap::  [Policy String] -> Map String (Policy String) -> Map String (Policy String)
createPolicyMap [] m = m
createPolicyMap (p @ (Secret a):ps) m =
  createPolicyMap ps $ Map.insert a p m
createPolicyMap (p @ (Public a):ps) m =
  createPolicyMap ps $ Map.insert a p m
createPolicyMap (p @ (Random a):ps) m =
  createPolicyMap ps $ Map.insert a p m


createRandomTSetMap::  [Policy String] -> Map String (Map String String) -> Map String (Map String String)
createRandomTSetMap [] m = m
createRandomTSetMap ((Random a):ps) m =
  createRandomTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m 
createRandomTSetMap (_:ps) m = createRandomTSetMap ps m 

createXorMap::  [Policy String] -> Map String Bool -> Map String Bool
createXorMap [] m = m
createXorMap ((Random a):ps) m =
  createXorMap ps $ Map.insert a True m 
createXorMap ((Public a):ps) m =
  createXorMap ps $ Map.insert a True m 
createXorMap ((Secret a):ps) m =
  createXorMap ps $ Map.insert a True m 


createTSetMap::  [Policy String] -> Map String (Map String String) -> Map String (Map String String)
createTSetMap [] m = m
createTSetMap ((Secret a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m 
createTSetMap ((Public a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m
createTSetMap ((Random a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m


createP2p:: Show r => [(Operand r, Operand r)] -> Map Integer [Integer] -> Map Integer [Integer]
createP2p [] mp = mp
createP2p ops mp = foldl (\mp' -> \op -> createP2pi op mp') mp ops


createP2pi::Show r => (Operand r, Operand r) -> Map Integer [Integer] -> Map Integer [Integer]
createP2pi (o1, o2) mp =
  let oid1 = getOid o1
      oid2 = getOid o2
  in case Map.lookup oid2 mp of
    Nothing -> Map.insert oid2 [oid1] mp
    Just oids -> Map.insert oid2 (oid1:oids) mp


getOid OperandRef {operandRefId = orid}  = orid
getOid op = error $ "GetOid failed: " ++ show op


getOpidTemps MOperand {operandId = oid,
                       altTemps = ts} = (oid, ts)
getOpidTemps op = error $ "GetOidTemps failed: " ++ show op


getRegTemps MOperand {operandId = _,
                       altTemps = ts,
                       operandReg = r} =
  let ts' = getTids ts []
  in map (\t -> (t,r)) ts'
getRegTemps Temporary {tId = t,
                       tReg = r} = [('t':(show t),r)]

getRegTemps t = error $ "getRegTemps failed: " ++ show t


filterF (Secret ('F':_)) = True
filterF (Public ('F':_)) = True
filterF (Random ('F':_)) = True
filterF _ = False

getPoliciesF:: Show r => Show i => Function i r -> Map String (Policy String) -> [Policy String]
getPoliciesF Function {fCode = code} policies =
  let 
    fs = filter filterF $ map snd $ Map.toList policies
  in foldl (getPoliciesB policies) fs code


getPoliciesB:: Show r => Show i => Map String (Policy String)-> [Policy String] -> Block i r -> [Policy String]
getPoliciesB policies accpol Block {bCode = code} =
  foldl (getPoliciesO policies) accpol code

getPoliciesO :: Show r => Show i => Map String (Policy String) ->  [Policy String] -> BlockOperation i r -> [Policy String]
getPoliciesO pols accpol Bundle {bundleOs = bs } =
  foldl (getPoliciesO pols) accpol bs
getPoliciesO pols accpol SingleOperation
  { oOpr = Virtual ( Delimiter (In {
                                   oIns = oins
                                   })
                   )} =
                        let
                          rts = concatMap getRegTemps oins -- returns (t,r)
                          f (t,r) =
                            case r of
                              Just r' ->
                                case Map.lookup (show r') pols of
                                  Just (Secret _) -> [Secret t]
                                  Just (Public _) -> [Public t]
                                  Just (Random _) -> [Random t]
                                  Nothing -> []
                              Nothing -> []
                          nlist = concatMap f rts
                        in nlist ++ accpol
getPoliciesO _ accpol _ = accpol


addPrefix True = "F"
addPrefix False = "S"

inferTypes:: Show r => Show i => (TargetDescription i r0 rc0 s0, TargetOptions) -> StateTuple r -> Function i r -> StateTuple r 
inferTypes target types f @ Function {fCode = code} =
  foldl (inferTypesBlock target f) types code
  


inferTypesBlock:: Show r => Show i => (TargetDescription i r0 rc0 s0, TargetOptions) -> Function i r -> StateTuple r -> Block i r -> StateTuple r
inferTypesBlock target f types Block {bLab= bid, bCode = code} =
  inferTypesOperation target f bid types code

    
inferTypesOperation :: Show r => Show i => Integral a => (TargetDescription i r0 rc0 s0, TargetOptions) -> Function i r -> BlockId -> StateTuple r -> [BlockOperation i r] -> StateTuple r
inferTypesOperation _ _ _ types [] = types
inferTypesOperation target f bid types (Bundle {bundleOs = bs}:codes) =
  inferTypesOperation target f bid types (bs ++ codes)
-- Branch Operations
inferTypesOperation target f bid types (
  op1 @ SingleOperation {oOpr = Natural {oNatural = Branch {
                                            oBranchIs = _, -- instruction i
                                              oBranchUs = tbs }}}:codes) =
  let d = "brtemp"
  in case splitOps tbs of
      (t1:t2:[], [BlockRef {blockRefId=blid}]) ->
        let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
            isxor = False
            isgmul= False
            ts1 = getTids [t1] []
            ts2 = getTids [t2] []
            xor' = updateXorD xor ts1 ts2 isxor [d]
            args' = updateArgsD args KOOther ts1 ts2 [d]
            supp' = updateBSuppsD isxor (supp, xor', args') ts1 ts2 [d]
            unq' = updateBUnqsD (unq, supp) ts1 ts2 [d]
            -- if is xor
            dom' = updateBDomsEmptyD dom [d]
            pmap' = updatePmapID isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) ts1 ts2 d
            typ  = Map.lookup "brtemp" pmap'
            bbs' = 
              if isMaybeSecret typ then Map.insert (blid, bid+1) (fromJustSecret typ) bbs
              else bbs
            types' = (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs', flag)
        in inferTypesOperation target f bid types' codes
      (t1:[], [BlockRef {blockRefId=blid}])    ->
        let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
            ts1 = getTids [t1] []
            supp' = updateSuppsD supp ts1 [d]
            unq'  = updateUnqsD unq ts1 [d]
            dom'  = updateDomsD dom ts1 [d]
            isxor = False
            isgmul = False
            xor' = updateXorD xor ts1 [] True [d]
            args' = updateArgsUopD args  ts1 [d]
            pmap' = updatePmapID isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) ts1 [] d
            typ  = Map.lookup "brtemp" pmap'
            bbs' = 
              if isMaybeSecret typ then Map.insert (blid, bid+1) (fromJustSecret typ) bbs
              else bbs
            types' = (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs', flag)
        in -- if bbs == bbs'
           -- then error $ show typ
           -- else
          inferTypesOperation target f bid types' codes
      ([], [BlockRef {blockRefId=_}])         -> inferTypesOperation target f bid types codes
      -- a case with return - should always be safe
      ([_], [])                                  -> inferTypesOperation target f bid types codes
      ([], [])                                   -> inferTypesOperation target f bid types codes
      (tbv, tbb)                                 -> error ("Should not happen: " ++ show op1 ++ show tbv ++ show tbb)
-- Normal Operations
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = (MOperand {altTemps = ts1}) :
                            (MOperand {altTemps = ts2}) : _,
                      -- TODO(fix): trying to avoid the control register 
                      oDs = d:_ }}}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    isxor = any (isXor target) i
    isgmul= any (isGMul target) i
    xor' = updateXor xor ts1 ts2 isxor [d]
    args' = updateArgs args (knownOp target i) ts1 ts2 [d]
    supp' = updateBSupps isxor (supp, xor', args') ts1 ts2 [d]
    unq' = updateBUnqs (unq, supp) ts1 ts2 [d]
    -- if is xor
    dom' = if isxor
           then updateBDoms (dom, unq') ts1 ts2 [d]
           else updateBDomsEmpty dom [d]
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) ts1 ts2 [d]
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- Normal Operations with Temps
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = (t1 @ (Temporary {})) :
                            (t2 @ (Temporary {})) : _,
                      -- TODO(fix): trying to avoid the control register 
                      oDs = d:_ }}}:codes) =
  let
    -- exception for control register???
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    -- if is xor
    isxor = any (isXor target) i
    isgmul= any (isGMul target) i
    xor' = updateXor xor [t1] [t2] isxor [d]
    args' = updateArgs args (knownOp target i) [t1] [t2] [d]
    supp' = updateBSupps isxor (supp, xor', args') [t1] [t2] [d]
    unq' = updateBUnqs (unq, supp) [t1] [t2] [d]
    dom' = if isxor
           then updateBDoms (dom, unq') [t1] [t2] [d]
           else updateBDomsEmpty dom [d]
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) [t1] [t2] [d]
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- store operation
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} :
                            mfi @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                             mfiFixed = isfixed})) : _,
                      oDs = [] }}}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp ts [mfi]
    unq'  = updateUnqs unq ts [mfi]
    dom'  = updateDoms dom ts [mfi]
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    m2o' = updateM2o m2o (addPrefix isfixed ++ show mf) oid ts
    args' = updateArgsUop args  ts [mfi]
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag) ts [] [mfi]
    types' = (pmap', init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = t @ Temporary {} :
                            mfi @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                             mfiFixed = isfixed})) : _,
                      oDs = [] }}}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp [t] [mfi]
    unq'  = updateUnqs unq [t] [mfi]
    dom'  = updateDoms dom [t] [mfi]
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    m2o' = updateM2o m2o (addPrefix isfixed ++ show mf) oid []
    args' = updateArgsUop args  [t] [mfi]
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag) [t] [] [mfi]
    types' = (pmap', init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- operation with one operand
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} : _,
                      oDs = defops }}}:codes) =
  -- if join of uops is...
  -- add defops to the map
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp ts defops
    unq'  = updateUnqs unq ts defops
    dom'  = updateDoms dom ts defops
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    xor' = updateXor xor ts [] True defops
    args' = updateArgsUop args  ts defops
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) ts [] defops 
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = t @ Temporary {} : _,
                      oDs = defops }}}:codes) =
  -- if join of uops is...
  -- add defops to the map
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp [t] defops
    unq'  = updateUnqs unq [t] defops
    dom'  = updateDoms dom [t] defops
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    xor' = updateXor xor [t] [] True defops
    args' = updateArgsUop args  [t] defops
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) [t] [] defops 
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- stack operations
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (Bound (_)) : [],
                      oDs = [] }}}:codes) =
  inferTypesOperation target f bid types codes
-- load operation
inferTypesOperation target f bid types (SingleOperation
  {
    oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (u @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                           mfiFixed = isfixed}))) : _,
                      oDs = [MOperand {altTemps = d}] }}}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp [u] d
    unq'  = updateUnqs unq [u] d
    dom'  = updateDoms dom [u] d
    isxor = False
    isgmul= False
    xor' = updateXor xor d [] True d
    args' = updateArgsUop args  [u] d
    m2o' = updateM2o m2o (addPrefix isfixed ++ show mf) oid d
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag) [u] [] d
    types' = (pmap', init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
inferTypesOperation target f bid types (SingleOperation
  {
    oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (u @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                            mfiFixed = isfixed}))) : _,
                      oDs = [t @ Temporary {}] }}}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp [u] [t]
    unq'  = updateUnqs unq [u] [t]
    dom'  = updateDoms dom [u] [t]
    isxor = False
    isgmul= False
    xor' = updateXor xor [t] [] True [t]
    args' = updateArgsUop args  [u] [t]
    m2o' = updateM2o m2o (addPrefix isfixed ++ show mf) oid []
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag) [u] [] [t]
    types' = (pmap', init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- operation with one static operand
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = Bound (MachineImm {}) : _,
                      oDs = d:_ }}}:codes) = 
  -- if join of uops is...
  -- add defops to the map
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp'  = updateSupps supp [] [d]
    unq'   = updateUnqs unq [] [d]
    dom'   = updateDoms dom [] [d]
    isxor  = False
    isgmul = False
    xor'   = updateXor xor [] [] True [d]
    args'  = updateArgsUop args  [] [d]
    pmap'  = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) [] [] [d]
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
  -- in pmap' `seq` error "lalal"
inferTypesOperation _ _ _ _ (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = ins, -- Instruction i
                      oUs = ous,
                      oDs = defops }}}:_) =
  -- if join of uops is...
  -- add defops to the map
  error ("SecTypeInf: Operation with unknown operands: " ++
         "Len ins:" ++ (show $ length ins) ++ " " ++
         "Len oups:" ++ (show $ length ous) ++ " " ++
         "Len defops:" ++ (show $ length defops) ++ " " ++
         foldl (\s -> \op -> s ++ " " ++ (show op)) "Use Operands: " ous ++ "\n" ++
         foldl (\s -> \op -> s ++ " " ++ (show op)) "Def Operands: " defops ++ "\n" ++
         foldl (\s -> \i  -> s ++ " " ++ (show i))  "Instructions: " ins ++ "\n")
-- inferTypesOperation target f bid types (SingleOperation
--   {oOpr = Natural {oNatural = Branch {
--                       oBranchIs = _, -- instruction i
--                       oBranchUs = _ }}}:codes) = inferTypesOperation target f bid types codes
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Call {
                      oCallIs = _, -- Instruction i
                      oCallUs = _ }}}:codes) = inferTypesOperation target f bid types codes
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = TailCall {
                      oTailCallIs = _, -- Instruction i
                      oTailCallUs = _ }}}:codes) = inferTypesOperation target f bid types codes
-- copies
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Copy { oCopyIs = _,
                  oCopyS = sop,
                  oCopyUs = uops,
                  oCopyD = dop,
                  oCopyDs = defops }}:codes) =
  let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
      supp' = updateSupps supp (sop:uops) (dop:defops)
      unq'  = updateUnqs unq (sop:uops) (dop:defops)
      dom'  = updateDoms dom (sop:uops) (dop:defops)
      isxor = False
      isgmul= False
      xor' = updateXor xor (sop:uops) [] True (dop:defops)
      args' = updateArgsUop args  (sop:uops) (dop:defops)
      c2o' = updateC2o c2o (dop:defops) oid (sop:uops) -- take only destinations - dests and sources are the same..
      pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag) (sop:uops) [] (dop:defops)
      types' = (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- virtual copies
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Virtual (VirtualCopy { oVirtualCopyS = s,
                                  oVirtualCopyD = d })}:codes) =
  let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
      supp' = updateSupps supp [s] [d]
      unq'  = updateUnqs unq [s] [d]
      dom'  = updateDoms dom [s] [d]
      xor'  = updateXor xor [s] [] True [d]
      args' = updateArgsUop args  [s] [d]
      c2o'  = updateC2o c2o [d] oid [s]-- take only destinations - dests and sources are the same..
      pmap' = updatePmaps False False (pmap, init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag) [s] [] [d]
      types' = (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types' codes
-- -- TODO(VirtualOperation): Phi/Delimiter/Kill/Define
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Fun  { oFunctionUs = oFus,
                            oFunctionDs = oFds
                          }
                   )}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp oFus oFds -- assume no side effects in function
    -- This will anyways be False
    xor' = updateXor xor oFus [] False oFds
    -- args' = updateArgs args KOOther ts1 ts2 [d]
    -- supp' = updateBSupps isxor (supp, xor', args') ts1 ts2 [d]
    unq' = updateLUnqs (unq, supp) oFus oFds
    -- if is xor
    dom' = updateBDomsEmpty dom oFds
    pmap' = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oFus oFds
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag)
  in inferTypesOperation target f bid types' codes
-- Phi (TODO)
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Phi  { oPhiUs = oFus,
                            oPhiD = oFd
                          }
                   )}:codes) =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps supp oFus [oFd] -- assume no side effects in function
    -- This will anyways be False
    xor' = updateXor xor oFus [] False [oFd]
    -- args' = updateArgs args KOOther ts1 ts2 [d]
    -- supp' = updateBSupps isxor (supp, xor', args') ts1 ts2 [d]
    unq' = updateLUnqs (unq, supp) oFus [oFd]
    -- if is xor
    dom' = updateBDomsEmpty dom [oFd]
    pmap' = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oFus [oFd]
    types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag)
  in inferTypesOperation target f bid types' codes

inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (In {
                                   oIns = oins
                                   })
                   )}:codes) | all isMOperand oins = 
  let
    (_, _, _, _, _, _, _, _, p2p, p2t, _, _, _) = types
    ins = map getOpidTemps oins
    types' = foldl (updateNewTemps p2t p2p) types ins
  in inferTypesOperation target f bid types' codes
inferTypesOperation target f @ Function {fCode = code} bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (Out {
                                   oOuts = oouts
                                   })
                   )}:codes) | all isMOperand oouts = 
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    p2t' = foldl (\p2t'' -> \(p,ts) -> Map.insert p ts p2t'') p2t $ map getOpidTemps oouts
    init' = if (fromIntegral (length(code))-1 == bid) then insertOutTemps pmap init oouts else init
    types' = (pmap, init', supp, unq, dom, xor, m2o, c2o, p2p, p2t', args, bbs, flag)
  in inferTypesOperation target f bid types' codes
-- TODO: fix these - import will not know about intra-block dependencies
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (In {})
                   )}:codes) =  inferTypesOperation target f bid types codes
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (Out {})
                   )}:codes) = inferTypesOperation target f bid types codes
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual (_)}:codes) = inferTypesOperation target f bid types codes

updateNewTemps :: Show r => Map Integer [Operand r] -> Map Integer [Integer] -> StateTuple r -> (Integer, [Operand r]) -> StateTuple r
updateNewTemps p2t p2p types (oid, tmps) = 
  let 
    (pmap, init, supp, unq, dom, xor, m2o, c2o, _, _, args, bbs, flag) = types
    adj = Map.lookup oid p2p
    -- in case of a phi
    f oldtmps isAnyNothing tmps =
      let
        supp' = updateSupps supp oldtmps tmps 
        xor' = updateXor xor oldtmps [] True tmps
        -- args' = updateArgs args KOOther ts1 ts2 [d]
        unq' = updateLUnqs (unq, supp) oldtmps tmps
        dom' = updateBDomsEmpty dom tmps
        pmap' = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oldtmps tmps
        flag' = flag {hasPhis = isAnyNothing}
        types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag')
      in types'
    f2 oldtmps tmps = 
      let 
        supp' = updateSupps supp oldtmps tmps
        unq'  = updateUnqs unq oldtmps tmps
        dom'  = updateDoms dom oldtmps tmps
        xor'  = updateXor xor oldtmps [] True tmps
        args' = updateArgsUop args  oldtmps tmps
        pmap' = updatePmaps False False (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) oldtmps [] tmps
      in (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in case adj of
       Just [a] -> 
         let oldtmps = fJust $ Map.lookup a p2t
         in f2 oldtmps tmps
       Just as -> 
         let
           -- contain nothing if the definition is ahead
           ats = map (\ai -> Map.lookup ai p2t) as
           isNothing = \ai -> case ai of
                                   Nothing -> True
                                   _ -> False
           notIsNoth = \ai -> not $ isNothing ai
           isAnyNothing = any isNothing ats
           ats_nonothing = concatMap fJust $ filter notIsNoth ats
           -- ats = concatMap id ats_nonothing
         in f ats_nonothing isAnyNothing tmps
       Nothing -> types

updateM2o m2o fid oid tids =
  let
    dtids = getTids tids [] -- destinations
    f v = case v of
      Just m -> Just (Map.insert oid dtids m)
      Nothing -> Just (Map.insert oid dtids Map.empty)
  in Map.alter f fid m2o


updateC2o c2o dts oid sts =
  let
    dtids  = getTids dts [] -- destinations
    stids  = getTids sts [] -- sources
    f v = case v of
      Just m -> Just (Map.insert oid stids m)
      Nothing -> Just (Map.insert oid stids Map.empty)
  in foldl (\mnew -> \cid -> Map.alter f cid mnew) c2o dtids 



insertOutTemps pmap init oouts =
  let 
    souts = getTids oouts [] -- sources
    f init tid = Map.insert tid (fJust $ Map.lookup tid pmap) init
    init' = foldl f init souts
  in init'


anyOp _ [] = Nothing
anyOp f (h:tail) = case f h of
  Just x -> Just x
  Nothing -> anyOp f tail

getSameOperand :: Map String (KnownOperations, [String], [String]) -> [String] -> [String]
  -> Maybe (KnownOperations, [String], [String])
getSameOperand args ts1 ts2 =
  let f  (t,t') = case Map.lookup t' args of
        Just (op, tsa, tsb) | t `elem` tsa -> Just (op, tsa, tsb)
        Just (op, tsa, tsb) | t `elem` tsb -> Just (op, tsb, tsa)
        Nothing -> Nothing
      ext ts1 ts2 = [ (t1,t2) | t1 <- ts1, t2 <- ts2 ]
  in if isSameOperand args ts1 ts2 then
       (anyOp f $ ext ts1 ts2) 
     else (anyOp f $ ext ts2 ts1) 


isSameOperand :: Map String (KnownOperations, [String], [String]) -> [String] -> [String] -> Bool
isSameOperand args ts1 ts2 =
  let f (t,t') = case Map.lookup t' args of
        Just (_, tsa, tsb) | t `elem` tsa || t `elem` tsb -> True
        _ -> False
      ext ts1 ts2 = [ (t1,t2) | t1 <- ts1, t2 <- ts2 ]
  in  (any  f $ ext ts1 ts2) 


isExactlySameOperand :: Map String (KnownOperations, [String], [String]) -> [String] -> [String] -> Bool
isExactlySameOperand args ts1 ts2 =
  let f (t,t') = case (Map.lookup t' args, Map.lookup t args) of
        (Just (_, tsa', tsb'), Just (_, tsa, tsb)) | tsa == tsa' && tsb == tsb' -> True
        _ -> False
      ext ts1 ts2 = [ (t1,t2) | t1 <- ts1, t2 <- ts2 ]
  in  (any  f $ ext ts1 ts2) 


getDistributiveOperands :: Map String (KnownOperations, [String], [String]) -> [String] -> [String]
  -> Maybe ([String], [String], [String])
getDistributiveOperands args ts1 ts2 =
  let f  (t,t') = case (Map.lookup t args, Map.lookup t' args) of
        (Just (KOGmul, tsa, tsb), Just (KOGmul, tsa', tsb'))
          | tsa `intersect` tsa' /= [] -> Just (tsa, tsb, tsb')
        (Just (KOGmul, tsa, tsb), Just (KOGmul, tsa', tsb'))
          | tsa `intersect` tsb' /= [] -> Just (tsa, tsb, tsa')
        (Just (KOGmul, tsa, tsb), Just (KOGmul, tsa', tsb'))
          | tsb `intersect` tsa' /= [] -> Just (tsb, tsb', tsa)
        (Just (KOGmul, tsa, tsb), Just (KOGmul, tsa', tsb'))
          | tsb `intersect` tsb' /= [] -> Just (tsb, tsa, tsa')
        _ -> Nothing
      ext ts1 ts2 = [ (t1,t2) | t1 <- ts1, t2 <- ts2 ]
  in (anyOp f $ ext ts2 ts1) 

isDistributive :: Map String (KnownOperations, [String], [String]) -> [String] -> [String] -> Bool
isDistributive args ts1 ts2 =
  let f (t,t') = case (Map.lookup t args, Map.lookup t' args) of
        (Just (KOGmul, tsa, _), Just (KOGmul, tsa', _)) | tsa `intersect` tsa' /= [] -> True
        (Just (KOGmul, tsa, _), Just (KOGmul, _, tsb')) | tsa `intersect` tsb' /= [] -> True
        (Just (KOGmul, _, tsb), Just (KOGmul, tsa', _)) | tsb `intersect` tsa' /= [] -> True
        (Just (KOGmul, _, tsb), Just (KOGmul, _, tsb')) | tsb `intersect` tsb' /= [] -> True
        _ -> False
      ext ts1 ts2 = [ (t1,t2) | t1 <- ts1, t2 <- ts2 ]
  in  (any f $ ext ts1 ts2) 

  

knownOp target i =
  let isxor = any (isXor target) i
      isgmul = any (isGMul target) i
  in case (isxor, isgmul) of
    (True,_) -> KOXor
    (_,True) -> KOGmul
    (_,_) -> KOOther


updateArgs args op ts1 ts2 dts = 
  let
    stids1 = getTids ts1 [] -- sources
    stids2 = getTids ts2 [] -- sources
    dtids  = getTids dts [] -- destinations
    f s dtid = Map.insert dtid (op, stids1, stids2) s
    args' = foldl f args dtids
  in args'

updateArgsD args op ts1 ts2 dts = 
  let
    f s dtid = Map.insert dtid (op, ts1, ts2) s
    args' = foldl f args dts
  in args'

updateArgsUop args ts dts = 
  let
    stids = getTids ts [] -- sources
    dtids  = getTids dts [] -- destinations
    f tid = Map.lookup tid args
    s1 = anyOp f stids
    f2 s1 s dtid = case s1 of
      Just x -> Map.insert dtid x s
      Nothing -> s
    args' = foldl (f2 s1) args dtids
  in args'

updateArgsUopD args stids dtids = 
  let
    f tid = Map.lookup tid args
    s1 = anyOp f stids
    f2 s1 s dtid = case s1 of
      Just x -> Map.insert dtid x s
      Nothing -> s
    args' = foldl (f2 s1) args dtids
  in args'



updateXor :: Map String Bool -> [Operand r] -> [Operand r] -> Bool -> [Operand r] -> Map String Bool
updateXor xor ts1 ts2 val dts = 
  let
    stids1 = getTids ts1 [] -- sources
    stids2 = getTids ts2 [] -- sources
    dtids  = getTids dts [] -- destinations
    val1   = foldl (&&) True $ map (\tid -> Map.findWithDefault True tid xor) stids1
    val2   = foldl (&&) True $ map (\tid -> Map.findWithDefault True tid xor) stids2
    f2 s dtid = Map.insert dtid (val && val1 && val2) s
    xor' = foldl f2 xor dtids
  in xor'

updateXorD :: Map String Bool -> [String] -> [String] -> Bool -> [String] -> Map String Bool
updateXorD xor ts1 ts2 val dts = 
  let
    val1   = foldl (&&) True $ map (\tid -> Map.findWithDefault True tid xor) ts1
    val2   = foldl (&&) True $ map (\tid -> Map.findWithDefault True tid xor) ts2
    f2 s dtid = Map.insert dtid (val && val1 && val2) s
    xor' = foldl f2 xor dts
  in xor'


updateSupps supp sts dts =
  let stids = getTids sts [] -- sources
      dtids = getTids dts [] -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f supp) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      supp' = foldl (f2 s1) supp dtids
  in supp'

updateSuppsD supp sts dts =
  let stids = sts
      dtids = dts
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f supp) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      supp' = foldl (f2 s1) supp dtids
  in supp'



updateUnqs unq sts dts =
  let stids = getTids sts [] -- sources
      dtids = getTids dts [] -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f unq) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      unq' = foldl (f2 s1) unq dtids
  in unq'


updateUnqsD unq stids dtids =
  let f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f unq) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      unq' = foldl (f2 s1) unq dtids
  in unq'

updateDoms dom sts dts =
  let stids = getTids sts [] -- sources
      dtids = getTids dts [] -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f dom) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      dom' = foldl (f2 s1) dom dtids
  in dom'

updateDomsD dom stids dtids =
  let f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f dom) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      dom' = foldl (f2 s1) dom dtids
  in dom'

-- To check this
all_xor xor stids =
  foldl (&&) True $ map (\tid -> Map.findWithDefault True tid xor) stids
  
updateBSupps True (supp, _, args) sts1 sts2 dts
  | (isSameOperand args ts1 ts2 || isSameOperand args ts2 ts1) =
    case getSameOperand args ts1 ts2 of
      Just (KOXor, _, ts2') ->
        let f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
            s2    = foldl (f supp) Map.empty ts2'
            f2 s1 s dtid = Map.insert dtid s1 s
            supp' = foldl (f2 s2) supp dtids
        in supp'
      _ -> updateSupps supp (sts1 ++ sts2) dts
  where
    dtids = getTids dts [] -- destinations
    ts1 = getTids sts1 [] -- destinations
    ts2 = getTids sts2 [] -- destinations
updateBSupps _ (supp, xor,_) sts1 sts2 dts | all_xor xor (getTids dts []) =
  let stids1 = getTids sts1 [] -- sources
      stids2 = getTids sts2 [] -- sources
      dtids = getTids dts [] -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f supp) Map.empty stids1
      s2    = foldl (f supp) Map.empty stids2
      s1s2  = Map.difference (Map.union s1 s2) (Map.intersection s1 s2)
      f2 s1 s dtid = Map.insert dtid s1 s
      supp' = foldl (f2 s1s2) supp dtids
  in supp'
updateBSupps _ (supp, _, _) sts1 sts2 dts =
  updateSupps supp (sts1 ++ sts2) dts



updateBSuppsD True (supp, _, args) ts1 ts2 dts
  | (isSameOperand args ts1 ts2 || isSameOperand args ts2 ts1) =
    case getSameOperand args ts1 ts2 of
      Just (KOXor, _, ts2') ->
        let f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
            s2    = foldl (f supp) Map.empty ts2'
            f2 s1 s dtid = Map.insert dtid s1 s
            supp' = foldl (f2 s2) supp dtids
        in supp'
      _ -> updateSuppsD supp (ts1 ++ ts2) dts
  where
    dtids = dts
updateBSuppsD _ (supp, xor,_) sts1 sts2 dts | all_xor xor dts =
  let stids1 = sts1
      stids2 = sts2
      dtids = dts
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f supp) Map.empty stids1
      s2    = foldl (f supp) Map.empty stids2
      s1s2  = Map.difference (Map.union s1 s2) (Map.intersection s1 s2)
      f2 s1 s dtid = Map.insert dtid s1 s
      supp' = foldl (f2 s1s2) supp dtids
  in supp'
updateBSuppsD _ (supp, _, _) sts1 sts2 dts =
  updateSuppsD supp (sts1 ++ sts2) dts

updateBUnqs (unq, supp) sts1 sts2 dts =
  let stids1  = getTids sts1 [] -- sources
      stids2  = getTids sts2 [] -- sources
      dtids   = getTids dts []  -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1      = foldl (f supp) Map.empty stids1
      s2      = foldl (f supp) Map.empty stids2
      u1      = foldl (f unq)  Map.empty stids1
      u2      = foldl (f unq)  Map.empty stids2
      u1u2    = Map.union u1 u2
      s1s2    = Map.intersection s1 s2
      unqt    = Map.difference u1u2 s1s2
      f2 ut u dtid = Map.insert dtid ut u 
      unq'    = foldl (f2 unqt) unq dtids
  in unq'

updateLUnqs (unq, supp) sts dts =
  let stids  = map (\x -> getTids [x] []) sts -- sources
      dtids   = getTids dts []  -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      ss      = map (foldl (f supp) Map.empty) stids
      us      = map (foldl (f unq) Map.empty) stids
      u1u2    = foldl Map.union Map.empty us
      s1s2    = case ss of
        sh:st -> foldl Map.intersection sh st --Map.intersection s1 s2
        []   -> Map.empty
      unqt    = Map.difference u1u2 s1s2
      f2 ut u dtid = Map.insert dtid ut u 
      unq'    = foldl (f2 unqt) unq dtids
  in unq'


updateBUnqsD (unq, supp) sts1 sts2 dts =
  let stids1  = sts1
      stids2  = sts2
      dtids   = dts
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1      = foldl (f supp) Map.empty stids1
      s2      = foldl (f supp) Map.empty stids2
      u1      = foldl (f unq)  Map.empty stids1
      u2      = foldl (f unq)  Map.empty stids2
      u1u2    = Map.union u1 u2
      s1s2    = Map.intersection s1 s2
      unqt    = Map.difference u1u2 s1s2
      f2 ut u dtid = Map.insert dtid ut u 
      unq'    = foldl (f2 unqt) unq dtids
  in unq'


mapIntersection (h:tl) = foldl Map.union h tl
mapIntersection []     = Map.empty

updateBDoms (dom, unq) sts1 sts2 dts =
  let stids1  = getTids sts1 [] -- sources
      stids2  = getTids sts2 [] -- sources
      dtids   = getTids dts  [] -- destinations
      f m tid = Map.findWithDefault Map.empty tid m 
      d1      = mapIntersection $ map (f dom) stids1
      d2      = mapIntersection $ map (f dom) stids2
      u0      = mapIntersection $ map (f unq) dtids
      d1d2    = Map.union d1 d2
      domt    = Map.intersection d1d2 u0
      f2 ut u dtid = Map.insert dtid ut u 
      dom'    = foldl (f2 domt) dom dtids
  in dom'


updateBDomsEmpty dom dts =
  let dtids   = getTids dts []  -- destinations
      f2 u dtid = Map.insert dtid Map.empty  u 
      dom'    = foldl f2 dom dtids
  in dom'

updateBDomsEmptyD dom dtids =
  let f2 u dtid = Map.insert dtid Map.empty  u 
      dom'    = foldl f2 dom dtids
  in dom'


updateBDomsD (dom, unq) sts1 sts2 dts =
  let stids1  = sts1
      stids2  = sts2
      dtids   = dts
      f m tid = Map.findWithDefault Map.empty tid m 
      d1      = mapIntersection $ map (f dom) stids1
      d2      = mapIntersection $ map (f dom) stids2
      u0      = mapIntersection $ map (f unq) dtids
      d1d2    = Map.union d1 d2
      domt    = Map.intersection d1d2 u0
      f2 ut u dtid = Map.insert dtid ut u 
      dom'    = foldl (f2 domt) dom dtids
  in dom'

updateLPmaps (pmap, _, _, _, _, _, _, _, _, _, _, _, _) _ [] = pmap
updateLPmaps types ts dts =
  let stids   = getTids ts []  -- sources
      dtids = getTids dts []              -- destinations
      (pmap, _, _, _, _, _, _, _, _, _, _, _, _) = types
      typs  = map (\tid -> Map.lookup tid pmap) stids
      typ   = case mergeTypes typs of
        Nothing -> Secret "t"
        Just ty -> ty
      f2 typ pmap dtid = Map.insert dtid typ pmap 
      pmap'    = foldl (f2 typ) pmap dtids
  in pmap'

-- updateLPmapsID (pmap, _, _, _, _, _, _, _, _, _, _, _, _) _ [] = pmap
-- updateLPmapsID types @ (_, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) ts1 ts2 (dt:dts) =
--   let pmap' = updatePmapID False False types ts1 ts2 dt
--   in updateLPmapsID isxor isgmul (pmap', init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) ts1 ts2 dts


updatePmaps _ _ (pmap, _, _, _, _, _, _, _, _, _, _, _, _) _ _ [] = pmap
updatePmaps isxor isgmul types ts1 ts2 dts =
  let stids1 = getTids ts1 [] -- sources1
      stids2 = getTids ts2 [] -- sources2
      dtids  = getTids dts [] -- destinations
  in updatePmapsID isxor isgmul types stids1 stids2 dtids

updatePmapsID _ _ (pmap, _, _, _, _, _, _, _, _, _, _, _, _) _ _ [] = pmap
updatePmapsID isxor isgmul types @ (_, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) ts1 ts2 (dt:dts) =
  let pmap' = updatePmapID isxor isgmul types ts1 ts2 dt
  in updatePmapsID isxor isgmul (pmap', init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) ts1 ts2 dts
  
updatePmapID _ _ (pmap, _, _, _, dom, _, _, _, _, _, _, _, _) _ _ dt
  | not $ isEmpty dt dom =
    Map.insert dt (Random dt) pmap
updatePmapID _ _ (pmap, init, supp, _, dom, _, _, _, _, _, _, _, _) _ _ dt
  | (isEmpty dt dom) && (not $ intersectSec supp init dt)  =
    Map.insert dt (Public dt) pmap    
updatePmapID True _ (pmap, _, _, _, _, _, _, _, _, _, args, _, _) ts1 ts2 dt
  | isExactlySameOperand args ts1 ts2 = Map.insert dt (Public dt) pmap
updatePmapID True isgmul types @ (pmap, _, _, _, _, _, _, _, _, _, args, _, _) ts1 ts2 dt
  | (isSameOperand args ts1 ts2 || isSameOperand args ts2 ts1) =
    case getSameOperand args ts1 ts2 of
      Nothing -> updatePmapID True isgmul types ts1 ts2 dt
      Just (KOOther, _, _) -> updatePmapID True isgmul types ts1 ts2 dt
      -- Just (KOXor, _, _) -> updatePmapID False False types ts1 ts2 dt
      Just (KOXor, _, ts2') -> updatePmapID False False types ts2' [] dt
      Just (KOOr, ts1', ts2') -> updatePmapID False False types ts1' ts2' dt
      Just (KOAnd, ts1', ts2') -> updatePmapID False False types ts1' ts2' dt
      Just (KOGmul, ts1, ts2) ->
        let typs1  = map (\tid -> Map.lookup tid pmap) ts1
            typs2  = map (\tid -> Map.lookup tid pmap) ts2
            typ1   = mergeTypes typs1
            typ2   = mergeTypes typs2
        in
          case (typ1,typ2) of
            (Just (Secret _), _) -> Map.insert dt (Secret dt) pmap
            (_,Just (Secret _)) -> Map.insert dt (Secret dt) pmap
            _ -> Map.insert dt (Public dt) pmap
updatePmapID True _ (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) ts1 ts2 dt
  | isDistributive args ts1 ts2 =
    case getDistributiveOperands args ts1 ts2 of
      Nothing -> error "updatePmapID: Should not happen!"
      Just (ts1, ts2, ts3) -> 
        let
          isxor = True
          xor' = updateXorD xor ts2 ts3 isxor ["dist"]
          args' = updateArgsD args KOXor ts2 ts3 ["dist"]
          supp' = updateBSuppsD isxor (supp, xor', args') ts2 ts3 ["dist"]
          unq' = updateBUnqsD (unq, supp) ts2 ts3 ["dist"]
          dom' = updateBDomsD (dom, unq') ts2 ts3 ["dist"]
          types' = (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
          pmap' = updatePmapID True False types' ts2 ts3 "dist"
          
          xor'' = updateXorD xor' ts1 ["dist"] False [dt]
          args'' = updateArgsD args' KOGmul ts1 ["dist"] [dt]
          supp'' = updateBSuppsD False (supp', xor'', args'') ts1 ["dist"] [dt]
          unq'' = updateBUnqsD (unq', supp') ts1 ["dist"] [dt]
          dom'' = updateBDomsD (dom', unq'') ts1 ["dist"] [dt]
          types'' = (pmap', init, supp'', unq'', dom'', xor'', m2o, c2o, p2p, p2t, args'', bbs, flag)
        in updatePmapID False True types'' ts1 ["dist"] dt
updatePmapID isxor isgmul (pmap, init, supp, _, dom, xor, _, _, _, _, _, _, _) ts1 ts2 dt =
  let supp1   = unionMaps supp ts1
      supp2   = unionMaps supp ts2
      dom1    = unionMaps dom ts1 
      dom2    = unionMaps dom ts2
      xordt   = Map.findWithDefault False dt xor
      suppxor = if xordt
                then Just (Map.union (Map.difference supp1 supp2) (Map.difference supp2 supp1))
                else Nothing
      is1s2  = Map.intersection supp1 supp2
      is1s2null = Map.null is1s2
      is1s2norand = not $ intersectRand2 is1s2 init
      -- is1s2  = Map.null $ Map.intersection supp1 supp2
      eqs1s2 = (Map.null $ Map.difference supp1 supp2) && (Map.null $ Map.difference supp2 supp1)
      eqd1d2 = (Map.null $ Map.difference dom1 dom2) && (Map.null $ Map.difference dom2 dom1)
      diffd1s2 = Map.null $ Map.difference dom1 supp2
      diffd2s1 = Map.null $ Map.difference dom2 supp1
      diffd1d2 = Map.null $ Map.difference dom1 dom2
      diffd2d1 = Map.null $ Map.difference dom2 dom1
      typs1  = map (\tid -> Map.lookup tid pmap) ts1
      typs2  = map (\tid -> Map.lookup tid pmap) ts2
      typ1   = mergeTypes typs1
      typ2   = mergeTypes typs2
  in case (typ1, typ2, is1s2null, eqs1s2, eqd1d2, diffd1s2, diffd2s1, diffd1d2, diffd2d1, suppxor, is1s2norand) of
    (Just (Public _), Nothing, _, _, _, _, _, _, _, _, _) -> -- a copy
      Map.insert dt (Public dt) pmap    
    (Nothing, Just (Public _), _, _, _, _, _, _, _, _, _) -> -- a copy
      Map.insert dt (Public dt) pmap
    -- (Just (Random _), Just (Random _), _, _, _, False, _, _) | not isxor -> 
    --                                                            Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Public _), True, _, _, _, _, _, _, _, _) ->
      Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Random _), True, _, _, _, _, _, _, _, _) | not isxor && not isgmul -> 
                                                           Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Public _), True, _, _, _, _, _, _, _, _) | not isxor && not isgmul -> 
                                                           Map.insert dt (Public dt) pmap
    (Just (Random _), _, _, True, True, True, _, _, _, _, _) -> Map.insert dt (Public dt) pmap
    (_, Just (Random _), _, True, True, _, True, _, _, _, _) -> Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, False, _, _, _, _, _) | not isxor && not isgmul -> 
                                                            Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, _, False, _, _, _, _) | not isxor && not isgmul -> 
                                                            Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Public _), _, _, _, False, _, _, _, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Random _), _, _, _, _, False, _, _, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, _, _, False, _, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, _, _, _, False, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (_, _, _, _, _, _, _, _, _, Just d, _) | not $ intersectSec2 d init ->
                                    Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Public _), _, _, _, _, _, _, _, _, True) ->
      Map.insert dt (Public dt) pmap
    (_, _, _, _, _, _, _, _, _, _, _) -> Map.insert dt (Secret dt) pmap


-- updateNewPmap types pmap' =
--   let (_, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args) = types
--   in (pmap', init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args)
-- to complete

-- fromJust (Just x) = x
-- fromJust Nothing = error "SecTypeInf: Unexpected Nothing" 

mergeTypes [] = Nothing
mergeTypes (Nothing:rest) = mergeTypes rest
mergeTypes ((Just h):rest) = Just (mergeTypes1 rest h) 

mergeTypes1 [] t = t 
mergeTypes1 ((Just (Secret a)):_) _ = Secret a
mergeTypes1 ((Just (Public _)):_) (Secret b) = Secret b 
mergeTypes1 ((Just (Public a)):rest) _ = mergeTypes1 rest (Public a)
mergeTypes1 ((Just (Random a)):rest) (Random _) = mergeTypes1 rest (Random a)
mergeTypes1 ((Just (Random _)):rest) t = mergeTypes1 rest t
mergeTypes1 (Nothing:rest) t = mergeTypes1 rest t 


-- isJustRandom (Just (Random _)) = True
-- isJustRandom _ = False

unionMaps mp sts = 
  let f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
  in foldl (f mp) Map.empty sts

intersectSec m1 m2 t =
  let ds = Map.findWithDefault Map.empty t m1 
  in any (\di -> isMaybeSecret $ Map.lookup di m2 ) (map fst $ Map.toList ds)

intersectSec2 ds m2 =
  any (\di -> isMaybeSecret $ Map.lookup di m2 ) (map fst $ Map.toList ds)


isMaybeSecret (Just (Secret _)) = True
isMaybeSecret _ = False

fromJustSecret (Just (a @ (Secret _))) = a
fromJustSecret _ = error "FromJustSecret: This should not happen"


intersectRand2 ds m2 =
  any (\di -> isMaybeRandom $ Map.lookup di m2 ) (map fst $ Map.toList ds)
  
isMaybeRandom (Just (Random _)) = True
isMaybeRandom _ = False

isEmpty t mp =
  case Map.lookup t mp of
    Just d | Map.null d -> True
    Just _              -> False
    Nothing             -> error "SecTypeInf: isEmpty - tid should be in dom"

getTids [] tids = tids
getTids ((Temporary {tId = tid}):ts) tids = getTids ts (("t" ++ show tid):tids)
getTids (NullTemporary:ts) tids = getTids ts tids
getTids ((MOperand {altTemps = temps}):ts) tids = getTids (temps ++ ts) tids
getTids (Bound (MachineFrameIndex {mfiIndex = mi,
                                   mfiFixed = isfixed}):ts) tids =
         getTids ts ((addPrefix isfixed ++ show mi):tids)
getTids (_:ts) tids = getTids ts tids


splitOps_i [] acc = acc
splitOps_i (t@(Temporary {}):ts) (tbv, tbb) = splitOps_i ts (t:tbv, tbb)
splitOps_i (t@(MOperand {}):ts) (tbv, tbb) = splitOps_i ts (t:tbv, tbb)
splitOps_i (t@(BlockRef {}):ts) (tbv, tbb) = splitOps_i ts (tbv, t:tbb) 
splitOps_i (_:ts) acc = splitOps_i ts acc


splitOps tbs = splitOps_i tbs ([],[])

fJust (Just v) = v
fJust Nothing = error "fJust: SecurityTypeInference.hs: This should not be Nothing."

-- For debugging
-- fJust2 _ (Just v) = v
-- fJust2 a Nothing = error (show a ++ "fJust: SecurityTypeInference.hs: This should not be Nothing.")
