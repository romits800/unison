{-|
Copyright   :  Copyright (c) 2021, Rodothea Myrsini Tsoupidi
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
-}

module Unison.Transformations.SecurityTypeInference (inferSecurityTypes,
                                                     isMaybeSecret,
                                                     updateBSupps,
                                                     updateBUnqs,
                                                     updateBDoms,
                                                     updatePmapID,
                                                     StateTuple (..))
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
  deriving (Show, Eq, Ord)

-- TODO
data StateTuple r = StateTuple {
  fPmap  :: Map String (Policy String),          -- pmap
  fInmap :: Map String (Policy String),          -- inmap
  fSupp  :: Map String (Map String String),
  fUnq   :: Map String (Map String String),
  fDom   :: Map String (Map String String),
  fXor   :: Map String Bool,
  fM2o   :: Map String (Map Integer [String]),
  fC2o   :: Map String (Map Integer [String]),
  fP2p   :: Map Integer [Integer],
  fP2t   :: Map Integer [Operand r],
  fArgs  :: Map String (KnownOperations, [String], [String]),
  fBbs   :: Map (Integer, Integer) (Policy String),
  fFlag  :: FlagsType
  }

data FlagsType = FlagsType {
  isFixedPoint :: Bool,
  hasPhis :: Bool
  } deriving (Eq, Ord)

-- type StateTuple r = (Map String (Policy String),          -- pmap
--                      Map String (Policy String),
--                      Map String (Map String String),
--                      Map String (Map String String),
--                      Map String (Map String String),
--                      Map String Bool,                     -- xor
--                      Map String (Map Integer [String]),   -- m2o
--                      Map String (Map Integer [String]),   -- c2o
--                      Map Integer [Integer],               -- p2p
--                      Map Integer [Operand r],
--                      Map String (KnownOperations, [String], [String]),
--                      Map (Integer, Integer) (Policy String),
--                      FlagsType)


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
      
      t               = StateTuple {fPmap = pmap, fInmap = inmap, fSupp = supp, fUnq = unq,
                                    fDom = dom,   fXor = xor,     fM2o = m2o,   fC2o  = c2o,
                                    fP2p = p2p,   fP2t = p2t,     fArgs = args, fBbs = bbs,
                                    fFlag = flag}
      -- t               = (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag)
      t'              = inferSecurityTypesI target t f
      -- t''             = inferTypes target t' f
  in t'


inferSecurityTypesI target t f =
  case inferTypes target t f of
    types @ StateTuple {fFlag = FlagsType { hasPhis = True }} -> 
      let types' =  types {fFlag = FlagsType { isFixedPoint = True, hasPhis = False }}
      in inferSecurityTypesI target types' f
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
        let --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
            isxor = False
            isgmul= False
            ts1 = getTids [t1] []
            ts2 = getTids [t2] []
            xor' = updateXorD (fXor types) ts1 ts2 isxor [d]
            args' = updateArgsD (fArgs types) KOOther ts1 ts2 [d]
            supp' = updateBSuppsD isxor (fSupp types, xor', args') ts1 ts2 [d]
            unq' = updateBUnqsD (fUnq types, fSupp types) ts1 ts2 [d]
            -- if is xor
            dom' = updateBDomsEmptyD (fDom types) [d]
            types' = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
            pmap' = updatePmapID isxor isgmul types' ts1 ts2 d
            typ  = Map.lookup "brtemp" pmap'
            bbs' = 
              if isMaybeSecret typ then Map.insert (blid, bid+1) (fromJustSecret typ) (fBbs types)
              else fBbs types
            types'' = types {fBbs = bbs'} 
        in inferTypesOperation target f bid types'' codes
      (t1:[], [BlockRef {blockRefId=blid}])    ->
        let --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
            ts1 = getTids [t1] []
            supp' = updateSuppsD (fSupp types) ts1 [d]
            unq'  = updateUnqsD (fUnq types) ts1 [d]
            dom'  = updateDomsD (fDom types) ts1 [d]
            xor' = updateXorD (fXor types) ts1 [] True [d]
            args' = updateArgsUopD (fArgs types)  ts1 [d]
            types' = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
            pmap' = updatePmapID False False types' ts1 [] d
            typ  = Map.lookup "brtemp" pmap'
            bbs' = 
              if isMaybeSecret typ then Map.insert (blid, bid+1) (fromJustSecret typ) (fBbs types)
              else fBbs types
            types'' = types {fBbs = bbs'} 
        in inferTypesOperation target f bid types'' codes
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
    isxor = any (isXor target) i
    isgmul= any (isGMul target) i
    xor' = updateXor (fXor types) ts1 ts2 isxor [d]
    args' = updateArgs (fArgs types) (knownOp target i) ts1 ts2 [d]
    supp' = updateBSupps isxor (fSupp types, xor', args') ts1 ts2 [d]
    unq' = updateBUnqs (fUnq types, fSupp types) ts1 ts2 [d]
    -- if is xor
    dom' = if isxor
           then updateBDoms (fDom types, unq') ts1 ts2 [d]
           else updateBDomsEmpty (fDom types) [d]
    types'  = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
    pmap'   = updatePmaps isxor isgmul types' ts1 ts2 [d]
    types'' = types' {fPmap = pmap'} 
  in inferTypesOperation target f bid types'' codes
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
    --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    -- if is xor
    isxor = any (isXor target) i
    isgmul= any (isGMul target) i
    xor' = updateXor (fXor types) [t1] [t2] isxor [d]
    args' = updateArgs (fArgs types) (knownOp target i) [t1] [t2] [d]
    supp' = updateBSupps isxor (fSupp types, xor', args') [t1] [t2] [d]
    unq' = updateBUnqs (fUnq types, fSupp types) [t1] [t2] [d]
    dom' = if isxor
           then updateBDoms (fDom types, unq') [t1] [t2] [d]
           else updateBDomsEmpty (fDom types) [d]
    types'  = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
    pmap' = updatePmaps isxor isgmul types' [t1] [t2] [d]
    types'' = types' {fPmap = pmap'}
    --(pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- store operation
-- TODO update xor
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} :
                            mfi @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                             mfiFixed = isfixed})) : _,
                      oDs = [] }}}:codes) =
  let
    --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) ts [mfi]
    unq'  = updateUnqs (fUnq types) ts [mfi]
    dom'  = updateDoms (fDom types) ts [mfi]
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    m2o' = updateM2o (fM2o types) (addPrefix isfixed ++ show mf) oid ts
    args' = updateArgsUop (fArgs types)  ts [mfi]
    types'  = types { fSupp = supp', fUnq = unq', fDom = dom', fM2o = m2o', fArgs = args'} 
    pmap' = updatePmaps isxor isgmul types' ts [] [mfi]
    types'' = types' {fPmap = pmap'}
    --(pmap', init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
  -- TODO update xor
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = t @ Temporary {} :
                            mfi @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                             mfiFixed = isfixed})) : _,
                      oDs = [] }}}:codes) =
  let
    --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) [t] [mfi]
    unq'  = updateUnqs (fUnq types) [t] [mfi]
    dom'  = updateDoms (fDom types) [t] [mfi]
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    m2o' = updateM2o (fM2o types) (addPrefix isfixed ++ show mf) oid []
    args' = updateArgsUop (fArgs types)  [t] [mfi]
    types'  = types { fSupp = supp', fUnq = unq', fDom = dom', fM2o = m2o', fArgs = args'} 
    pmap' = updatePmaps isxor isgmul types' [t] [] [mfi]
    types'' = types { fPmap = pmap'}
    -- (pmap', init, supp', unq', dom', xor, m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- operation with one operand
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} : _,
                      oDs = defops }}}:codes) =
  -- if join of uops is...
  -- add defops to the map
  let
    --(pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) ts defops
    unq'  = updateUnqs (fUnq types) ts defops
    dom'  = updateDoms (fDom types) ts defops
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    xor' = updateXor (fXor types) ts [] True defops
    args' = updateArgsUop (fArgs types)  ts defops
    types'  = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
    pmap' = updatePmaps isxor isgmul types' ts [] defops 
    types'' = types' {fPmap = pmap'}
    --(pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = t @ Temporary {} : _,
                      oDs = defops }}}:codes) =
  -- if join of uops is...
  -- add defops to the map
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) [t] defops
    unq'  = updateUnqs (fUnq types) [t] defops
    dom'  = updateDoms (fDom types) [t] defops
    isxor = any (isXor target) i
    isgmul = any (isGMul target) i
    xor' = updateXor (fXor types) [t] [] True defops
    args' = updateArgsUop (fArgs types)  [t] defops
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'}
    pmap' = updatePmaps isxor isgmul types' [t] [] defops
    types'' = types' {fPmap = pmap'}
    -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
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
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) [u] d
    unq'  = updateUnqs (fUnq types) [u] d
    dom'  = updateDoms (fDom types) [u] d
    isxor = False
    isgmul= False
    xor' = updateXor (fXor types) d [] True d
    args' = updateArgsUop (fArgs types)  [u] d
    m2o' = updateM2o (fM2o types) (addPrefix isfixed ++ show mf) oid d
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args', fM2o = m2o'}
    -- pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag) [u] [] d
    pmap' = updatePmaps isxor isgmul types' [u] [] d
    types'' = types' {fPmap = pmap'}
    -- types' = (pmap', init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
inferTypesOperation target f bid types (SingleOperation
  {
    oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (u @ (Bound (MachineFrameIndex {mfiIndex = mf,
                                                            mfiFixed = isfixed}))) : _,
                      oDs = [t @ Temporary {}] }}}:codes) =
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) [u] [t]
    unq'  = updateUnqs (fUnq types) [u] [t]
    dom'  = updateDoms (fDom types) [u] [t]
    isxor = False
    isgmul= False
    xor' = updateXor (fXor types) [t] [] True [t]
    args' = updateArgsUop (fArgs types)  [u] [t]
    m2o' = updateM2o (fM2o types) (addPrefix isfixed ++ show mf) oid []
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args', fM2o = m2o'}
    pmap' = updatePmaps isxor isgmul types' [u] [] [t]
    types'' = types' {fPmap = pmap'}
    -- pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag) [u] [] [t]
    -- types' = (pmap', init, supp', unq', dom', xor', m2o', c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- operation with one static operand
inferTypesOperation target f bid types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = Bound (MachineImm {}) : _,
                      oDs = d:_ }}}:codes) = 
  -- if join of uops is...
  -- add defops to the map
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp'  = updateSupps (fSupp types) [] [d]
    unq'   = updateUnqs (fUnq types) [] [d]
    dom'   = updateDoms (fDom types) [] [d]
    isxor  = False
    isgmul = False
    xor'   = updateXor (fXor types) [] [] True [d]
    args'  = updateArgsUop (fArgs types)  [] [d]
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'}
    pmap'  = updatePmaps isxor isgmul types' [] [] [d]
    types'' = types' {fPmap = pmap'}
    -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- Special case for some MIPS instruction  
inferTypesOperation _ _ _ types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = [],
                      oDs = [] }}}:_) = types
-- TODO: Check this again
inferTypesOperation _ _ _ types (SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = [_],
                      oDs = [_] }}}:_) = types
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
  let -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
      supp' = updateSupps (fSupp types) (sop:uops) (dop:defops)
      unq'  = updateUnqs (fUnq types) (sop:uops) (dop:defops)
      dom'  = updateDoms (fDom types) (sop:uops) (dop:defops)
      isxor = False
      isgmul= False
      xor' = updateXor (fXor types) (sop:uops) [] True (dop:defops)
      args' = updateArgsUop (fArgs types)  (sop:uops) (dop:defops)
      -- take only destinations - dests and sources are the same..
      c2o' = updateC2o (fC2o types) (dop:defops) oid (sop:uops) 
      types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args', fC2o = c2o'}
      pmap' = updatePmaps isxor isgmul types' (sop:uops) [] (dop:defops)
      types'' = types' {fPmap = pmap'}
      -- pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag) (sop:uops) [] (dop:defops)
      -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- virtual copies
inferTypesOperation target f bid types (SingleOperation
  { oId  = oid, 
    oOpr = Virtual (VirtualCopy { oVirtualCopyS = s,
                                  oVirtualCopyD = d })}:codes) =
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
      supp' = updateSupps (fSupp types) [s] [d]
      unq'  = updateUnqs (fUnq types) [s] [d]
      dom'  = updateDoms (fDom types) [s] [d]
      xor'  = updateXor (fXor types) [s] [] True [d]
      args' = updateArgsUop (fArgs types)  [s] [d]
        -- take only destinations - dests and sources are the same..
      c2o'  = updateC2o (fC2o types) [d] oid [s]
      types'  = types {fSupp = supp', fUnq = unq', fDom = dom',
                       fXor = xor', fArgs = args', fC2o = c2o'}
      pmap' = updatePmaps False False types' [s] [] [d]
      types'' = types' {fPmap = pmap'}
      -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t, args', bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- -- TODO(VirtualOperation): Phi/Delimiter/Kill/Define
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Fun  { oFunctionUs = oFus,
                            oFunctionDs = oFds
                          }
                   )}:codes) =
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) oFus oFds -- assume no side effects in function
    -- This will anyways be False
    xor' = updateXor (fXor types) oFus [] False oFds
    -- args' = updateArgs args KOOther ts1 ts2 [d]
    -- supp' = updateBSupps isxor (supp, xor', args') ts1 ts2 [d]
    unq' = updateLUnqs ((fUnq types), (fSupp types)) oFus oFds
    -- if is xor
    dom' = updateBDomsEmpty (fDom types) oFds
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor'}
    pmap' = updateLPmaps types' oFus oFds
    -- pmap' = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oFus oFds
    types'' = types' {fPmap = pmap'}
    -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag)
  in inferTypesOperation target f bid types'' codes
-- Phi (TODO)
inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Phi  { oPhiUs = oFus,
                            oPhiD = oFd
                          }
                   )}:codes) =
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    supp' = updateSupps (fSupp types) oFus [oFd] -- assume no side effects in function
    -- This will anyways be False
    xor' = updateXor (fXor types) oFus [] False [oFd]
    -- args' = updateArgs args KOOther ts1 ts2 [d]
    -- supp' = updateBSupps isxor (supp, xor', args') ts1 ts2 [d]
    unq' = updateLUnqs (fUnq types, fSupp types) oFus [oFd]
    -- if is xor
    dom' = updateBDomsEmpty (fDom types) [oFd]
    types'  = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor'}
    -- pmap' = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oFus [oFd]
    pmap' = updateLPmaps types' oFus [oFd]
    types'' = types' {fPmap = pmap'}
    -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag)
  in inferTypesOperation target f bid types'' codes

inferTypesOperation target f bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (In {
                                   oIns = oins
                                   })
                   )}:codes) | all isMOperand oins = 
  let
    -- (_, _, _, _, _, _, _, _, p2p, p2t, _, _, _) = types
    ins = map getOpidTemps oins
    types' = foldl (updateNewTemps (fP2t types) (fP2p types)) types ins
  in inferTypesOperation target f bid types' codes
inferTypesOperation target f @ Function {fCode = code} bid types (SingleOperation
  { oId = _,
    oOpr = Virtual ( Delimiter (Out {
                                   oOuts = oouts
                                   })
                   )}:codes) | all isMOperand oouts = 
  let
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args, bbs, flag) = types
    p2t' = foldl (\p2t'' -> \(p,ts) -> Map.insert p ts p2t'') (fP2t types) $ map getOpidTemps oouts
    init' = if (fromIntegral (length(code))-1 == bid) then insertOutTemps (fPmap types) (fInmap types) oouts else (fInmap types)
    types'  = types {fInmap = init', fP2t = p2t'}
    -- types' = (pmap, init', supp, unq, dom, xor, m2o, c2o, p2p, p2t', args, bbs, flag)
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
    -- (pmap, init, supp, unq, dom, xor, m2o, c2o, _, _, args, bbs, flag) = types
    adj = Map.lookup oid p2p
    -- in case of a phi
    f oldtmps isAnyNothing tmps =
      let
        supp'  = updateSupps (fSupp types) oldtmps tmps 
        xor'   = updateXor (fXor types) oldtmps [] True tmps
        -- args' = updateArgs args KOOther ts1 ts2 [d]
        unq'   = updateLUnqs (fUnq types, fSupp types) oldtmps tmps
        dom'   = updateBDomsEmpty (fDom types) tmps
        types' = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor'}
        pmap'  = updateLPmaps types' oldtmps tmps
        -- pmap'  = updateLPmaps (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag) oldtmps tmps
        flag' = (fFlag types) {hasPhis = isAnyNothing}
        types'' = types' {fPmap = pmap', fFlag = flag'}
        -- types' = (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args, bbs, flag')
      in types''
    f2 oldtmps tmps = 
      let 
        supp' = updateSupps (fSupp types) oldtmps tmps
        unq'  = updateUnqs (fUnq types) oldtmps tmps
        dom'  = updateDoms (fDom types) oldtmps tmps
        xor'  = updateXor (fXor types) oldtmps [] True tmps
        args' = updateArgsUop (fArgs types)  oldtmps tmps
        types' = types {fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'}
        pmap' = updatePmaps False False types' oldtmps [] tmps
        -- pmap' = updatePmaps False False (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag) oldtmps [] tmps
        types'' = types' {fPmap = pmap'}
      in types'' --(pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
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

updateLPmaps types _ [] = fPmap types
updateLPmaps types ts dts =
  let stids = getTids ts []  -- sources
      dtids = getTids dts []              -- destinations
      pmap  = fPmap types
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


updatePmaps _ _ types _ _ [] = fPmap types
updatePmaps isxor isgmul types ts1 ts2 dts =
  let stids1 = getTids ts1 [] -- sources1
      stids2 = getTids ts2 [] -- sources2
      dtids  = getTids dts [] -- destinations
  in updatePmapsID isxor isgmul types stids1 stids2 dtids

updatePmapsID _ _ types _ _ [] = fPmap types
updatePmapsID isxor isgmul types ts1 ts2 (dt:dts) =
  let pmap' = updatePmapID isxor isgmul types ts1 ts2 dt
  in updatePmapsID isxor isgmul (types {fPmap = pmap'}) ts1 ts2 dts
  
updatePmapID _ _ types _ _ dt
  | not $ isEmpty dt (fDom types) =
    Map.insert dt (Random dt) (fPmap types)
updatePmapID _ _ types _ _ dt
  | (isEmpty dt (fDom types)) && (not $ intersectSec (fSupp types) (fInmap types) dt)  =
    Map.insert dt (Public dt) (fPmap types)    
updatePmapID True _ types ts1 ts2 dt
  | isExactlySameOperand (fArgs types) ts1 ts2 = Map.insert dt (Public dt) (fPmap types)
updatePmapID True isgmul types ts1 ts2 dt
  | (isSameOperand (fArgs types) ts1 ts2 || isSameOperand (fArgs types) ts2 ts1) =
    case getSameOperand (fArgs types) ts1 ts2 of
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
            pmap   = fPmap types
        in
          case (typ1,typ2) of
            (Just (Secret _), _) -> Map.insert dt (Secret dt) pmap
            (_,Just (Secret _)) -> Map.insert dt (Secret dt) pmap
            _ -> Map.insert dt (Public dt) pmap
updatePmapID True _ types ts1 ts2 dt
  | isDistributive (fArgs types) ts1 ts2 =
    case getDistributiveOperands (fArgs types) ts1 ts2 of
      Nothing -> error "updatePmapID: Should not happen!"
      Just (ts1, ts2, ts3) -> 
        let
          isxor = True
          xor' = updateXorD (fXor types) ts2 ts3 isxor ["dist"]
          args' = updateArgsD (fArgs types) KOXor ts2 ts3 ["dist"]
          supp' = updateBSuppsD isxor (fSupp types, xor', args') ts2 ts3 ["dist"]
          unq' = updateBUnqsD (fUnq types, fSupp types) ts2 ts3 ["dist"]
          dom' = updateBDomsD (fDom types, unq') ts2 ts3 ["dist"]
          types' = types { fSupp = supp', fUnq = unq', fDom = dom', fXor = xor', fArgs = args'} 
          -- types' = (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args', bbs, flag)
          pmap' = updatePmapID True False types' ts2 ts3 "dist"
          
          xor'' = updateXorD xor' ts1 ["dist"] False [dt]
          args'' = updateArgsD args' KOGmul ts1 ["dist"] [dt]
          supp'' = updateBSuppsD False (supp', xor'', args'') ts1 ["dist"] [dt]
          unq'' = updateBUnqsD (unq', supp') ts1 ["dist"] [dt]
          dom'' = updateBDomsD (dom', unq'') ts1 ["dist"] [dt]
          types'' = types { fSupp = supp'', fUnq = unq'', fDom = dom'',
                            fXor = xor'', fArgs = args'', fPmap = pmap'} 
          -- types'' = (pmap', init, supp'', unq'', dom'', xor'', m2o, c2o, p2p, p2t, args'', bbs, flag)
        in updatePmapID False True types'' ts1 ["dist"] dt
updatePmapID isxor isgmul types ts1 ts2 dt =
  let supp1   = unionMaps (fSupp types) ts1
      supp2   = unionMaps (fSupp types) ts2
      dom1    = unionMaps (fDom types) ts1 
      dom2    = unionMaps (fDom types) ts2
      pmap    = fPmap types
      init    = fInmap types
      xordt   = Map.findWithDefault False dt (fXor types)
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

-- fpmap types = fPmap types
-- fm2o types = fM2o types
-- finmap types = fInmap types

-- fc2o  types = fC2o types
-- fbbs  types = fBbs types
-- fsupp types = fSupp types
-- fdom  types = fDom types
-- fxor  types = fXor types
-- funq  types = fUnq types


-- For debugging
-- fJust2 _ (Just v) = v
-- fJust2 a Nothing = error (show a ++ "fJust: SecurityTypeInference.hs: This should not be Nothing.")
