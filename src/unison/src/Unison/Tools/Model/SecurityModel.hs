{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>

This file is part of DivCon
-}
module Unison.Tools.Model.SecurityModel (parameters) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Aeson (toJSON)

import Unison
import Unison.Target.API
-- import Unison.Target.Query


import Unison.Tools.Model.Definitions()
import Unison.Tools.Model.ParseSecurityPolicies

import MachineIR

import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.BCFG as BCFG


type StateTuple r = (Map String (Policy String), Map String (Policy String),
                     Map String (Map String String),
                     Map String (Map String String),
                     Map String (Map String String),
                     Map String Bool,
                     Map String (Map Integer Integer),
                     Map String (Map Integer Integer),
                     Map Integer Integer,
                     Map Integer [Operand r])
--                   Map String (Map Integer Integer))


parameters (_,_,_,_,ra,_) target f @ Function {fCode = _} policies =
  let inpol           = showPolicies ([],[],[]) policies
      pmap            = createPolicyMap policies Map.empty -- try to find a way to copy
      inmap           = createPolicyMap policies Map.empty
      supp            = createTSetMap policies Map.empty
      unq             = createRandomTSetMap policies Map.empty
      dom             = createRandomTSetMap policies Map.empty
      xor             = createXorMap policies Map.empty
      m2o             = Map.empty -- mem to operations
      c2o             = Map.empty -- copies to operations

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

      t               = (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t)
      nt @ (pmap',_,supp',unq',dom',xor',m2o',c2o', _, p2t') = inferTypes target t f
      
      (sec,pub,ran)   = splitTemps (Map.toAscList pmap') ([],[],[])
      ran'            = filter (\x -> head x == 't') ran
      sec'            = filter (\x -> head x == 't') sec
      sec''           = filter (\x -> head x == 'F') sec  -- memory secrets
      ran''           = filter (\x -> head x == 'F' || head x == 't') ran  -- memory randoms

      -- Parameters
      pairs           = findPairs ran' nt []
      secdom          = findRandSec sec' ran' nt []
      p2o             = Map.union c2o' m2o'
      secdommem       = findRandSecMC sec'' ran'' nt p2o [] 
      hr              = map (mkRegister . mkTargetRegister) $ hardwareRegisters target
      hregs           = concatMap (\x -> Map.findWithDefault [] x $ regAtoms ra) hr
      
  in
    [
      ("Types",     toJSON $ [sec,pub,ran]),
      -- Security parameters
      ("pairs",     toJSON $ map toInt pairs), -- pairs of random vars that should not reside in the same register
      -- todo(Romy): is it enough with random x random or should I check public x random
      ("spairs",    toJSON $ map toInt2 secdom), -- secret vars that should be preceeded by a random variable in the same register.
      ("mpairs",    toJSON secdommem), -- secret memory - operations
      ("adj2",      toJSON $ Map.toList p2p), -- secret memory - operations
      ("adj25",     toJSON adjacent), -- secret memory - operations
      ("adj3",      toJSON $ Map.toList p2t'), -- secret memory - operations
      ("HR",        toJSON hregs)
    ]

splitTemps [] res = res
splitTemps ((k, Secret _):ts) (s,p,r) = splitTemps ts (k:s, p, r)
splitTemps ((k, Random _):ts) (s,p,r) = splitTemps ts (s, p, k:r)
splitTemps ((k, Public _):ts) (s,p,r) = splitTemps ts (s, k:p, r)

-- debug
printPmap (_, Secret t) = t ++ ": " ++ "Secret"
printPmap (_, Random t) = t ++ ": " ++ "Random"
printPmap (_, Public t) = t ++ ": " ++ "Public"

toInt ('t':x,'t':y) = (read x:: Integer, read y::Integer) 
toInt (t1, t2)      = error $ "SecTypeInf: toInt cannot convert to integer: " ++ t1 ++ " " ++ t2


toInt2 (['t':x], rs) = ([read x:: Integer], map toInt1 rs)
toInt2 ([s], _) = error $ "SecTypeInf: toInt2 cannot convert to integer: " ++ s
toInt2 _ = error "SecTypeInf: toInt2 unexpected argument."

toInt1 ('t':x) = read x:: Integer
toInt1 t       = error $ "SecTypeInf: toInt cannot convert to integer: " ++ t

printMap (_, m) = map fst $ Map.toAscList m

  
showPolicies:: ([String], [String], [String]) -> [Policy String] -> ([String], [String], [String])
showPolicies res (p:ps) =
  let
    res' = showPolicy res p
  in showPolicies res' ps
showPolicies res [] = res


showPolicy:: ([String], [String], [String]) -> Policy String -> ([String], [String], [String])
showPolicy (sec, pub, rand) (Secret a) = (a:sec, pub, rand)
showPolicy (sec, pub, rand) (Public a) = (sec, a:pub, rand)
showPolicy (sec, pub, rand) (Random a) = (sec, pub, a:rand)



createPolicyMap::  [Policy String] -> Map String (Policy String) -> Map String (Policy String)
createPolicyMap [] m = m
createPolicyMap (p @ (Secret a):ps) m =
  createPolicyMap ps $ Map.insert a p m
createPolicyMap (p @ (Public a):ps) m =
  createPolicyMap ps $ Map.insert a p m
createPolicyMap (p @ (Random a):ps) m =
  createPolicyMap ps $ Map.insert a p m


createTSetMap::  [Policy String] -> Map String (Map String String) -> Map String (Map String String)
createTSetMap [] m = m
createTSetMap ((Secret a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m 
createTSetMap ((Public a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m
createTSetMap ((Random a):ps) m =
  createTSetMap ps $ Map.insert a (Map.insert a a Map.empty) m

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


createP2p:: Show r => [(Operand r, Operand r)] -> Map Integer Integer -> Map Integer Integer
createP2p [] mp = mp
createP2p ops mp = foldl (\mp' -> \op -> createP2pi op mp') mp ops


createP2pi::Show r => (Operand r, Operand r) -> Map Integer Integer -> Map Integer Integer
createP2pi (o1, o2) mp =
  let oid1 = getOid o1
      oid2 = getOid o2
  in Map.insert oid2 oid1 mp


getOid OperandRef {operandRefId = orid}  = orid
getOid op = error $ "GetOid failed: " ++ show op


getOpidTemps MOperand {operandId = oid,
                       altTemps = ts} = (oid, ts)
getOpidTemps op = error $ "GetOidTemps failed: " ++ show op


inferTypes:: Show r => Show i => (TargetDescription i r0 rc0 s0, TargetOptions) -> StateTuple r -> Function i r -> StateTuple r 
inferTypes target types f @ Function {fCode = code} =
  foldl (inferTypesBlock target f) types code
  


inferTypesBlock:: Show r => Show i => (TargetDescription i r0 rc0 s0, TargetOptions) -> Function i r -> StateTuple r -> Block i r -> StateTuple r
inferTypesBlock target f types Block {bCode = code} =
  foldl (inferTypesOperation target f) types code

  
-- TODO:
-- Create a Map for policies with tempid
-- Check what to do with the phis
inferTypesOperation :: Show r => Show i => (TargetDescription i r0 rc0 s0, TargetOptions) -> Function i r -> StateTuple r -> BlockOperation i r -> StateTuple r
inferTypesOperation target f types Bundle {bundleOs = bs } =
  foldl (inferTypesOperation target f) types bs
-- Normal Operations
inferTypesOperation target _ types SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = (MOperand {altTemps = ts1}) :
                            (MOperand {altTemps = ts2}) : _,
                      -- TODO(fix): trying to avoid the control register 
                      oDs = d:_ }}} =
  let
    -- exception for control register???
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    supp' = updateSupps supp (ts1 ++ ts2) [d]
    unq' = updateBUnqs (unq, supp) ts1 ts2 [d]
    -- if is xor
    isxor = any (isXor target) i
    isgmul= False
    dom' = if isxor
           then updateBDoms (dom, unq') ts1 ts2 [d]
           else updateBDomsEmpty dom [d]
    xor' = if isxor
           then updateXor xor ts1 ts2 True [d]
           else updateXor xor ts1 ts2 False [d]
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t) ts1 ts2 [d]
  in (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t)
-- store operation
inferTypesOperation target _ types SingleOperation
  { oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} :
                            mfi @ (Bound (MachineFrameIndex {mfiIndex = mf})) : _,
                      oDs = [] }}} =
  -- if join of uops is...
  -- add defops to the map
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    supp' = updateSupps supp ts [mfi]
    unq'  = updateUnqs unq ts [mfi]
    dom'  = updateDoms dom ts [mfi]
    isxor = any (isXor target) i
    isgmul= False
    m2o' = updateM2o m2o ("F" ++ show mf) oid
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor, m2o', c2o, p2p, p2t) ts [] [mfi]
  in (pmap', init, supp', unq', dom', xor, m2o', c2o, p2p, p2t)
-- operation with one operand
inferTypesOperation target _ types SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = i, -- Instruction i
                      oUs = MOperand {altTemps = ts} : _,
                      oDs = defops }}} =
  -- if join of uops is...
  -- add defops to the map
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    supp' = updateSupps supp ts defops
    unq'  = updateUnqs unq ts defops
    dom'  = updateDoms dom ts defops
    isxor = any (isXor target) i
    isgmul= False
    xor' = updateXor xor ts [] True defops
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t) ts [] defops 
  in (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t)
-- stack operations
inferTypesOperation _ _ types SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (Bound (_)) : [],
                      oDs = [] }}} =
  types
-- load operation
inferTypesOperation _ _ types SingleOperation
  {
    oId  = oid, 
    oOpr = Natural {oNatural = Linear {
                      oIs = _, -- Instruction i
                      oUs = (u @ (Bound (MachineFrameIndex {mfiIndex = mf}))) : _,
                      oDs = [MOperand {altTemps = d}] }}} =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    supp' = updateSupps supp [u] d
    unq'  = updateUnqs unq [u] d
    dom'  = updateDoms dom [u] d
    isxor = False
    isgmul= False
    xor' = updateXor xor d [] True d
    m2o' = updateM2o m2o ("F" ++ show mf) oid
    pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o', c2o, p2p, p2t) [u] [] d
  in (pmap', init, supp', unq', dom', xor', m2o', c2o, p2p, p2t)
inferTypesOperation _ _ _ SingleOperation
  {oOpr = Natural {oNatural = Linear {
                      oIs = ins, -- Instruction i
                      oUs = ous,
                      oDs = defops }}} =
  -- if join of uops is...
  -- add defops to the map
  error ("SecTypeInf: Operation with unknown operands: " ++
         "Len ins:" ++ (show $ length ins) ++ " " ++
         "Len oups:" ++ (show $ length ous) ++ " " ++
         "Len defops:" ++ (show $ length defops) ++ " " ++
         foldl (\s -> \op -> s ++ " " ++ (show op)) "Use Operands: " ous ++
         foldl (\s -> \op -> s ++ " " ++ (show op)) "Def Operands: " defops ++
         foldl (\s -> \i  -> s ++ " " ++ (show i))  "Instructions: " ins)
inferTypesOperation _ _ types SingleOperation
  {oOpr = Natural {oNatural = Branch {
                      oBranchIs = _, -- instruction i
                      oBranchUs = _ }}} = types
inferTypesOperation _ _ types SingleOperation
  {oOpr = Natural {oNatural = Call {
                      oCallIs = _, -- Instruction i
                      oCallUs = _ }}} = types
inferTypesOperation _ _ types SingleOperation
  {oOpr = Natural {oNatural = TailCall {
                      oTailCallIs = _, -- Instruction i
                      oTailCallUs = _ }}} = types
-- copies
inferTypesOperation _ _ types SingleOperation
  { oId  = oid, 
    oOpr = Copy { oCopyIs = _,
                  oCopyS = sop,
                  oCopyUs = uops,
                  oCopyD = dop,
                  oCopyDs = defops }} =
  let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
      supp' = updateSupps supp (sop:uops) (dop:defops)
      unq'  = updateUnqs unq (sop:uops) (dop:defops)
      dom'  = updateDoms dom (sop:uops) (dop:defops)
      isxor = False
      isgmul= False
      xor' = updateXor xor (sop:uops) [] True (dop:defops)
      c2o' = updateC2o c2o (dop:defops) oid -- take only destinations - dests and sources are the same..
      pmap' = updatePmaps isxor isgmul (pmap, init, supp', unq', dom', xor', m2o, c2o', p2p, p2t) (sop:uops) [] (dop:defops)
  in (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t)
-- virtual copies
inferTypesOperation _ _ types SingleOperation
  { oId  = oid, 
    oOpr = Virtual (VirtualCopy { oVirtualCopyS = s,
                                  oVirtualCopyD = d })} =
  let (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
      supp' = updateSupps supp [s] [d]
      unq'  = updateUnqs unq [s] [d]
      dom'  = updateDoms dom [s] [d]
      xor'  = updateXor xor [s] [] True [d]
      c2o' = updateC2o c2o [d] oid -- take only destinations - dests and sources are the same..
      pmap' = updatePmaps False False (pmap, init, supp', unq', dom', xor', m2o, c2o', p2p, p2t) [s] [] [d]
  in (pmap', init, supp', unq', dom', xor', m2o, c2o', p2p, p2t)
-- TODO(VirtualOperation): Phi/Delimiter/Kill/Define
inferTypesOperation _ _ types o @ SingleOperation
  { oId = oid,
    oOpr = Virtual ( Delimiter (In {
                                   oIns = oins
                                   })
                   )} =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    ins = map getOpidTemps oins
    res = foldl (updateNewTemps p2t p2p) types ins
  in res
inferTypesOperation _ _ types SingleOperation
  { oId = oid,
    oOpr = Virtual ( Delimiter (Out {
                                   oOuts = oouts
                                   })
                   )} =
  let
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    p2t' = foldl (\p2t'' -> \(p,ts) -> Map.insert p ts p2t'') p2t $ map getOpidTemps oouts
  in (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t')


inferTypesOperation _ _ types SingleOperation
  { oId = oid,
    oOpr = Virtual (_)} = types

-- inferTypeOperand _ types Temporary {tId = tid} =
--   case (Map.lookup (show tid) types) of
--     Nothing -> error ("SecTypeInf: Not found temporary" ++ show tid)
--     Just val -> Just val
-- inferTypeOperand _ _ NullTemporary =
--   Nothing
-- inferTypeOperand f types MOperand {altTemps = temps} =
--   let temptypes = map (inferTypeOperand f types) temps 
--       mtype = mergeTypes temptypes 
--   in mtype
-- inferTypeOperand _ _ _ = Nothing

-- TODO (Check if this is correct)
updateNewTemps :: Show r => Map Integer [Operand r] -> Map Integer Integer -> StateTuple r -> (Integer, [Operand r]) -> StateTuple r
updateNewTemps p2t p2p types (oid, tmps) = 
  let 
    (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) = types
    adj = Map.lookup oid p2p
    f oldtmps tmps =
      let
        supp' = updateSupps supp oldtmps tmps
        unq'  = updateUnqs unq oldtmps tmps
        dom'  = updateDoms dom oldtmps tmps
        xor'  = updateXor xor oldtmps [] True tmps
        pmap' = updatePmaps False False (pmap, init, supp', unq', dom', xor', m2o, c2o, p2p, p2t) oldtmps [] tmps
      in (pmap', init, supp', unq', dom', xor', m2o, c2o, p2p, p2t)
  in case adj of
       Just 62 ->
         -- case Map.lookup a p2t of
         --   Just oldtmps -> f oldtmps tmps
         --   Nothing -> 
         error $ "updateNewTemps: " ++ show oid ++ " " ++ show tmps 
       Just a ->
         case Map.lookup a p2t of
           Just oldtmps -> f oldtmps tmps
           Nothing -> error $ "updateNewTemps: " ++ show oid ++ " " ++ show tmps 
       Nothing -> types
  



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


updateM2o m2o fid oid =
  let
    f v = case v of
      Just m -> Just (Map.insert oid oid m)
      Nothing -> Just (Map.insert oid oid Map.empty)
  in Map.alter f fid m2o



updateC2o c2o dts oid =
  let
    dtids  = getTids dts [] -- destinations
    f v = case v of
      Just m -> Just (Map.insert oid oid m)
      Nothing -> Just (Map.insert oid oid Map.empty)
  in foldl (\mnew -> \cid -> Map.alter f cid mnew) c2o dtids 



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


updateSupps supp sts dts =
  let stids = getTids sts [] -- sources
      dtids = getTids dts [] -- destinations
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

updateDoms dom sts dts =
  let stids = getTids sts [] -- sources
      dtids = getTids dts [] -- destinations
      f m s tid = Map.union s $ Map.findWithDefault Map.empty tid m
      s1    = foldl (f dom) Map.empty stids
      f2 s1 s dtid = Map.insert dtid s1 s
      dom' = foldl (f2 s1) dom dtids
  in dom'


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


mapIntersection (h:tl) = foldl Map.intersection h tl
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

updatePmaps _ _ (pmap, _, _, _, _, _, _, _, _, _) _ _ [] = pmap
updatePmaps isxor isgmul (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) ts1 ts2 dts =
  let stids1 = getTids ts1 [] -- sources1
      stids2 = getTids ts2 [] -- sources2
      dtids  = getTids dts [] -- sources
  in updatePmapsID isxor isgmul (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) stids1 stids2 dtids

updatePmapsID _ _ (pmap, _, _, _, _, _, _, _, _, _) _ _ [] = pmap
updatePmapsID isxor isgmul (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) ts1 ts2 (dt:dts) =
  let pmap' = updatePmapID isxor isgmul (pmap, init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) ts1 ts2 dt
  in updatePmapsID isxor isgmul (pmap', init, supp, unq, dom, xor, m2o, c2o, p2p, p2t) ts1 ts2 dts

updatePmapID _ _ (pmap, _, _, _, dom, _, _, _, _, _) _ _ dt
  | not $ isEmpty dt dom =
    Map.insert dt (Random dt) pmap
updatePmapID _ _ (pmap, init, supp, _, dom, _, _, _, _, _) _ _ dt
  | (isEmpty dt dom) && (not $ intersectSec supp init $ dt)  =
    Map.insert dt (Public dt) pmap   
updatePmapID isxor isgmul (pmap, init, supp, _, dom, xor, _, _, _, _) ts1 ts2 dt =
  let supp1  = unionMaps supp ts1 
      supp2  = unionMaps supp ts2
      dom1   = unionMaps dom ts1 
      dom2   = unionMaps dom ts2
      xordt  = Map.findWithDefault False dt xor
      suppxor = if xordt
                then Just (Map.union (Map.difference supp1 supp2) (Map.difference supp2 supp1))
                else Nothing
      is1s2  = Map.null $ Map.intersection supp1 supp2
      eqs1s2 = (Map.null $ Map.difference supp1 supp2) && (Map.null $ Map.difference supp2 supp1)
      eqd1d2 = (Map.null $ Map.difference dom1 dom2) && (Map.null $ Map.difference dom2 dom1)
      diffd1s2 = Map.null $ Map.difference dom1 supp2
      diffd2s1 = Map.null $ Map.difference dom2 supp1
      typs1  = map (\tid -> Map.lookup tid pmap) ts1
      typs2  = map (\tid -> Map.lookup tid pmap) ts2
      typ1   = mergeTypes typs1
      typ2   = mergeTypes typs2
  in case (typ1, typ2, is1s2, eqs1s2, eqd1d2, diffd1s2, diffd2s1, suppxor) of
    (Just (Public _), Just (Public _), True, _, _, _, _, _) ->
      Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Random _), True, _, _, _, _, _) | not isxor -> 
                                                           Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Public _), True, _, _, _, _, _) | not isxor -> 
                                                           Map.insert dt (Public dt) pmap
    (Just (Random _), _, _, True, True, True, _, _) ->
      Map.insert dt (Public dt) pmap
    (_, Just (Random _), _, True, True, _, True, _) ->
      Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, False, _, _) | not isxor -> 
                                                            Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, _, False, _) | not isxor -> 
                                                            Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Public _), _, _, _, False, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Public _), Just (Random _), _, _, _, _, False, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, False, _, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (Just (Random _), Just (Random _), _, _, _, _, False, _) | isgmul ->
                                                 Map.insert dt (Public dt) pmap
    (_, _, _, _, _, _, _, Just d) | not $ intersectSec2 d init ->
                                    Map.insert dt (Public dt) pmap
    (_, _, _, _, _, _, _, _) -> Map.insert dt (Secret dt) pmap

-- to complete

-- fromJust (Just x) = x
-- fromJust Nothing = error "SecTypeInf: Unexpected Nothing" 

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

isEmpty t mp =
  case Map.lookup t mp of
    Just d | Map.null d -> True
    Just _              -> False
    Nothing             -> error "SecTypeInf: isEmpty - tid should be in dom"

getTids [] tids = tids
getTids ((Temporary {tId = tid}):ts) tids = getTids ts (("t" ++ show tid):tids)
getTids (NullTemporary:ts) tids = getTids ts tids
getTids ((MOperand {altTemps = temps}):ts) tids = getTids (temps ++ ts) tids
getTids (Bound (MachineFrameIndex {mfiIndex = mi}):ts) tids =
         getTids ts (("F" ++ show mi):tids)
getTids (_:ts) tids = getTids ts tids



findPairs [] _ res = res
findPairs (p:ps) types res = 
  let
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t) res p2 =
      let
        s1   = Map.findWithDefault Map.empty p supp
        s2   = Map.findWithDefault Map.empty p2 supp
        u1   = Map.findWithDefault Map.empty p unq
        u2   = Map.findWithDefault Map.empty p2 unq
        d1   = Map.findWithDefault Map.empty p dom
        d2   = Map.findWithDefault Map.empty p2 dom
        x1   = Map.findWithDefault False p xor
        x2   = Map.findWithDefault False p2 xor
        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12
        x12  = x1 && x2
        supp' = Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom
        xor' = Map.insert "tmp" x12 xor
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t) [p] [p2] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in if isMaybeSecret typ
         then (p,p2):res
         else res
      -- in (p,p2):res
    res' = foldl (f types) res ps
  in findPairs ps types res'




findRandSec [] _ _ res = res
findRandSec (s:ss) rs types @ (_, _, supp, unq, dom, xor, _, _, _, _) res = 
  let
    s1   = Map.findWithDefault Map.empty s supp
    u1   = Map.findWithDefault Map.empty s unq
    d1   = Map.findWithDefault Map.empty s dom
    x1   = Map.findWithDefault False s xor
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t) res r = 
      let
        s2   = Map.findWithDefault Map.empty r supp
        u2   = Map.findWithDefault Map.empty r unq
        d2   = Map.findWithDefault Map.empty r dom
        x2   = Map.findWithDefault False r xor
        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12
        x12  = x1 && x2
        supp' = Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom
        xor' = Map.insert "tmp" x12 xor
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t) [s] [r] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in if not $ isMaybeSecret typ
         then r:res
         else res
    ress = foldl (f types) [] rs
  in findRandSec ss rs types (([s],ress):res)


findRandSecMC [] _ _ _ res = res
findRandSecMC (s:ss) rs types @ (_, _, supp, unq, dom, xor, m2o, _, _, _) t2o res = 
  let
    s1   = Map.findWithDefault Map.empty s supp
    u1   = Map.findWithDefault Map.empty s unq
    d1   = Map.findWithDefault Map.empty s dom
    x1   = Map.findWithDefault False s xor
    ops1 = map fst $ Map.toList $ Map.findWithDefault Map.empty s m2o
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t) res r = 
      let
        s2   = Map.findWithDefault Map.empty r supp
        u2   = Map.findWithDefault Map.empty r unq
        d2   = Map.findWithDefault Map.empty r dom
        x2   = Map.findWithDefault False r xor
        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12
        x12  = x1 && x2
        supp'= Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom
        xor' = Map.insert "tmp" x12 xor
        ops2 = Map.findWithDefault Map.empty r t2o
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t) [s] [r] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in if not $ isMaybeSecret typ
         then Map.union ops2 res
         else res
    ress = foldl (f types) Map.empty rs
    ops2' = map fst $ Map.toList ress
  in findRandSecMC ss rs types t2o ((ops1,ops2'):res)


  
-- lowerConstraintExpr fs (OrExpr es) = OrExpr (map (lowerConstraintExpr fs) es)
-- lowerConstraintExpr fs (AndExpr es) = AndExpr (map (lowerConstraintExpr fs) es)
-- lowerConstraintExpr fs (XorExpr e1 e2) =
--   XorExpr (lowerConstraintExpr fs e1) (lowerConstraintExpr fs e2)
-- lowerConstraintExpr fs (ImpliesExpr e1 e2) =
--   ImpliesExpr (lowerConstraintExpr fs e1) (lowerConstraintExpr fs e2)
-- lowerConstraintExpr fs (NotExpr e) = NotExpr (lowerConstraintExpr fs e)
-- lowerConstraintExpr _ e @ ActiveExpr {} = e
-- lowerConstraintExpr _ e @ ConnectsExpr {} = e
-- lowerConstraintExpr (im, _) (ImplementsExpr oid i) =
--   EImplementsExpr oid (toIndexedInstruction im i)
-- lowerConstraintExpr _ e @ DistanceExpr {} = e
-- lowerConstraintExpr _ e @ ShareExpr {} = e
-- lowerConstraintExpr _ e @ OperandOverlapExpr {} = e
-- lowerConstraintExpr _ e @ TemporaryOverlapExpr {} = e
-- lowerConstraintExpr _ e @ CallerSavedExpr {} = e
-- lowerConstraintExpr (_, ra) (AllocatedExpr pid rc) =
--   EAllocatedExpr pid (raIndexedRc ra rc)
-- lowerConstraintExpr _ e @ AlignedExpr {} = e
