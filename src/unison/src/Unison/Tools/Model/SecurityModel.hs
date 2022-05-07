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

-- import Data.List
-- import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Set as S
import Data.Aeson (toJSON)

import Unison
import Unison.Target.API


import Unison.Tools.Model.Definitions()
import Unison.ParseSecurityPolicies
import Unison.Transformations.SecurityTypeInference


isNotInInmap inmap t = isNothing $ Map.lookup t inmap

allNotInmap ts inmap =
  all (isNotInInmap inmap) ts 

parameters (_,_,_,_,ra,_) target f @ Function {fCode = _} policies =
  let
    nt @ (pmap',inmap,_,_,_,_,m2o',c2o', _, _,_) = inferSecurityTypes target f policies
    (sec,pub,ran)   = splitTemps (Map.toAscList pmap') ([],[],[])
    ran'            = filter (\x -> head x == 't') ran
    pub'            = filter (\x -> head x == 't') pub
    sec'            = filter (\x -> head x == 't' && isNotInInmap inmap x) sec
    sec''           = filter (\x -> head x == 'F' || head x == 'S') sec  -- memory secrets
    ran''           = filter (\x -> head x == 'F' || head x == 'S' || head x == 't') ran  -- memory randoms
    pub''           = filter (\x -> head x == 'F' || head x == 'S' || head x == 't') pub
    -- Parameters
    pairs           = findPairs ran' pub' nt []
    secdom          = findRandSec sec' ran' nt []
    p2o             = Map.union c2o' m2o'   --- Not the same key
    mpairs          = findPairsMC ran'' pub'' nt p2o []
    secdommem       = findRandSecMC sec'' ran'' nt p2o inmap [] 
    hr              = map (mkRegister . mkTargetRegister) $ hardwareRegisters target
    hregs           = concatMap (\x -> Map.findWithDefault [] x $ regAtoms ra) hr
  in
    [
      ("Types",     toJSON $ [sec,pub,ran]),
      ("Rands",     toJSON $ ran'),
      -- Security parameters
      ("pairs",     toJSON $ map toInt pairs), -- pairs of random vars that should not reside in the same register
      -- todo(Romy): is it enough with random x random or should I check public x random
      ("spairs",    toJSON $ map toInt2 secdom), -- secret vars that should be preceeded by a random variable in the same register.
      ("mmpairs",    toJSON  mpairs), -- mem to memory leakage
      ("mspairs",    toJSON secdommem), -- secret memory - operations
      -- ("adj2",      toJSON $ Map.toList p2p), -- secret memory - operations
      -- ("adj25",     toJSON adjacent), -- secret memory - operations
      -- ("adj3",      toJSON $ Map.toList p2t'), -- secret memory - operations
      ("HR",        toJSON hregs)
    ]


isNothing Nothing = True
isNothing _ = False

-- isRandom (Random _) = True
-- isRandom _ = False


splitTemps [] res = res
splitTemps ((k, Secret _):ts) (s,p,r) = splitTemps ts (k:s, p, r)
splitTemps ((k, Random _):ts) (s,p,r) = splitTemps ts (s, p, k:r)
splitTemps ((k, Public _):ts) (s,p,r) = splitTemps ts (s, k:p, r)

-- debug
-- printPmap (_, Secret t) = t ++ ": " ++ "Secret"
-- printPmap (_, Random t) = t ++ ": " ++ "Random"
-- printPmap (_, Public t) = t ++ ": " ++ "Public"

toInt ('t':x,'t':y) = (read x:: Integer, read y::Integer) 
toInt (t1, t2)      = error $ "SecTypeInf: toInt cannot convert to integer: " ++ t1 ++ " " ++ t2


toInt2 (['t':x], rs) = ([read x:: Integer], map toInt1 rs)
toInt2 ([s], _) = error $ "SecTypeInf: toInt2 cannot convert to integer: " ++ s
toInt2 _ = error "SecTypeInf: toInt2 unexpected argument."

toInt1 ('t':x) = read x:: Integer
toInt1 t       = error $ "SecTypeInf: toInt cannot convert to integer: " ++ t

-- printMap (_, m) = map fst $ Map.toAscList m


-- TODO (Check if this is correct)
findPairs [] _ _ res = res
findPairs (p:ps) pubs types res = 
  let
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args) res p2 =
      let
        s1   = Map.findWithDefault Map.empty p supp
        s2   = Map.findWithDefault Map.empty p2 supp
        u1   = Map.findWithDefault Map.empty p unq
        u2   = Map.findWithDefault Map.empty p2 unq
        d1   = Map.findWithDefault Map.empty p dom
        d2   = Map.findWithDefault Map.empty p2 dom
        x1   = Map.findWithDefault False p xor
        x2   = Map.findWithDefault False p2 xor
        x12  = x1 && x2

        -- supp'= updateBSupps (supp, xor) [p] [p2] ["tmp"]
        -- unq' = updateBUnqs (unq, supp) [p] [p2] ["tmp"]
        -- dom' = updateBDoms (dom, unq') [p] [p2] ["tmp"]

        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12
        supp' = Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom
        xor' = Map.insert "tmp" x12 xor
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args) [p] [p2] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in
        -- if (p == "t26" && p2 == "t12") || (p == "t12" && p2 == "t26") then
        --   error (show typ)
        -- else 
          if isMaybeSecret typ
          then (p,p2):res
          else res
      -- in (p,p2):res
    res' = foldl (f types) res (ps ++ pubs)
  in findPairs ps pubs types res'




findRandSec [] _ _ res = res
findRandSec (s:ss) rs types @ (_, _, supp, unq, dom, xor, _, _, _, _, _) res = 
  let
    s1   = Map.findWithDefault Map.empty s supp
    u1   = Map.findWithDefault Map.empty s unq
    d1   = Map.findWithDefault Map.empty s dom
    x1   = Map.findWithDefault False s xor
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args) res r = 
      let
        s2   = Map.findWithDefault Map.empty r supp
        u2   = Map.findWithDefault Map.empty r unq
        d2   = Map.findWithDefault Map.empty r dom
        x2   = Map.findWithDefault False r xor
        x12  = x1 && x2
        
        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12 
        supp' = Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom

        -- supp'= updateBSupps (supp, xor) [r] s ["tmp"]
        -- unq' = updateBUnqs (unq, supp) [r] s ["tmp"]
        -- dom' = updateBDoms (dom, unq') [r] s ["tmp"]

        xor' = Map.insert "tmp" x12 xor
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args) [s] [r] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in if not $ isMaybeSecret typ
         then r:res
         else res
    ress = foldl (f types) [] rs
  in findRandSec ss rs types (([s],ress):res)


filterInmap [] _ res = res
filterInmap ((op1, ts):rest) inmap res =
  if allNotInmap ts inmap
  then filterInmap rest inmap (op1:res)
  else filterInmap rest inmap res 


findRandSecMC [] _ _ _ _ res = res
findRandSecMC (s:ss) rs types @ (_, _, supp, unq, dom, xor, m2o, _, _, _, _) t2o inmap res = 
  let
    s1   = Map.findWithDefault Map.empty s supp
    u1   = Map.findWithDefault Map.empty s unq
    d1   = Map.findWithDefault Map.empty s dom
    x1   = Map.findWithDefault False s xor
    ops  = Map.toList $ Map.findWithDefault Map.empty s m2o
    ops1 = filterInmap ops inmap []
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args) res r = 
      let
        s2   = Map.findWithDefault Map.empty r supp
        u2   = Map.findWithDefault Map.empty r unq
        d2   = Map.findWithDefault Map.empty r dom
        x2   = Map.findWithDefault False r xor
        x12  = x1 && x2

        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12

        supp'= Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom

        -- supp'= updateBSupps (supp, xor) [r] s ["tmp"]
        -- unq' = updateBUnqs (unq, supp) [r] s ["tmp"]
        -- dom' = updateBDoms (dom, unq') [r] s ["tmp"]

        xor' = Map.insert "tmp" x12 xor
        ops2 = Map.findWithDefault Map.empty r t2o
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args) [s] [r] "tmp"
        typ  = Map.lookup "tmp" pmap'
      in if not $ isMaybeSecret typ
         then Map.union ops2 res
         else res
    ress = foldl (f types) Map.empty rs
    ops2' = map fst $ Map.toList ress
  in case ops1 of
    [] -> findRandSecMC ss rs types t2o inmap res
    _ -> findRandSecMC ss rs types t2o inmap ((ops1,ops2'):res)



findPairsMC [] _ _ _ res = res
findPairsMC (p:ps) pubs types t2o res = 
  let
    f (pmap, inmap, supp, unq, dom, xor, m2o, c2o, p2p, p2t, args) res p2 =
      let
        s1   = Map.findWithDefault Map.empty p supp
        s2   = Map.findWithDefault Map.empty p2 supp
        u1   = Map.findWithDefault Map.empty p unq
        u2   = Map.findWithDefault Map.empty p2 unq
        d1   = Map.findWithDefault Map.empty p dom
        d2   = Map.findWithDefault Map.empty p2 dom
        x1   = Map.findWithDefault False p xor
        x2   = Map.findWithDefault False p2 xor
        x12  = x1 && x2

        -- supp'= updateBSupps (supp, xor) [p] [p2] ["tmp"]
        -- unq' = updateBUnqs (unq, supp) [p] [p2] ["tmp"]
        -- dom' = updateBDoms (dom, unq') [p] [p2] ["tmp"]

        s12  = Map.union s1 s2
        is12 = Map.intersection s1 s2
        u12  = Map.difference (Map.union u1 u2) is12
        d12  = Map.intersection (Map.union d1 d2) u12
        supp' = Map.insert "tmp" s12 supp
        unq' = Map.insert "tmp" u12 unq
        dom' = Map.insert "tmp" d12 dom
        xor' = Map.insert "tmp" x12 xor

        -- get operations that correspond to p and p2
        ops1 = map fst $ Map.toList $ Map.findWithDefault Map.empty p t2o
        ops2 = map fst $ Map.toList $ Map.findWithDefault Map.empty p2 t2o
        pmap' = updatePmapID True False (pmap, inmap, supp', unq', dom', xor', m2o, c2o, p2p, p2t, args) [p] [p2] "tmp"
        typ  = Map.lookup "tmp" pmap'
        pairs = [ (i,j) | i <- ops1, j <- ops2]
      in
        -- if (p == "t26" && p2 == "t12") || (p == "t12" && p2 == "t26") then
        --   error (show typ)
        -- else 
          if isMaybeSecret typ
          then pairs ++ res
          else res
      -- in (p,p2):res
    res' = foldl (f types) res (ps ++ pubs)
  in findPairsMC ps pubs types t2o res'

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
