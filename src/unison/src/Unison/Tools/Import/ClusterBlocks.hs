{-|
Copyright   :  Copyright (c) 2022, KTH
License     :  BSD3 (see the LICENSE file)
Maintainer  :  tsoupidi@kth.se
-}
{-
Main authors:
  Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>

-}
module Unison.Tools.Import.ClusterBlocks (clusterBlocks) where

import Data.List     -- for `sortBy`
import Data.Function -- for `on`
import Data.Word(Word32)
-- import Data.List.Split
-- import Data.Ord
-- import qualified Data.Map as M

{-
procedure:
  1. generate adjacency matrix
  2. find eigenvalues
  3. run kmeans
-}

import AI.Clustering.KMeans
-- import Numeric.LinearAlgebra.Arnoldi -- bindings to fortran library arpack
import Numeric.LinearAlgebra.HArpack
-- from vector
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U
-- from matrices
-- import qualified Data.Matrix.Generic as M
import qualified Data.Matrix.Unboxed as MU
import Unison

import Unison.Target.API
import Unison.Target.Query
import Unison.Tools.Import.SplitBlocks(applyToPhiOps, splitIntoBlocks)

import qualified Unison.Graphs.DG as DG
-- import qualified Unison.Graphs.BCFG as BCFG
import Data.Graph.Inductive

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

--- k: the number of clusters
type AccType = (Integer, Integer, Map Integer Integer)

clusterBlocks:: Show i => Show r => Ord s => Ord r => Ord i =>
                Integer -> Maybe Word32 -> Maybe Integer -> Function i r ->
                TargetWithOptions i r rc s -> Function i r
clusterBlocks k iter neigens f @ Function {fCode = code} target =
  let
    -- TODO(Convert to fold)
    bid     = newBlockIndex code  
    oid     = newId code
    ((_, _, lastB), bs) = mapAccumL (clusterBlock k iter neigens target)
                          (bid, oid, Map.empty) code
    code'   = mapToOperationInBlocks (applyToPhiOps (applyMap lastB)) (concat bs)
  in f{ fCode = code'}


clusterBlock:: Show i => Show r => Ord s => Ord r => Ord i =>
               Integer -> Maybe Word32 -> Maybe Integer ->
               TargetWithOptions i r rc s -> AccType ->
               Block i r -> (AccType, [Block i r])
clusterBlock k iter neigens target acc code =
  let
    -- Generate Dependency Graph
    (dg,dg')      = genDgs code target
    -- Number of clusters
    ki            = fromIntegral k
    -- Generate eigenvalues from adjacency matrix
    eigvs         = genEigenValues ki dg neigens
    -- Run KMeans on the eigenvalues
    (_err, km)    = runKMeansMany iter ki eigvs
    -- Sort clusters so that they are in accending order
    km'           = sortClusterNumbers (-1) km Map.empty []
    -- Correct the clustering
    km''          = correctClustering dg' km'
    -- Reoder instructions in block based on clustering
    (orkm, code') = reorderOps code km''
    -- Split blocks based on clustering
    nb            = splitBlock orkm acc code'
    -- code'       = zip (flatten code) km''
    -- use splitIntoBlocks 

    -- test  = map show $ head dgs
    -- adj   = adjMatrix dgs
    -- deps  = map DG.dependencies dgs
  in nb -- error $ show (km, km', km'', zip km'' blcode, code', nb, err)


splitBlock :: [Int] -> AccType -> Block i r -> (AccType, [Block i r])
splitBlock cls acc b =
  let lengths = countLengths cls
  in splitIntoBlocks lengths acc b

  -- in b

countLengths :: [Int] -> [Int] 
countLengths cls =
  countLengthsI cls 0 0 []

-- countLengths listclusters currentCluster currentLength accumulator
countLengthsI :: [Int] -> Int -> Int -> [Int] -> [Int]
countLengthsI [] _ l acc = List.reverse (l:acc)
countLengthsI (h:t) n l acc | h == n = countLengthsI t n (l+1) acc
countLengthsI (h:t) _ l acc | otherwise =
                              countLengthsI t h 1 (l:acc) 

reorderOps :: Block i r -> [Int] -> ([Int],Block i r)
reorderOps b @ Block {bCode = code} clusters = 
  let blcl = zip clusters code
      sorted = sortBy (\(cl1,o1) (cl2,o2) -> compare (cl1,oId o1) (cl2, oId o2)) blcl
  in (map fst sorted, b {bCode = map snd sorted})

genDgs :: Show i => Show r => Ord s => Ord r => Ord i =>
          Block i r -> TargetWithOptions i r rc s -> (DGraph i r, DGraph i r)
genDgs f target = 
  let rwlf  = readWriteLatency target
      rm    = resourceManager target
      oif   = operandInfo target
      dg    = DG.fromBlockCl rwlf rm oif f
      dg'   = DG.fromBlock rwlf rm oif f
  in (dg, dg')


-- maximum :: Ord a => [a] -> a
-- maximum = foldr1 (\x y ->if x >= y then x else y)
genEigenValues:: Show i => Show r => Ord r => Ord i =>
                 Int -> DGraph i r -> Maybe Integer -> MU.Matrix Double --[[Double]] --[V.Vector Double]
genEigenValues k dg neigens =
  let 
    dim = (maximum $ nodes dg) + 1
    eds = edges dg
    edsv = concatMap (\(i,j) -> [((i,j), 1.0), ((j,i), 1.0)]) eds
    sm  = SparseMatrix { dim = dim, indexes = edsv } -- sparse matrix
    -- taken from lineageflow that uses harpack
    -- TODO: Don't know why this is better when using more than k
    RReal m = case neigens of
      Just n -> eigs sm (Symmetric SLA) (fromIntegral n)
      Nothing -> eigs sm (Symmetric SLA) (k + 2)
    vectors = map snd . sortBy (compare `on` (\x -> - (fst x))) $ m
  in MU.tr $ MU.fromLists $ map V.toList vectors


-- kmeans :: Int                -- ^ The number of clusters
--        -> MU.Matrix Double   -- ^ Input data stored as rows in a matrix
--        -> KMeansOpts
--        -> KMeans (U.Vector Double)
-- kmeans k mat opts
sortClusterNumbers :: Int -> [Int] -> Map Int Int -> [Int] -> [Int]
sortClusterNumbers _ [] _ acc = List.reverse acc
sortClusterNumbers curr (h:t) m acc | isInMap h m =
                                      let newh = 
                                            case Map.lookup h m of
                                              Just nh -> nh
                                              Nothing -> error ("SortClusterNumbers: This should not happen." ++ show h)
                                      in sortClusterNumbers curr t m (newh:acc)
sortClusterNumbers curr (h:t) m acc | otherwise =
                                      let
                                        newcurr = curr + 1
                                        m' = Map.insert h newcurr m
                                      in sortClusterNumbers newcurr t m' (newcurr:acc)

isInMap :: Int -> Map Int Int -> Bool
isInMap h m = case Map.lookup h m of
                Just _ -> True
                Nothing -> False



runKMeansMany :: Maybe Word32 -> Int -> MU.Matrix Double -> (Double,[Int]) --KMeans (U.Vector Double)
runKMeansMany num k eigs =
  let
    opts = defaultKMeansOpts {
      kmeansClusters = False --,
     -- kmeansSeed = U.fromList [1..num]
      }
    res     = kmeans k eigs opts
    err     = sse res
    members = U.toList $ membership res
    num'    = case num of
                Just n -> n
                Nothing -> 100
  in
    runKMeansManyI (num'-1) k (err, members) eigs

runKMeansManyI :: Word32 -> Int -> (Double,[Int]) -> MU.Matrix Double -> (Double,[Int]) 
runKMeansManyI num _ best _ | num <= 0 = best
runKMeansManyI num k (berr, bmembers) eigs =
  let
    -- num' = int2word# num --fromIntegral ((toIntegral) num :: Word32)
    opts = defaultKMeansOpts {
      kmeansClusters = False,
      kmeansSeed = U.fromList [num]
      }
    res     = kmeans k eigs opts
    err     = sse res
    members = U.toList $ membership res
  in if err < berr
     then runKMeansManyI (num-1) k (err, members) eigs
     else runKMeansManyI (num-1) k (berr, bmembers) eigs


-- runKMeans :: Int -> MU.Matrix Double -> (Double,[Int]) --KMeans (U.Vector Double)
-- runKMeans k eigs =
--   let
--     opts = defaultKMeansOpts {
--       kmeansClusters = False,
--       kmeansSeed = U.fromList [1..200]
--       }
--     res     = kmeans k eigs opts
--     err     = sse res
--     members = U.toList $ membership res
--   in
--     (err, members)


correctClustering :: Show i => Show r => DGraph i r -> [Int] -> [Int]
correctClustering dg cls =
  let
    zcls = (zip [0..] cls)
    m = Map.fromList zcls
    mx = maximum cls
    cls' = checkClusters dg zcls m mx
  in cls'

checkClusters :: Show i => Show r => DGraph i r -> [(Int, Int)] -> Map Int Int -> Int -> [Int]
checkClusters _ [] m _ = map snd $ Map.toList m
checkClusters dg ((h,cl):t) m mx =
  let op = DG.toIstr dg h
      hprecs = DG.precs dg h --- nodes that have dependency with current node
      clprecs = map (\n -> myFromJust $ Map.lookup n m) hprecs
      allprecs = all (\pr -> pr <= cl) clprecs
  in case (allprecs, op) of
    -- Returns don't seem to have branches
    (_, SingleOperation {oOpr = Natural { oNatural = Branch {}}}) -> -- TODO(Fix this for only returns)
      let m' = Map.insert h mx m
      in checkClusters dg t m' mx
        --error ("operation" ++ show op)
    (True,_) -> checkClusters dg t m mx
    (False,_) -> let maxdep = maximum clprecs
                     m' = Map.insert h maxdep m
                 in checkClusters dg t m' mx

myFromJust (Just j) = j
myFromJust _ = error "MyFromJust: This should not happen."
  
-- splitBlocksCl maxBlockSize f @ Function {fCode = code} _target =
--   let bid     = newBlockIndex code
--       oid     = newId code
--       ((_, _, lastB),
--           bs) = mapAccumL (splitBlock maxBlockSize) (bid, oid, M.empty) code
--       code'   = mapToOperationInBlocks (applyToPhiOps (applyMap lastB)) (concat bs)
--   in f {fCode = code'}

-- splitBlock maxSize acc b @ Block {bCode = code} =
--   let last     = toInteger $ length code - 3
--       (_, ps)  = mapAccumL splittable Splittable (zip [0..] code)
--       possible = concat (init ps)
--       ideal    = [maxSize, maxSize + maxSize .. last]
--       final    = map (\i -> minimumBy (comparing (distanceTo i)) possible) ideal
--       lengths  = distances (toInteger $ length code) final
--   in case lengths of
--       []      -> (acc, [b])
--       lengths -> splitIntoBlocks lengths acc b

-- data SplitState =
--   Splittable | WithinCall | PostCall Integer | WithinPhi Integer | NoSplit

-- {-
-- This assumes the following code sequence for function calls:
--   [] <- callr [...]
--   [t1, t2, ...] <- (fun) [...] (call: (the one before))
--   [] <- (kill) [t1, t2, ...] (optional)
-- -}

-- splittable Splittable (_, o) | isDelimiter o = (Splittable, [])
-- splittable Splittable (_, o) | isCall o = (WithinCall, [])
-- splittable WithinCall (p, o) | isFun o = (PostCall p, [])
-- splittable (PostCall p') (p, o)
--     | isKill o  = (Splittable, [p])
--     | otherwise = (Splittable, [p', p])
-- splittable _ (p, o) | isPhi o = (WithinPhi p, [])
-- splittable (WithinPhi p') (p, _) = (Splittable, [p', p])
-- splittable Splittable (_, o)
--     | isSplitBarrier o = (NoSplit, [])
-- splittable Splittable (p, _) = (Splittable, [p])
-- splittable NoSplit _ = (NoSplit, [])

-- distanceTo x y = abs (y - x)

-- distances n (x:list) = x+1:distances1 n (x:list)
-- distances _ []       = []

-- distances1 n (x:y:list) = y-x:distances1 n (y:list)
-- distances1 n (x:[])     = [n-x-1]
-- distances1 _ []         = []

-- splitIntoBlocks lengths (bid, oid, lastB)
--                 Block {bLab = l, bCode = code, bAs = attrs} =
--   let codes  = splitPlaces lengths code
--       bs1    = zipWith (curry mkNewBlock) [bid..] codes
--       bs2    = (head bs1) {bLab = l} : tail bs1
--       bs3    = copyBlockAttrs attrs [(aEntry, copyEntry)] (head bs2) : tail bs2
--       bs4    = init bs3 ++ [copyBlockAttrs attrs
--                             [(aExit, copyExit), (aReturn, copyReturn)]
--                             (last bs3)]
--       bs5    = map (copyBlockAttrs attrs [(aFreq, copyFreq)]) bs4
--       bs6    = zipWith (curry addIn) [oid..] bs5
--       bs7    = zipWith (curry addOut) [newId bs6..] bs6
--       bs8    = [head bs7] ++ map addSplit (tail bs7)
--       bid'   = newBlockIndex bs8
--       oid'   = newId bs8
--       lastB' = M.insert l (bLab $ last bs8) lastB
--   in ((bid', oid', lastB'), bs8)

-- addIn (oid, b @ Block {bCode = code})
--   | any isIn code = b
--   | otherwise     = b {bCode = mkIn oid [] : code}

-- addOut (oid, b @ Block {bCode = code})
--   | any isOut code = b
--   | otherwise      = b {bCode = code ++ [mkOut oid []]}

-- mkNewBlock (bid, code) = mkBlock bid mkNullBlockAttributes code

-- copyBlockAttrs srcAttrs afs b @ Block {bAs = dstAttrs} =
--   b {bAs = foldl (copyBlockAttr srcAttrs) dstAttrs afs}

-- copyBlockAttr srcAttrs dstAttrs (af, cf) = cf dstAttrs (af srcAttrs)

-- copyEntry  attrs v = attrs {aEntry = v}
-- copyFreq   attrs v = attrs {aFreq = v}
-- copyExit   attrs v = attrs {aExit = v}
-- copyReturn attrs v = attrs {aReturn = v}

-- applyToPhiOps lastB o
--     | isPhi o = mapToOperandIf isBlockRef (replaceBlockRef lastB) o
--     | otherwise = o

-- replaceBlockRef lastB (BlockRef l) = mkBlockRef (lastB l)

-- addSplit b @ Block {bAs = attrs} = b {bAs = attrs {aSplit = True}}
