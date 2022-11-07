{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Algorithms to compute makespan bounds.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.MakespanBounds
    (computeMaxC, maxCost, maxInt, scaleFactor, scaleDown, minLiveOfDef, updateMaxC) where

import Data.Maybe as Maybe
import Data.Int
--import Data.List
import Data.Map as Map
import Data.List as List

import Unison
import Unison.Target.Query

-- TODO(Romy): This is the balancing block that should have any number of
--             cycles possible. Just multiplying with 8 for now.
-- computeMaxC (rm, oif) (Block {bAs = BlockAttributes {aBalanc = True}, bCode = code}, deps) = 
--   8 * (sum $ map (maxInstructionLatency (rm, oif, deps)) code)
computeMaxC (rm, oif) (Block {bCode = code}, deps) =
  sum $ List.map (maxInstructionLatency (rm, oif, deps)) code


updateMaxC bbs oldmcs = 
    let maxcs = getMaxBbs bbs Map.empty oldmcs
    in maxcs

getMaxBbs [] mp oldmcs = [ gNt (i,j) mp | (i,j) <- zip [0..] oldmcs ]
getMaxBbs (bb:bbs) mp oldmcs =
    let maxval = maximum $ List.map (\bi -> sum $ List.map 
                                        (\bii -> oldmcs !! (fromIntegral bii)) bi ) bb
        bb' = List.map fromIntegral $ concat bb
        mp' = List.foldl (\mpi -> \bi -> case Map.lookup bi mpi of
                                          Nothing -> Map.insert bi maxval mpi
                                          Just oldv | oldv < maxval -> Map.insert bi maxval mpi
                                          otherwise -> mpi) mp bb'

    in getMaxBbs bbs mp' oldmcs
--case Map.lookup bi mpi of 
--                                        Nothing -> Map.insert bi maxval mpi
--                                        Just oldv | oldv < maxval -> Map.insert bi maxval mpi
--                                        otherwise -> mpi

gNt (num,oldmc) mp = case Map.lookup num mp of
         Nothing -> oldmc
         Just nm -> nm

maxInstructionLatency targetFs o
    | isVirtual o = 1
    | isBundle o =
        maximum $ List.map (maxInstructionLatency targetFs) (bundleOs o)
    | otherwise =
        maximum $ Maybe.mapMaybe (worst targetFs o) (oInstructions o)

worst _ _ (General NullInstruction) = Nothing
worst (rm, oif, deps) o ti @ (TargetInstruction i) =
    let maxDur     = maxOrZero $ List.map (occupation . usage) (iUsages rm ti)
        useLats    = operandLats $ fst $ oif i
        defLats    = operandLats $ snd $ oif i
        maxOperLat = maxOrZero useLats + maxOrZero defLats
        ii         = fromJust $ elemIndex ti (oInstructions o)
        outgoing   = [dist !! ii | (p, _, dist) <- deps, oId o == p]
        incoming   = [dist | (_, s, dist) <- deps, oId o == s]
        maxDist    = maxOrZero $ catMaybes $ concat incoming ++ outgoing
        maxMinLive = if any isModelOperand (oDefs o) then
                       maximum [minLiveOfDef oif t o
                                 | t <- concatMap extractTemps $ oDefs o]
                     else 0
    in Just $ maximum [maxDur, maxOperLat, maxDist, maxMinLive]

operandLats oifs = [l | TemporaryInfo {oiLatency = l} <- oifs]

maxOrZero = maybeMax 0

maxCost (rm, oif, deps) code =
  let maxc    = List.map (computeMaxC (rm, oif)) (zip code deps)
      rawfreq = List.map blockFreq code
  in sum $ List.map (\(f, m) -> f * m) (zip rawfreq maxc)

maxInt = toInteger (maxBound - 1 :: Int32)

scaleFactor targetFs code =
  let max    = maxCost targetFs code
      factor = toRational maxInt / toRational max
  in if max > maxInt then factor else 1.0

scaleDown factor = atLeast 1 . floor . (*) factor . toRational

atLeast k n
  | n < k     = k
  | otherwise = n

minLiveOfDef oif t o
  | isIn o      = 1
  | isFun  o    = 1
  | isVirtual o = 0
  | otherwise =
      let lats = [latOf t (tempLatencies oif o i)
                   | TargetInstruction i <- oInstructions o]
      in if any ((==) 0) lats then 0 else 1 :: Latency

latOf t p2l = snd $ fromJust $ find (\(p, _) -> t `elem` extractTemps p) p2l
