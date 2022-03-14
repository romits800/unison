{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Export.AssignRegisters (assignRegisters) where

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray
import qualified Unison.Graphs.CG as CG

import Unison.Analysis.TemporaryType

-- TODO(Romy): do something better than this..
assignRegisters tight registers f @ Function {fCode = code} target
  | Unison.Target.API.isBoolOption "assign-reg" $ snd target  =
      let oif   = operandInfo target
          fCode = flatten code
          ra    = mkRegisterArray target 0
          ts    = sort (tUniqueOps fCode)
          cg    = CG.fromFunction f
          _     = CG.toDot cg
          t2w   = tempWidths ra oif fCode cg
          inf   = maxTempWidth tight code t2w
          ra'   = mkRegisterArray target inf
          t2aw  = M.fromList (zip ts (zip registers (M.elems t2w)))
          aw2r  = atomWidthToRegs ra'
          t2r   = M.map (toRegOrNull aw2r) t2aw
          code' = mapToOperationInBlocks (mapToModelOperand (applyMapToChoice t2r)) code
      -- in error ("Registers"  ++ (foldl (++) " " $ map show t2aw))
      in error ("\nBEGIN\n" ++ (print_vreg_to_mreg "" $ M.assocs t2r) ++ "END\n" ++ (print_copies ""  code')  ++ "Deleting identity copy.")
assignRegisters tight registers f @ Function {fCode = code} target =
    let oif   = operandInfo target
        fCode = flatten code
        ra    = mkRegisterArray target 0
        ts    = sort (tUniqueOps fCode)
        cg    = CG.fromFunction f
        _     = CG.toDot cg
        t2w   = tempWidths ra oif fCode cg
        inf   = maxTempWidth tight code t2w
        ra'   = mkRegisterArray target inf
        t2aw  = M.fromList (zip ts (zip registers (M.elems t2w)))
        aw2r  = atomWidthToRegs ra'
        t2r   = M.map (toRegOrNull aw2r) t2aw
        code' = mapToOperationInBlocks (mapToModelOperand (applyMapToChoice t2r)) code
    in f {fCode = code'}

toRegOrNull _ (RegisterAtom (-1), _) = NullTemporary
toRegOrNull aw2r aw =
  case M.lookup aw aw2r of
    Just r -> r
    Nothing -> error ("no register could be found corresponding to the (atom, width) combination " ++ show aw)

applyMapToChoice k2t tc @ MOperand {altTemps = ts} =
    tc {altTemps = map (applyMap' k2t) ts}

applyMap' _ NullTemporary = NullTemporary
applyMap' t2r t =
    let r = fromMaybe t (M.lookup t t2r)
    in preAssign t r


-- TODO(Romy): Do it in a better way
print_vreg_to_mreg acc []  = acc
print_vreg_to_mreg acc ((_, NullTemporary):tl) = print_vreg_to_mreg acc tl
print_vreg_to_mreg acc ((NullTemporary, _):tl) = print_vreg_to_mreg acc tl
print_vreg_to_mreg acc ((Temporary{ tId = tid },reg):tl) =
  let str = "[%vreg" ++ (show tid) ++ " -> %" ++ (show reg) ++ "] tGPR\n" ++ acc 
  in print_vreg_to_mreg str tl



print_copies acc [] = acc
print_copies acc (Block {bCode=bcode}:cs) =
  let pbcode = print_copies_bb "" bcode
  in print_copies (pbcode ++ acc) cs

print_copies_bb acc [] = acc
print_copies_bb acc
  (SingleOperation {
      oOpr = Natural {oNatural = Linear {oDs = ops }}} : cs) | is_assigned_mop ops =
  let ts = get_assigned_mops [] ops
      str = foldr (\ x -> \y -> x ++ "\n" ++ y ) "" $ map show_store ts
  in print_copies_bb (str ++ "\n" ++ acc) cs
print_copies_bb acc
  (SingleOperation {
      oOpr = Copy {oCopyD = MOperand {altTemps = ts}
                   -- oCopyDs = dests,
                   -- oCopyS = source,
                   -- oCopyUs = uses
                  }} : cs) | is_assigned_temp ts =
  let t = get_assigned_temp ts
      str = show_store t
  in print_copies_bb (str ++ "\n" ++ acc) cs
-- In
print_copies_bb acc (SingleOperation {oOpr = Virtual (Delimiter (In {oIns= oins}))} : cs) =
  let ts = get_assigned_mops [] oins
      str = foldr (\ x -> \y -> x ++ "\n" ++ y ) "" $ map show_store ts
  in print_copies_bb (str ++ "\n" ++ acc) cs
-- Out
print_copies_bb acc (SingleOperation {oOpr = Virtual (Delimiter (Out {oOuts= oouts}))} : cs) =
  let ts = get_assigned_mops [] oouts
      str = foldr (\ x -> \y -> x ++ "\n" ++ y ) "" $ map show_store ts
  in print_copies_bb (str ++ "\n" ++ acc) cs

print_copies_bb acc (Bundle {bundleOs = (hb : bos)} : cs) =
  let str = show hb  
  in print_copies_bb (str ++ "\n" ++ acc) (bos ++ cs)
print_copies_bb acc (_ : cs) = print_copies_bb acc cs



is_assigned_mop [] = False
is_assigned_mop (MOperand {altTemps = ts} : rest) = is_assigned_temp ts || is_assigned_mop rest
is_assigned_mop (_:tmps) = is_assigned_mop tmps

is_assigned_temp [] = False
is_assigned_temp (Temporary {tReg = Just (Register {})}: _) = True
is_assigned_temp (_:tmps) = is_assigned_temp tmps


get_assigned_mops acc [] = acc
get_assigned_mops acc (MOperand {altTemps = ts} : rest)
  | is_assigned_temp ts =
      get_assigned_mops (get_assigned_temp ts: acc) rest
get_assigned_mops acc (_:rest) = get_assigned_mops acc rest


get_assigned_temp [] = error "This should not happen"
get_assigned_temp (Temporary {tId = tid, tReg = Just (Register {})} : _) = tid 
get_assigned_temp (_:tmps) = get_assigned_temp tmps

show_store t = "ST4[%] %vreg" ++ (show t)
