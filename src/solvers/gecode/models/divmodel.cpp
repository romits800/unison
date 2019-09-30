/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  Contributing authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, RISE SICS AB
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *  3. Neither the name of the copyright holder nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */


#include "divmodel.hpp"



DivModel::DivModel(Parameters * p_input, ModelOptions * p_options,
                   IntPropLevel p_ipl) :
  GlobalModel(p_input, p_options, p_ipl)
{

  div_r.seed(p_options->seed());
  div_p = p_options->relax();

  int op_size = O().size();

  int maxval = max_of(input->maxc);
  // difference between operators
  v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
  // Hamming distance between operators
  v_hamm  = int_var_array(op_size, -1, maxval);

}

DivModel::DivModel(DivModel& cg) :
  GlobalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);
}

DivModel* DivModel::copy(void) {
  return new DivModel(*this);
}


void DivModel::post_diversification_constraints(void) {
  post_diversification_hamming();
  if (options->dist_metric() == DIST_HAMMING_DIFF) {
    post_diversification_diffs();
  }
}

void DivModel::post_diversification_diffs(void) {
  int k=0;
  int maxval = max_of(input->maxc);
  for (uint i = 0; i < input->O.size(); i++)
    for (uint j = i+1; j< input->O.size(); j++) {
      // If then else constraint
      BoolVar ifb = var ((a(i) == 1) && (a(j) == 1));
      IntVar elseb = var (maxval) ;
      IntVar thenb =  var (c(i) - c(j));
      ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
      k++;
    }
}


void DivModel::post_diversification_hamming(void) {
  for (operation i : input -> O) {
    BoolVar ifb = var (a(i) == 1);
    IntVar thenb = var ( c(i) );
    IntVar elseb = var ( -1 );
    ite(*this, ifb,  thenb, elseb, hamm(i), IPL_DOM);
  }
}



void DivModel::constrain(const Space & _b) {

  const DivModel& b = static_cast<const DivModel&>(_b);

  BoolVarArgs bh;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (operation o: input -> O) {
      bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_DIFF:

    for (int i = 0; i < v_diff.size(); i++) {
      bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_BR:
    for (operation o : input -> O) {
      if (input->type[o] == BRANCH)
        bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  }

  return;

}


void DivModel::post_constrain(DivModel* _b) {

  const DivModel& b = static_cast<const DivModel&>(*_b);

  BoolVarArgs bh;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (operation o: input -> O) {
      bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_DIFF:

    for (int i = 0; i < v_diff.size(); i++) {
      bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_BR:
    for (operation o : input -> O) {
      if (input->type[o] == BRANCH)
        bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  }

  return;

}

bool DivModel::master(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    assert(mi.type() == MetaInfo::PORTFOLIO);
    return true; // default return value for portfolio master (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if (mi.last() != NULL)
      constrain(*mi.last());
    mi.nogoods().post(* this);
    return true; // forces a restart even if a solution has been found
  }
  GECODE_NEVER;
}



bool DivModel::slave(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    post_complete_branchers(mi.asset());
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if ((mi.restart() > 0) && (div_p > 0.0)) {
      if (mi.last() != NULL)// {
        next(static_cast<const DivModel&>(*mi.last()));
      return false;
      // } else
        // return true;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void DivModel::next(const DivModel& l) {


  if (!options->disable_relax_i()) {
    IntVarArgs instr, linstr;
    for (operation o: input -> O) {
      instr << i(o);
      linstr << l.i(o);
    }
    relax(*this, instr, linstr, div_r, div_p);
  }

  if (!options->disable_relax_y()) {
    IntVarArgs temp, ltemp;
    for (operand p : input -> P) {
      temp << y(p);
      ltemp << l.y(p);
    }
    relax(*this,temp, ltemp, div_r, div_p);
  }


  if (!options->disable_relax_c()) {
    relax(*this, v_a, l.v_a, div_r, div_p);
    IntVarArgs cycles, lcycles;
    for (operation o : input -> O) {
      if (l.a(o).val()) { // if activated
        cycles << c(o);
        lcycles << l.c(o);
      }
    }
    relax(*this, cycles, lcycles, div_r, div_p);
  }

  if (!options->disable_relax_r()) {
    IntVarArgs lregs, regs;
    for (temporary t : input->T) {
      if (l.l(t).assigned() && l.l(t).val()) { // if the tempoorary is assigned
        lregs << l.r(t);
        regs << r(t);
      }
    }
    relax(*this, regs, lregs, div_r, div_p);
  }

}

