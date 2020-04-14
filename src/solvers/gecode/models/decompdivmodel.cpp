/*
 *  Main authors:
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  This file is part of DivCon
 *
 *  Copyright (c) 2020, Rodothea Myrsini Tsoupidi
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


#include "decompdivmodel.hpp"



DecompDivModel::DecompDivModel(Parameters * p_input, ModelOptions * p_options,
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

DecompDivModel::DecompDivModel(DecompDivModel& cg) :
  GlobalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);
}

DecompDivModel* DecompDivModel::copy(void) {
  return new DecompDivModel(*this);
}


void DecompDivModel::post_diversification_constraints(void) {
  post_diversification_hamming();
  if (options->dist_metric() == DIST_HAMMING_DIFF) {
    post_diversification_diffs();
  }
}

void DecompDivModel::post_diversification_diffs(void) {
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


void DecompDivModel::post_diversification_hamming(void) {
  for (operation i : input -> O) {
    BoolVar ifb = var (a(i) == 1);
    IntVar thenb = var ( c(i) );
    IntVar elseb = var ( -1 );
    ite(*this, ifb,  thenb, elseb, hamm(i), IPL_DOM);
  }
}

bool DecompDivModel::is_real_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == LINEAR ||
          input->type[o] == CALL ||
          input->type[o] == COPY);
}

bool DecompDivModel::is_branch_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == CALL);
}


void DecompDivModel::constrain(const Space & _b) {
  // const DecompDivModel& b = static_cast<const DecompDivModel&>(_b);

  // BoolVarArgs bh;

  // switch (options->dist_metric()) {
  // case DIST_HAMMING:
  //   for (operation o: input -> O) {
  //     bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)           //
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_DIFF:

  //   for (int i = 0; i < v_diff.size(); i++) {
  //     bh << var (diff(i) != b.diff(i));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_BR:
  //   for (operation o : input -> O) {
  //     if (input->type[o] == BRANCH)
  //       bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // }

  return;

}


void DecompDivModel::post_constrain(DecompDivModel* _b) {

  const DecompDivModel& b = static_cast<const DecompDivModel&>(*_b);

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
  case DIST_HAMMING_DIFF_BR:

    for (int i = 0; i < v_diff.size(); i++) {
      bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_BR:
    for (operation o : input -> O) {
      if (is_branch_type(o))
        bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  }

  return;

}

bool DecompDivModel::master(const MetaInfo& mi) {
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



bool DecompDivModel::slave(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    post_complete_branchers(mi.asset());
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if ((mi.restart() > 0) && (div_p > 0.0)) {
      if (mi.last() != NULL)// {
        next(static_cast<const DecompDivModel&>(*mi.last()));
      return false;
      // } else
        // return true;
    } else if (mi.restart() == 0) {
      return true;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void DecompDivModel::next(const DecompDivModel& l) {

    relax(*this, v_oa, l.v_oa, div_r, div_p);

    relax(*this, v_s , l.v_s , div_r, div_p);
}

