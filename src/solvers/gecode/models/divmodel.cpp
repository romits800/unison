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

  div_r.seed(time(NULL)); //p_options->seed());
  div_p = p_options->relax();
  int op_size = O().size();
  int maxval = max_of(input->maxc);
  // difference between operations
  int real_op_size = 0;
  for (operation o : input -> O)
    if (is_real_type(o))
      real_op_size += 1;

  if (options->dist_metric() == DIST_HAMMING_DIFF) {
    v_diff  = int_var_array((real_op_size*(real_op_size -1))/2, -maxval, maxval);
  }
  // difference between operations and branch operations
  else if (options->dist_metric() == DIST_HAMMING_DIFF_BR) {
    int br_size = 0;
    for (operation o : input -> O)
      if (is_branch_type(o))
        br_size += 1;
    // Is it ok if br_size = 0?
    v_diff  = int_var_array((real_op_size  )*br_size, -maxval, maxval);
  }

  // Prepare cycles for hamming distance between operations' cycles
  v_hamm  = int_var_array(op_size, -1, maxval);

  // Global cycles array - similar to cycles
  v_gc = int_var_array(op_size, 0, sum_of(input->maxc));

  // Array for levenshtein distance
  v_oc = set_var_array(sum_of(input->maxc) + 1, IntSet::empty, IntSet(0,op_size));
}

DivModel::DivModel(DivModel& cg) :
  GlobalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);
  v_gc.update(*this, cg.v_gc);
  v_oc.update(*this, cg.v_oc);
}

DivModel* DivModel::copy(void) {
  return new DivModel(*this);
}


void DivModel::post_div_branchers(void) {

  if (options->branching() == BR_RND) {

    branch(*this, cost(), INT_VAR_NONE(), INT_VAL_MIN(),
	   NULL, &print_global_cost_decision);
    Rnd r;
    r.seed(time(NULL));
    branch(*this, v_a, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
           NULL, &print_global_inactive_decision);

    branch(*this, v_c, INT_VAR_RND(r), INT_VAL_RND(r),
	   &schedulable, &print_global_cycle_decision);

    branch(*this, v_i, INT_VAR_RND(r), INT_VAL_MIN(),
	   NULL, &print_global_instruction_decision);

    branch(*this, v_y, INT_VAR_RND(r), INT_VAL_MIN(),
	   NULL, &print_global_temporary_decision);

    branch(*this, v_r, INT_VAR_RND(r), INT_VAL_RND(r),
	   &global_assignable, &print_global_register_decision);

  } else if (options->branching() == BR_COMPLETE) {

    GlobalModel::post_complete_branchers(time(NULL));
  }

}

void DivModel::post_diversification_constraints(void) {
  post_global_cycles();
  post_diversification_hamming();
  if (options->dist_metric() == DIST_HAMMING_DIFF)
    post_diversification_diffs();
  if (options->dist_metric() == DIST_HAMMING_DIFF_BR)
    post_diversification_br_diffs();
  if (options->dist_metric() == DIST_LEVENSHTEIN || options->dist_metric() == DIST_LEVENSHTEIN_SET )
    post_diversification_levenshtein();

}


void DivModel::post_diversification_levenshtein(void) {
  IntVarArgs bs;
  uint smaxc = sum_of(input->maxc);
  for (operation i : input -> O) {
    if (!is_real_type(i)) continue; //
    BoolVar ifb = var ( a(i) == 1 );
    IntVar thenb = var ( gc(i) );
    IntVar elseb = IntVar(*this, 0, smaxc);
    max(*this, v_gc, elseb);
    IntVar res = IntVar(*this, 0, smaxc);
    ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
    bs << res;
  }
  channel(*this, bs, v_oc);
}

void DivModel::post_diversification_diffs(void) {
  int k=0;
  int maxval = max_of(input->maxc);
  for (uint i = 0; i < input->O.size(); i++) {
    if (!is_real_type(i)) continue;
    for (uint j = i+1; j< input->O.size(); j++) {
      // If then else constraint
      if (!is_real_type(j)) continue;
      BoolVar ifb = var ((a(i) == 1) && (a(j) == 1));
      IntVar elseb = var (maxval) ;
      IntVar thenb =  var (gc(i) - gc(j));
      ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
      k++;
    }
  }
}

void DivModel::post_diversification_br_diffs(void) {
  int k=0;
  int maxval = max_of(input->maxc);
  for (operation o : input -> O) {
    if (!is_real_type(o)) continue;
    for (operation br : input -> O)
      if (is_branch_type(br)) {
        BoolVar ifb = var ((a(br) == 1) && (a(o) == 1));
        IntVar elseb = var (maxval) ;
        IntVar thenb =  var (gc(br) - gc(o));
        ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
        k++;
      }
  }
}


void DivModel::post_diversification_hamming(void) {
  for (operation i : input -> O) {
    if (!is_real_type(i)) continue;
    BoolVar ifb = var (a(i) == 1);
    IntVar thenb = var ( gc(i) );
    IntVar elseb = var ( -1 );
    ite(*this, ifb,  thenb, elseb, hamm(i), IPL_DOM);
  }
}

void DivModel::post_global_cycles(void) {
  // VarInt offset;
  for(block b: input->B) {
    for (operation o: input->ops[b]) {
      if (b == 0)
        constraint(gc(o) == c(o));
      else
        constraint(gc(o) == c(o) + gc(input->out[b-1]));
    }
  }
}




void DivModel::post_levenshtein(const DivModel & b)
{
  uint sizex = v_oc.size() + 1; // size of maxc

  IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
  Matrix<IntVarArray> mat(x, sizex, sizex);


  mat(0,0) = var(0);
  for (uint i = 1; i < sizex; i++) {
    mat(i,0) = var(i);
    mat(0,i) = var(i);
  }
  for (uint i = 1; i < sizex; i++)
    for (uint j = 1; j < sizex; j++) {

      BoolVar res = var ( oc(i-1) != b.oc(j-1) );
      IntVarArgs v;
      v << var (mat(i-1,j) + 1);
      v << var (mat(i,j-1) + 1);
      v << var (mat(i-1,j-1) + res);
      min(*this, v, mat(i,j));
    }

  constraint(mat(sizex-1, sizex-1) >= 1); // Levenshtein distance
}

void DivModel::post_levenshtein_set(const DivModel & b)
{
  uint sizex = v_oc.size() + 1; // size of maxc
  int op_size = O().size();
  IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
  Matrix<IntVarArray> mat(x, sizex, sizex);


  mat(0,0) = var(0);
  for (uint i = 1; i < sizex; i++) {
    mat(i,0) = var( mat(i-1,0) + cardinality(oc(i-1)) ); //var(i);
    mat(0,i) = var( mat(0,i-1) + cardinality(b.oc(i-1)) ); //var(i);
  }
  for (uint i = 1; i < sizex; i++)
    for (uint j = 1; j < sizex; j++) {
      IntVarArgs cs;
      cs << var (cardinality (oc(i-1) - b.oc(j-1)));
      cs << var (cardinality (b.oc(j-1) - oc(i-1)));
      IntVar res = IntVar(*this, 0, op_size);
      max(*this, cs, res);

      IntVarArgs v;
      v << var (mat(i-1,j) + cardinality(oc(i-1)));
      v << var (mat(i,j-1) + cardinality(b.oc(j-1)));
      v << var (mat(i-1,j-1) + res);
      min(*this, v, mat(i,j));
    }

  constraint(mat(sizex-1, sizex-1) >= 1); // Levenshtein distance
}
bool DivModel::is_real_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == LINEAR ||
          input->type[o] == CALL ||
          input->type[o] == TAILCALL ||
          input->type[o] == COPY);
}

bool DivModel::is_branch_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == TAILCALL ||
          input->type[o] == CALL);
}

void DivModel::constrain(const Space & _b) {
  const DivModel& b = static_cast<const DivModel&>(_b);

  BoolVarArgs bh;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (operation o: input -> O) {
      if (is_real_type(o))
          bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_DIFF:

    for (int i = 0; i < v_diff.size(); i++) {
      bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_DIFF_BR:
    for (int i = 0; i < v_diff.size(); i++) {
      bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR:
    for (operation o : input -> O) {
      if (is_branch_type(o))
        bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance on the branches
    else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_LEVENSHTEIN:
    post_levenshtein(b);
    break;

  case DIST_LEVENSHTEIN_SET:
    post_levenshtein_set(b);
  }

  return;

}


void DivModel::post_constrain(DivModel* _b) {

  const DivModel& b = static_cast<const DivModel&>(*_b);

  BoolVarArgs bh;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (operation o: input -> O) {
      if (is_real_type(o))
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
  case DIST_LEVENSHTEIN:
    post_levenshtein(b);
    break;
  case DIST_LEVENSHTEIN_SET:
    post_levenshtein_set(b);
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
    } else if (mi.restart() == 0) {
      return true;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void DivModel::next(const DivModel& b) {
  // Instructions
  if (!options->disable_relax_i()) {
    IntVarArgs instr, linstr;
    for (operation o: input -> O) {
      instr << i(o);
      linstr << b.i(o);
    }
    relax(*this, instr, linstr, div_r, div_p);
  }

  // temporaries
  if (!options->disable_relax_y()) {
    IntVarArgs temp, ltemp;
    for (operand p : input -> P) {
      temp << y(p);
      ltemp << b.y(p);
    }
    relax(*this,temp, ltemp, div_r, div_p);
  }


  // Cycles
  if (!options->disable_relax_c()) {
    // Relax all active variables.
    // relax(*this, v_a, b.v_a, div_r, 1.0);
    IntVarArgs cycles, lcycles;
    for (operation o : input -> O) {
      if (b.a(o).val()) { // if activated
        cycles << c(o);
        lcycles << b.c(o);
      }
    }
    relax(*this, cycles, lcycles, div_r, div_p);
  }

  // Registers
  if (!options->disable_relax_r()) {
    IntVarArgs lregs, regs;
    for (temporary t : input->T) {
      if (b.l(t).assigned() && b.l(t).val()) { // if the tempoorary is assigned
        lregs << b.r(t);
        regs << r(t);
      }
    }
    // Regs
    relax(*this, regs, lregs, div_r, div_p);
  }

}

