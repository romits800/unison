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


#include "localdivmodel.hpp"


// LocalDivModel::LocalDivModel(Parameters * p_input, ModelOptions * p_options,
//                        IntPropLevel p_ipl,
//                        const DecompDivModel * gs, block p_b) :
//   LocalModel(p_input, p_options, p_ipl, gs, p_b)
// {
//   div_r.seed(p_options->seed());
//   div_p = p_options->relax();

//   int op_size = O().size();

//   int maxval = max_of(input->maxc);
//   // difference between operators
//   v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
//   // Hamming distance between operators
//   v_hamm  = int_var_array(op_size, -1, maxval);

// }

LocalDivModel::LocalDivModel(Parameters * p_input, ModelOptions * p_options,
                             IntPropLevel p_ipl,
                             const DecompDivModel * gs,
			     block p_b,
			     int seed_correction) :
  LocalModel(p_input, p_options, p_ipl, gs, p_b)
{
  div_r.seed(p_options->seed() + seed_correction);
  div_p = p_options->relax();

  int op_size = O().size();

  int maxval = max_of(input->maxc);
  // difference between operators
  v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
  // Hamming distance between operators
  v_hamm  = int_var_array(op_size, -1, maxval);

}

LocalDivModel::LocalDivModel(LocalDivModel& cg) :
  LocalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r),
  solver(cg.solver)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);

}

void LocalDivModel::set_solver(JSONVALUE root) {
  solver = new SolverParameters(root);
}

LocalDivModel* LocalDivModel::copy(void) {
  return new LocalDivModel(*this);
}

void LocalDivModel::constrain_total_cost(int cost) {
  //input->freq[b] * f(b,0), irt, cost
  //  rel(*this, input->freq[b] *f(b, 0), irt, cost, ipl); // 
  constraint( f(b, 0) <= cost);
}


void LocalDivModel::post_diversification_constraints(void) {
  post_diversification_hamming();
  if (options->dist_metric() == DIST_HAMMING_DIFF) {
    post_diversification_diffs();
  }

}

void LocalDivModel::post_diversification_diffs(void) {
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

void LocalDivModel::post_diversification_hamming(void) {
  //
  for (uint o = 0; o < input -> ops[b].size(); o++) {
    operation i = input->ops[b][o];
    BoolVar ifb = var (a(i) == 1);
    IntVar thenb = var ( c(i) );
    IntVar elseb = var ( -1 );
    ite(*this, ifb,  thenb, elseb, hamm(o), IPL_DOM);
  }
}

bool LocalDivModel::is_real_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == LINEAR ||
          input->type[o] == CALL ||
          input->type[o] == COPY);
}

bool LocalDivModel::is_branch_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == CALL);
}


void LocalDivModel::constrain(const Space & _b) {
  const LocalDivModel& bi = static_cast<const LocalDivModel&>(_b);

  BoolVarArgs bh;


  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      bh << var (hamm(o) != bi.hamm(o));
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_DIFF:
  //   // for (uint o=0; o< input -> ops[b].size(); o++) {
  //   for (int i = 0; i < v_diff.size(); i++) {
  //     bh << var (diff(i) != bi.diff(i));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_HAMMING_DIFF_BR:
    break;
  case DIST_HAMMING_BR:
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      if (is_branch_type(o))
        bh << var (hamm(o) != bi.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  }

  return;

}


bool LocalDivModel::master(const MetaInfo& mi) {
  std::cerr << "master loc " << b << std::endl; // 
  if (mi.type() == MetaInfo::PORTFOLIO) {
    assert(mi.type() == MetaInfo::PORTFOLIO);
    return true; // default return value for portfolio master (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if (mi.last() != NULL) {
      constrain(*mi.last());
    }
    mi.nogoods().post(* this);
    return true; // forces a restart even if a solution has been found
  }
  GECODE_NEVER;
}




bool LocalDivModel::slave(const MetaInfo& mi) {
  std::cerr << "master loc " << b << std::endl; // 
  if (mi.type() == MetaInfo::PORTFOLIO) {
    string portfolio = options->local_portfolio();
    assert(mi.asset() < portfolio.size());
    char search = portfolio[mi.asset()];
    post_branchers(search);
    return true;
  } else if (mi.type() == MetaInfo::RESTART) {
    std::cerr << "restart loc " << b << std::endl; // 
    //if ((mi.restart() > 0) && (div_p > 0.0)) {
    if ((div_p > 0.0)) {
      if (mi.last() != NULL) {
	next(static_cast<const LocalDivModel&>(*mi.last()));
      }
      return false;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void LocalDivModel::next(const LocalDivModel& l) {
  std::cerr << "next local " << b << std::endl; // 
  if (!options->disable_relax_i()) {
    IntVarArgs instr, linstr;
    for (operation o : input->ops[b]) {
      instr << i(o);
      linstr << l.i(o);
    }
    relax(*this, instr, linstr, div_r, div_p);
  }

  if (!options->disable_relax_y()) {
    IntVarArgs temp, ltemp;
    for (operand p : input->ope[b]) {
        temp << y(p);
        ltemp << l.y(p);
      }
    relax(*this,temp, ltemp, div_r, div_p);
  }


  if (!options->disable_relax_c()) {
    relax(*this, v_a, l.v_a, div_r, div_p); //
    IntVarArgs cycles, lcycles;
    for (operation o : input->ops[b])
      if (l.a(o).val()) { // if activated
        cycles << c(o);
        lcycles << l.c(o);
      }

    relax(*this, cycles, lcycles, div_r, div_p);
  }

  if (!options->disable_relax_r()) {
    IntVarArgs lregs, regs;
    for (temporary t : input->tmp[b]) {
      if (l.l(t).assigned() && l.l(t).val()) { // if the tempoorary is assigned
        lregs << l.r(t);
        regs << r(t);
      }
    }
    relax(*this, regs, lregs, div_r, div_p);
  }

}

void LocalDivModel::post_div_branchers(void) {

  Rnd rnd;
  rnd.seed(options->seed());

  // BoolVarArgs a;
  // for (operation o : input->ops[b])
  //   a << v_a[o];
  branch(*this, v_a, BOOL_VAR_RND(rnd), BOOL_VAL_RND(rnd));


  // IntVarArgs is;
  // for (operation o : input->ops[b])
  //   is << v_i[o];
  branch(*this, v_i, INT_VAR_RND(rnd), INT_VAL_MIN());

  IntVarArgs ts;
  for (operand p : input->groupcopyrel[b]) ts << y(p);
  branch(*this, ts, INT_VAR_RND(rnd), INT_VAL_MIN());

  // IntVarArgs c;
  // for (operation o : input->ops[b])
  //   c << v_c[o];

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);
  
  branch(*this, v_c, INT_VAR_RND(rnd), INT_VAL_RND(rnd));

  // IntVarArgs r;
  // for (temporary t : input->tmp[b])
  //   r << v_r[t];
      
  branch(*this, v_r, INT_VAR_RND(rnd), INT_VAL_RND(rnd));

}
