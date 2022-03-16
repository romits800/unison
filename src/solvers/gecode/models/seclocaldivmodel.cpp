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


#include "seclocaldivmodel.hpp"


// SecLocalDivModel::SecLocalDivModel(Parameters * p_input, ModelOptions * p_options,
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

SecLocalDivModel::SecLocalDivModel(Parameters * p_input, ModelOptions * p_options,
				   IntPropLevel p_ipl,
				   const SecDecompDivModel * gs,
				   block p_b,
				   int seed_correction) :
  LocalModel(p_input, p_options, p_ipl, gs, p_b)
{
  div_r.seed(p_options->seed() + seed_correction);
  div_p = p_options->relax();

  branch_op = -1;
  
  for (uint i = 0; i < input -> ops[b].size(); i++) {
    operation o = input->ops[b][i];
    if (is_branch_type(o)) {
        branch_op = i;
    }
  }
  int op_size = O().size(); //input -> ops[b].size(); //O().size();
  int operand_size = P().size();

//    cout << "init:" << b << ":" << op_size << endl;
  int maxval = max_of(input->maxc);

  // difference between operators
  v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
  // Hamming distance between operators
  v_hamm  = int_var_array(op_size, -1, maxval);
  // Register  distance between operands
  v_reghamm  = int_var_array(operand_size, -1, input->RA.size() - 1);

}

SecLocalDivModel::SecLocalDivModel(SecLocalDivModel& cg) :
  LocalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r),
  branch_op(cg.branch_op),
  solver(cg.solver)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);
  v_reghamm.update(*this, cg.v_reghamm);

}

void SecLocalDivModel::set_solver(Json::Value root) {
  solver = new SolverParameters(root);
}

SecLocalDivModel* SecLocalDivModel::copy(void) {
  return new SecLocalDivModel(*this);
}



void SecLocalDivModel::constrain_total_cost(int cost) {
  //input->freq[b] * f(b,0), irt, cost
  //  rel(*this, input->freq[b] *f(b, 0), irt, cost, ipl); // 
  constraint( f(b, 0) <= cost);
}


void SecLocalDivModel::post_diversification_constraints(void) {
  if (options->dist_metric() == DIST_HAMMING) {
    post_diversification_hamming();
  }
  else if (options->dist_metric() == DIST_HAMMING_BR) {
    post_diversification_hamming();
  }
  else if (options->dist_metric() == DIST_HAMMING_DIFF) {
    post_diversification_hamming();
    post_diversification_diffs();
  }
  else if (options->dist_metric() == DIST_REGHAMMING) {
    post_diversification_reghamming();
  }
  else if (options->dist_metric() == DIST_CYC_REG_GADGET) {
    post_diversification_hamming();
    post_diversification_reghamming();
  }

}

void SecLocalDivModel::post_diversification_diffs(void) {
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

void SecLocalDivModel::post_diversification_hamming(void) {
  //
  for (uint o = 0; o < input -> ops[b].size(); o++) {
    operation i = input->ops[b][o];

    if (is_real_type(i)) {
        //BoolVar ifb = var (a(i) == 1);
        //IntVar thenb = var ( c(i) );
        //IntVar elseb = var ( -1 );
        //ite(*this, ifb,  thenb, elseb, hamm(o), IPL_DOM);
        constraint(hamm(o) == c(i));
    }
  }
}

void SecLocalDivModel::post_diversification_reghamming(void) {
  for (operation p: P()) {
   // operation pp = P()[p];
    constraint(reghamm(p) == ry(p));
  }
}

bool SecLocalDivModel::is_real_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == LINEAR ||
          input->type[o] == CALL ||
          input->type[o] == TAILCALL || 
          input->type[o] == COPY);
}

bool SecLocalDivModel::is_branch_type(int o) {

  bool is_jal = false;
  bool may_branch = input->type[o] == BRANCH || 
                    input->type[o] == TAILCALL || 
                    input->type[o] == CALL;
  
  if (may_branch) {
    string ins1 (input->insname[input->instructions[o][0]]);
    if ((ins1.compare(0,3,"JALR") == 0) || 
        (ins1.compare(0,2,"JR") == 0) || 
        (ins1.compare(0,12,"PseudoReturn") == 0))
        is_jal = true;
  }
  return is_jal;
}



void SecLocalDivModel::constrain(const Space & _b) {
  const SecLocalDivModel& bi = static_cast<const SecLocalDivModel&>(_b);

  BoolVarArgs bh;


  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      operation op = input->ops[b][o];
      if (is_real_type(op)) {
       if (bi.a(op).assigned() && bi.a(op).val() == 1) {
         bh << var (c(op) != bi.c(op)); 
        }
        bh << var (a(op) != bi.a(op)); 
      }
    }
    if (bh.size() >0) {          //
      constraint(sum(bh) >= 1); // hamming distance
    }
    break;
  case DIST_REGHAMMING:
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      operation op = input->ops[b][o];
      if (is_real_type(op))
          for (operand p: input->operands[op]) {
            if (bi.reghamm(p).assigned())
              bh << var (reghamm(p) != bi.reghamm(p));
            if (bi.x(p).assigned())
              bh << var (x(p) != bi.x(p));
         }
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    break;

  case DIST_REG_CYC_HAMMING:
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      operation op = input->ops[b][o];
      if (is_real_type(op)) {
	if (bi.a(op).assigned() && bi.a(op).val() == 1)
	  bh << var (c(op) != bi.c(op)); 
        bh << var (a(op) != bi.a(op)); 
      }
    }
    for (uint o = 0; o < input -> ops[b].size(); o++) {
      operation op = input->ops[b][o];
      if (is_real_type(op))
	for (operand p: input->operands[op]) {
	  if (bi.reghamm(p).assigned())
	    bh << var (reghamm(p) != bi.reghamm(p));
	  if (bi.x(p).assigned())
	    bh << var (x(p) != bi.x(p));
	}
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
      operation op = input->ops[b][o];
      if (is_branch_type(op))
        bh << var (hamm(o) != bi.hamm(o));
    }
    if (bh.size() >0)
      constraint(sum(bh) >= 1); // hamming distance
    break;
  case DIST_CYC_REG_GADGET:
   {
    int cyc_gadget = (int)options->cyc_gadget_size();
    //int reg_gadget = (int)options->reg_gadget_size();
    if (branch_op > -1) {
     for (uint o = 0; o < input -> ops[b].size(); o++) {
       operation op = input->ops[b][o];
       if (is_real_type(op)) {
          BoolVar ifb = var ( abs ( hamm(branch_op) - hamm(o)) <= cyc_gadget );
          BoolVar thenb = var (hamm(o) != bi.hamm(o));
          BoolVar elseb = var (hamm(o) != hamm(o));
          //BoolVar elseb = var (1 == 1);
          BoolVar res = BoolVar(*this, 0, 1);
          ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
          bh << res;
          if (is_branch_type(op)) {
            for (operand p: input->operands[op]) {
               if (bi.reghamm(p).assigned() && bi.reghamm(p).val() != 0)
                  bh << var (reghamm(p) != bi.reghamm(p));
               if (bi.x(p).assigned())
                  bh << var (x(p) != bi.x(p));
            }
          }
        }
      }
    }
    if (bh.size() >0)           //
      constraint(sum(bh) >= 1); // hamming distance
    else {
       // cerr << "No constraint" << endl;
       // GECODE_NEVER;
    }
   }
   break;
  case DIST_COST:
    for (uint i = 0; i< input->N; i++)
      if (bi.cost()[i].assigned())
	constraint(cost()[i] != bi.cost()[i]);
    break;
  default:
    cerr << "Distance not implemented!" << endl;
    GECODE_NEVER;
    break;

  }

  return;

}


bool SecLocalDivModel::master(const MetaInfo& mi) {
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




bool SecLocalDivModel::slave(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    string portfolio = options->local_portfolio();
    assert(mi.asset() < portfolio.size());
    char search = portfolio[mi.asset()];
    post_branchers(search);
    return true;
  } else if (mi.type() == MetaInfo::RESTART) {
    // std::cerr << "restart loc " << b << std::endl; // 
    //if ((mi.restart() > 0) && (div_p > 0.0)) {
    if ((div_p > 0.0)) {
      if (mi.last() != NULL) {
	next(static_cast<const SecLocalDivModel&>(*mi.last()));
      }
      return false;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void SecLocalDivModel::next(const SecLocalDivModel& l) {
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

void SecLocalDivModel::post_div_branchers(void) {

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
