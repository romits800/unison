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
  DivModel(p_input, p_options, p_ipl)
{

  // div_r.seed(p_options->seed());
  // div_p = p_options->relax();

  // int op_size = O().size();

  // int maxval = max_of(input->maxc);
  // // difference between operators
  // v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
  // // Hamming distance between operators
  // v_hamm  = int_var_array(op_size, -1, maxval);

}

DecompDivModel::DecompDivModel(DecompDivModel& cg) :
  DivModel(cg)
  // div_p(cg.div_p),
  // div_r(cg.div_r)
{
  // v_diff.update(*this, cg.v_diff);
  // v_hamm.update(*this, cg.v_hamm);
}

DecompDivModel* DecompDivModel::copy(void) {
  return new DecompDivModel(*this);
}


// void DecompDivModel::post_random_branchers(void) {
//   branch(*this, cost(), INT_VAR_NONE(), INT_VAL_MIN(),
//          NULL, &print_global_cost_decision);
//   Rnd r;
//   r.seed(options->seed());
//   branch(*this, v_a, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
//          NULL, &print_global_inactive_decision);

//   branch(*this, v_c, INT_VAR_RND(r), INT_VAL_RND(r),
//          &schedulable, &print_global_cycle_decision);

//   branch(*this, v_i, INT_VAR_RND(r), INT_VAL_MIN(),
//          NULL, &print_global_instruction_decision);

//   branch(*this, v_y, INT_VAR_RND(r), INT_VAL_MIN(),
//          NULL, &print_global_temporary_decision);

//   branch(*this, v_r, INT_VAR_RND(r), INT_VAL_RND(r),
//          &global_assignable, &print_global_register_decision);


// }



// void DecompDivModel::post_clrandom_branchers(void) {
//   Rnd r;
//   r.seed(options->seed());
//   //branch(*this, v_gadget, INT_VAR_RND(r), INT_VAL_RND(r),
//   //       NULL, NULL);

//   branch(*this, v_a, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
//          NULL, &print_global_inactive_decision);

//   branch(*this, v_i, INT_VAR_RND(r), INT_VAL_MIN(),
//          NULL, &print_global_instruction_decision);

//   branch(*this, v_y, INT_VAR_RND(r), INT_VAL_MIN(),
//          NULL, &print_global_temporary_decision);

//   branch(*this, v_c, INT_VAR_RND(r), INT_VAL_RND(r),
//          &schedulable, &print_global_cycle_decision);

//   branch(*this, v_r, INT_VAR_RND(r), INT_VAL_RND(r),
//          &global_assignable, &print_global_register_decision);

// }

// void DecompDivModel::post_cloriginal_branchers(void) {
//   Rnd r;
//   r.seed(options->seed());
//   branch(*this, v_a, BOOL_VAR_NONE(), BOOL_VAL_RND(r),
//          NULL, &print_global_inactive_decision);

//   branch(*this, v_c, INT_VAR_NONE(), INT_VAL_MIN(),
//          &schedulable, &print_global_cycle_decision);

//   branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
//          NULL, &print_global_instruction_decision);

//   branch(*this, v_y, INT_VAR_NONE(), INT_VAL_MIN(),
//          NULL, &print_global_temporary_decision);

//   branch(*this, v_r, INT_VAR_NONE(), INT_VAL_RND(r),
//          &global_assignable, &print_global_register_decision);

// }


// // int  DivModel::commit(IntVar x, int i) 
// void DecompDivModel::post_div_branchers(void) {

//   // if (options->enable_solver_solution_brancher() && solver->has_solution) {
//   //   IntArgs sol;
//   //   IntVarArgs vs;
//   //   for (int c: solver->cycles) sol << c;
//   //   for (int r: solver->registers) sol << r;
//   //   for (int t: solver->temporaries) sol << t;
//   //   for (IntVar c: v_c) vs << c;
//   //   for (IntVar r: v_r) vs << r;
//   //   for (IntVar t: v_y) vs << t;
    
//   //   // TODO(Romy):
//   //   // assign(*this, vs, INT_ASSIGN(v,c*));
//   //   solution_branch(*this, vs, sol);
//   // }
//   if (options->branching() == BR_RND) {
//     post_random_branchers();
//   }
//   else if (options->branching() == BR_RND_COSTLAST) {
//     post_clrandom_branchers();
//   }
//   else if (options->branching() == BR_ORIGINAL_COSTLAST) {
//     post_cloriginal_branchers();
//   }
//   else if (options->branching() == BR_ORIGINAL) {
//     GlobalModel::post_complete_branchers(options->seed()) ;
//   }
// }


// void DecompDivModel::post_diversification_constraints(void) {
//   post_global_cycles();
//   post_diversification_hamming();
//   post_diversification_reghamming();
//   post_diversification_channel();
//   if (options->dist_metric() == DIST_HAMMING_DIFF)
//     post_diversification_diffs();
//   if ((options->dist_metric() == DIST_HAMMING_DIFF_BR) ||
//       (options->dist_metric() == DIST_DIFF_BR)) {
//     post_diversification_br_diffs();
//   }
 
//   if (options->dist_metric() == DIST_HAMMING_REG_GADGET ||
//       options->dist_metric() == DIST_REG_GADGET         ||
//       options->dist_metric() == DIST_CYC_GADGET) {
//     post_diversification_reg_gadget();
//   }
// }

// void DecompDivModel::post_diversification_channel(void) {
//   // IntVarArgs bs;
//   // uint smaxc = sum_of(input->maxc);
//   // for (operation i : input->O) { //real_operations) {
//   //   if (is_real_type(i)) {
//   //       BoolVar ifb = var ( a(i) == 1 );
//   //       IntVar thenb = var ( gc(i) );
//   //       IntVar elseb = var(smaxc); //IntVar(*this, 0, smaxc); //
//   //       // max(*this, v_gc, elseb);
//   //       IntVar res = IntVar(*this, 0, smaxc);
//   //       ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
//   //       bs << res;
//   //   } else {
//   //       bs << var(smaxc);
//   //   }
//   // }

//   // channel(*this, bs, v_oc);

// }

// void DecompDivModel::post_diversification_diffs(void) {
//   // int k=0;
//   // int maxval = max_of(input->maxc);
//   // for (uint i = 0; i < real_operations.size(); i++) {
//   //   for (uint j = i+1; j< real_operations.size(); j++) {
//   //     // If then else constraint
//   //     BoolVar ifb = var ((a(i) == 1) && (a(j) == 1));
//   //     IntVar elseb = var (maxval) ;
//   //     IntVar thenb =  var (gc(i) - gc(j));
//   //     ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
//   //     k++;
//   //   }
//   // }
// }

// void DecompDivModel::post_diversification_br_diffs(void) {
//   // int maxval = max_of(input->maxc);
//   // int prevbr = 0; 
//   // int k=0;
//   // for (operation br: branch_operations) {
//   //   for (operation o: real_operations) { 
//   //     if (br == o) continue;
//   //     BoolVar ifb = var ((a(br) == 1) && (a(o) == 1) && (gc(o) > gc(prevbr)) && (gc(o) <= gc(br)));
//   //     IntVar thenb =  var (gc(br) - gc(o));
//   //     IntVar elseb = var (maxval);
//   //     ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
//   //     k++;
//   //   }
//   //   prevbr = br;
//   // }

// }


// void DecompDivModel::post_diversification_reg_gadget(void) {
//   // int maxval = max_of(input->maxc);
//   // //int prevbr = 0; 
//   // int k=0;
//   // for (operation br: branch_operations) {
//   //   for (operation o: real_operations) { 
//   //     if (br == o) continue;
//   //     BoolVar ifb = var ((a(br) == 1) && (a(o) == 1) && (gc(o) <= gc(br))); //(gc(o) > gc(prevbr)) && (gc(o) < gc(br)));
//   //     //IntVar thenb =  gc(o);
//   //     IntVar thenb =  var (gc(br) - gc(o));
//   //     IntVar elseb = var (maxval);
//   //     ite(*this, ifb,  thenb, elseb, gadget(k), IPL_DOM);
//   //     k++;
//   //   }
//   //   //prevbr = br;
//   // }

// }


// void DecompDivModel::post_diversification_reghamming(void) {
//   // for (operand p: input->P) {
//   //   constraint(reghamm(p) == ry(p));
//   // }
// }


// void DecompDivModel::post_diversification_hamming(void) {
//   // for (operation i : real_operations) {
//   //   BoolVar ifb = var (a(i) == 1);
//   //   IntVar thenb = var ( gc(i) );
//   //   IntVar elseb = var ( -1 );
//   //   ite(*this, ifb,  thenb, elseb, hamm(i), IPL_DOM);
//   // }
// }

// void DecompDivModel::post_global_cycles(void) {
//   // VarInt offset;
//   // for (block b: input->B) {
//   //   for (operation o: input->ops[b]) {
//   //     if (b == 0)
//   //       constraint(gc(o) == c(o));
//   //     else
//   //       constraint(gc(o) == c(o) + gc(input->out[b-1]));
//   //   }
//   // }
// }




// void DecompDivModel::post_levenshtein(const DecompDivModel & b)
// {
//   // uint sizex = v_oc.size(); // size of maxc
//   // // uint num_gadgets = branch_operations.size();

//   // IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
//   // Matrix<IntVarArray> mat(x, sizex, sizex);

//   // mat(0,0) = var(0);
//   // for (uint i = 1; i < sizex; i++) {
//   //   mat(i,0) = var(i);
//   //   mat(0,i) = var(i);
//   // }
//   // for (uint i = 1; i < sizex; i++)
//   //   for (uint j = 1; j < sizex; j++) {

//   //     BoolVar res = var ( oc(i-1) != b.oc(j-1) );
//   //     IntVarArgs v;
//   //     v << var (mat(i-1,j) + 1);
//   //     v << var (mat(i,j-1) + 1);
//   //     v << var (mat(i-1,j-1) + res);
//   //     min(*this, v, mat(i,j));
//   //   }

//   // dist = var( mat(sizex-1, sizex-1));

//   // constraint(dist >= mindist); // Levenshtein distance
// }

// void DecompDivModel::post_levenshtein_set(const DecompDivModel & b)
// {
//   // uint sizex = v_oc.size();// + 1; // size of maxc
//   // // int op_size = O().size();
//   // IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
//   // Matrix<IntVarArray> mat(x, sizex, sizex);
//   // uint maxcap = max_of(input->cap);

//   // IntVarArray cap = int_var_array(sizex-1, 0, maxcap);
//   // IntVarArray bcap = int_var_array(sizex-1, 0, maxcap);


//   // for (uint i = 0; i < sizex-1; i++) {
//   //   cap[i] = var(cardinality(oc(i)));
//   //   bcap[i] = var(cardinality(b.oc(i)));
//   // }

//   // mat(0,0) = var(0);
//   // for (uint i = 1; i < sizex; i++) {
//   //   IntVar nw = cap[i-1]; //var(cardinality(oc(i-1)));
//   //   IntVar old = bcap[i-1]; //var(cardinality(b.oc(i-1)));
//   //   mat(i,0) = var( mat(i-1,0) +  nw);
//   //   mat(0,i) = var( mat(0,i-1) + old);
//   // }

//   // for (uint i = 1; i < sizex; i++)
//   //   for (uint j = 1; j < sizex; j++) {
//   //     IntVarArgs cs;
//   //     cs << var (cardinality (oc(i-1) - b.oc(j-1)));
//   //     cs << var (cardinality (b.oc(j-1) - oc(i-1)));
//   //     IntVar res = IntVar(*this, 0, maxcap);
//   //     max(*this, cs, res);

//   //     IntVarArgs v;
//   //     v << var (mat(i-1,j) + cap[i-1]); //cardinality(oc(i-1)));
//   //     v << var (mat(i,j-1) + bcap[i-1]); //cardinality(b.oc(j-1)));
//   //     v << var (mat(i-1,j-1) + res);
//   //     min(*this, v, mat(i,j));
//   //   }

//   // dist = var( mat(sizex-1, sizex-1));
//   // constraint( dist >= mindist); // Levenshtein distance
// }


// bool DecompDivModel::is_real_type(int o) {

//   return (input->type[o] == BRANCH ||
//           input->type[o] == LINEAR ||
//           input->type[o] == CALL ||
//           input->type[o] == TAILCALL ||
//           input->type[o] == COPY);

// }

// bool DecompDivModel::is_branch_type(int o) {

//   return (input->type[o] == BRANCH ||
//           input->type[o] == TAILCALL ||
//           input->type[o] == CALL);

// }

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

