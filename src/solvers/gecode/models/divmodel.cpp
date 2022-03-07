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


#include "divmodel.hpp"


DivModel::DivModel(Parameters * p_input, ModelOptions * p_options,
                   IntPropLevel p_ipl) :
  GlobalModel(p_input, p_options, p_ipl)
{

  div_r.seed(p_options->seed());
  div_p = p_options->relax();
  mindist = p_options->min_dist();

  int op_size = O().size();
  
  int operand_size = P().size();
  int maxval = sum_of(input->maxc);
  // difference between operations

  // Initialize branch operations
  for (operation o : input -> O)
    if (is_branch_type(o))
      branch_operations.push_back(o);

  // Initialize real operations
  for (operation o : input -> O)
    if (is_real_type(o))
      real_operations.push_back(o);


  int real_op_size = real_operations.size();


  // Initialize v_diff for diff_hamming and diff_br_hamming
  v_hamm  = int_var_array(op_size, -1, maxval);

  if (options->dist_metric() == DIST_HAMMING_DIFF) {
    v_diff  = int_var_array((real_op_size*(real_op_size -1))/2, -maxval, maxval);
  }

  // Gadgets creates groups between two branches that correspond to possible
  // gadgets
  int size = 0;
  for (operation br: branch_operations) {
    gadget_t g;
    g.start = size;
    for (operation o: real_operations) { // = prevbr; o < br; o++) {
      //if (br == o) continue;
      gadgets_operations.push_back(o);
      size++;
    }
    g.end = size;
    gadgets.push_back(g);
  }

  // print pairs of rands that should not be assigned to the same regs
  // for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
  //   std::cout << tp.first << " " << tp.second << std::endl;
  // }  

  // print pairs of rands that should not be assigned to the same regs
  // for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
  //   std::cout << tp.first;
  //   for (const temporary t: tp.second) 
  //     std::cout << " " << t;
  //   std::cout << std:: endl;
  // }

  // print pairs of rands that should not be assigned to the same regs
  // for (std::pair<const vector<operation>, const vector<operation>> tp : input -> mempairs) {
  //   for (const operation o: tp.first) 
  //     std::cout << " " << o;
  //   std::cout << std:: endl;
  //   for (const operation o: tp.second) 
  //     std::cout << " " << o;
  //   std::cout << std:: endl;
  // }

  // for (std::pair<const vector<operation>, const vector<operation>> tp : input -> copypairs) {
  //   for (const operation o: tp.first) 
  //     std::cout << " " << o;
  //   std::cout << std:: endl;
  //   for (const operation o: tp.second) 
  //     std::cout << " " << o;
  //   std::cout << std:: endl;
  // }



  // difference between operations and branch operations
  if ((options->dist_metric() == DIST_HAMMING_DIFF_BR) ||
      (options->dist_metric() == DIST_DIFF_BR)) {
    v_diff  = int_var_array(size, -maxval, maxval);

  }
  // Prepare register for hamming distance between registers
  v_reghamm  = int_var_array(operand_size, -1, input->RA.size() - 1);
 
  if (options->dist_metric() == DIST_CYC_REG_GADGET ||
      options->dist_metric() == DIST_REG_GADGET         ||
      options->dist_metric() == DIST_CYC_GADGET) {
    v_gadget  = int_var_array(size, -maxval, maxval);
  }

  // Global cycles array - similar to cycles
  v_gc = int_var_array(op_size, 0, sum_of(input->maxc));

  // Array for levenshtein distance
  v_oc = set_var_array(sum_of(input->maxc) + 1, IntSet::empty, IntSet(0,op_size));

  dist = IntVar(*this, 0, 10000);
}


DivModel::DivModel(DivModel& cg) :
  GlobalModel(cg),
  div_p(cg.div_p),
  div_r(cg.div_r),
  mindist(cg.mindist),
  solver(cg.solver),
  branch_operations(cg.branch_operations),
  real_operations(cg.real_operations),
  gadgets(cg.gadgets),
  gadgets_operations(cg.gadgets_operations)
{
  v_diff.update(*this, cg.v_diff);
  v_hamm.update(*this, cg.v_hamm);
  v_reghamm.update(*this, cg.v_reghamm);
  v_gadget.update(*this, cg.v_gadget);
  v_gc.update(*this, cg.v_gc);
  v_oc.update(*this, cg.v_oc);
  dist.update(*this, cg.dist);
}

DivModel* DivModel::copy(void) {
  return new DivModel(*this);
}

void DivModel::set_solver(Json::Value root) {
  solver = new SolverParameters(root);
}

void DivModel::post_random_branchers(void) {
  branch(*this, cost(), INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_cost_decision);
  Rnd r;
  r.seed(options->seed());
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


}



void DivModel::post_clrandom_branchers(void) {
  Rnd r;
  r.seed(options->seed());
  //branch(*this, v_gadget, INT_VAR_RND(r), INT_VAL_RND(r),
  //       NULL, NULL);

  branch(*this, v_a, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
         NULL, &print_global_inactive_decision);

  branch(*this, v_i, INT_VAR_RND(r), INT_VAL_MIN(),
         NULL, &print_global_instruction_decision);

  branch(*this, v_y, INT_VAR_RND(r), INT_VAL_MIN(),
         NULL, &print_global_temporary_decision);

  branch(*this, v_c, INT_VAR_RND(r), INT_VAL_RND(r),
         &schedulable, &print_global_cycle_decision);

  branch(*this, v_r, INT_VAR_RND(r), INT_VAL_RND(r),
         &global_assignable, &print_global_register_decision);

}

void DivModel::post_cloriginal_branchers(void) {
  Rnd r;
  r.seed(options->seed());
  branch(*this, v_a, BOOL_VAR_NONE(), BOOL_VAL_RND(r),
         NULL, &print_global_inactive_decision);

  branch(*this, v_c, INT_VAR_NONE(), INT_VAL_MIN(),
         &schedulable, &print_global_cycle_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_instruction_decision);

  branch(*this, v_y, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_temporary_decision);

  branch(*this, v_r, INT_VAR_NONE(), INT_VAL_RND(r),
         &global_assignable, &print_global_register_decision);

}


void DivModel::post_solution_brancher(void) {

    IntArgs sol;
    IntVarArgs vs;
    IntArgs sol2;
    BoolVarArgs vs2;
    for (int c: solver->cycles) sol2 << ((c == -1) ? 0 : 1);
    for (BoolVar a: v_a) vs2 << a;

    for (int c: solver->cycles) sol << c;
    for (int r: solver->registers) sol << r;
    for (IntVar c: v_c) vs << c;
    for (IntVar r: v_r) vs << r;


    if (options->div_method() == DIV_MONOLITHIC_DFS) {
      solution_branch_dfs(*this, vs2, sol2);
      solution_branch_dfs(*this, vs, sol);
    } else {
      solution_branch(*this, vs2, sol2);
      solution_branch(*this, vs, sol);
    }
}

// int  DivModel::commit(IntVar x, int i) 
void DivModel::post_div_branchers(void) {
  if (options->enable_solver_solution_brancher() && solver->has_solution) {
    post_solution_brancher();
  }
  if (options->branching() == BR_RND) {
    post_random_branchers();
  }
  else if (options->branching() == BR_RND_COSTLAST) {
    post_clrandom_branchers();
  }
  else if (options->branching() == BR_ORIGINAL_COSTLAST) {
    post_cloriginal_branchers();
  }
  else if (options->branching() == BR_ORIGINAL) {
    GlobalModel::post_complete_branchers(options->seed()) ;
  }
}



void DivModel::post_diversification_constraints(void) {
  // TODO(Romy: Check for other distance measures 
  post_global_cycles();
  switch (options->dist_metric()) {
  case DIST_HAMMING:
    post_diversification_hamming();
    break;
  case DIST_HAMMING_DIFF:
    post_diversification_diffs();
    break;
  case DIST_HAMMING_DIFF_BR:      // same as above 
    post_diversification_diffs(); 
    break;
  case DIST_HAMMING_BR:           // Hamming distance of only branches
    post_diversification_hamming();
    break;
  case DIST_LEVENSHTEIN:
    post_diversification_channel();
    break;
  case DIST_LEVENSHTEIN_SET:
    post_diversification_channel();
    break;
  case DIST_REGHAMMING:
    post_diversification_reghamming();
    break;
  case DIST_REG_CYC_HAMMING:
    post_diversification_reghamming();
    post_diversification_hamming();
    break;
  case DIST_CYC_REG_GADGET:
    post_diversification_reghamming();
    //post_diversification_reg_gadget();
    //post_diversification_channel();
    break;
  case DIST_REG_GADGET:
    post_diversification_reghamming();
    post_diversification_reg_gadget();
    post_diversification_channel();
    break;
  case DIST_CYC_GADGET:
    post_diversification_reg_gadget();
    post_diversification_channel();
    break;
  case DIST_DIFF_BR:               // Like DIST_HAMMING_DIFF_BR but calculates the actual distance
    post_diversification_diffs();
    break;
  case DIST_HAMMING_BR_REG:        // Like HAMMING_BR but add also register
    post_diversification_hamming();
    post_diversification_reghamming();
    break;
  case DIST_COST:
    break;
  }

}


void DivModel::post_diversification_channel(void) {
  IntVarArgs bs;
  uint smaxc = sum_of(input->maxc);
  for (operation i : input->O) { //real_operations) {
    if (is_real_type(i)) {
        BoolVar ifb = var ( a(i) == 1 );
        IntVar thenb = var ( gc(i) );
        IntVar elseb = var(smaxc); //IntVar(*this, 0, smaxc); //
        // max(*this, v_gc, elseb);
        IntVar res = IntVar(*this, 0, smaxc);
        ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
        bs << res;
    } else {
        bs << var(smaxc);
    }
  }

  channel(*this, bs, v_oc);

}

void DivModel::post_diversification_diffs(void) {
  int k=0;
  int maxval = max_of(input->maxc);
  for (uint i = 0; i < real_operations.size(); i++) {
    for (uint j = i+1; j< real_operations.size(); j++) {
      // If then else constraint
      BoolVar ifb = var ((a(i) == 1) && (a(j) == 1));
      IntVar elseb = var (maxval) ;
      IntVar thenb =  var (gc(i) - gc(j));
      ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
      k++;
    }
  }
}

void DivModel::post_diversification_br_diffs(void) {
  int maxval = max_of(input->maxc);
  int prevbr = 0; 
  int k=0;
  for (operation br: branch_operations) {
    for (operation o: real_operations) { 
      if (br == o) continue;
      BoolVar ifb = var ((a(br) == 1) && (a(o) == 1) && (gc(o) > gc(prevbr)) && (gc(o) <= gc(br)));
      IntVar thenb =  var (gc(br) - gc(o));
      IntVar elseb = var (maxval);
      ite(*this, ifb,  thenb, elseb, diff(k), IPL_DOM);
      k++;
    }
    prevbr = br;
  }

}


void DivModel::post_diversification_reg_gadget(void) {
  int maxval = max_of(input->maxc);
  //int prevbr = 0; 
  int k=0;
  for (operation br: branch_operations) {
    for (operation o: real_operations) { 
      if (br == o) continue;
      BoolVar ifb = var ((a(br) == 1) && (a(o) == 1) && (gc(o) <= gc(br))); //(gc(o) > gc(prevbr)) && (gc(o) < gc(br)));
      //IntVar thenb =  gc(o);
      IntVar thenb =  var (gc(br) - gc(o));
      IntVar elseb = var (maxval);
      ite(*this, ifb,  thenb, elseb, gadget(k), IPL_DOM);
      k++;
    }
    //prevbr = br;
  }

}


void DivModel::post_diversification_reghamming(void) {
  for (operand p: input->P) {
    constraint(reghamm(p) == ry(p));
  }
}


void DivModel::post_diversification_hamming(void) {
  for (operation i : real_operations) {
    BoolVar ifb = var (a(i) == 1);
    IntVar thenb = var ( gc(i) );
    IntVar elseb = var ( -1 );
    ite(*this, ifb,  thenb, elseb, hamm(i), IPL_DOM);
  }
}

void DivModel::post_global_cycles(void) {
  // VarInt offset;
  for (block b: input->B) {
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
  uint sizex = v_oc.size(); // size of maxc
  // uint num_gadgets = branch_operations.size();

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

  dist = var( mat(sizex-1, sizex-1));

  constraint(dist >= mindist); // Levenshtein distance
}

void DivModel::post_levenshtein_set(const DivModel & b)
{
  uint sizex = v_oc.size();// + 1; // size of maxc
  // int op_size = O().size();
  IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
  Matrix<IntVarArray> mat(x, sizex, sizex);
  uint maxcap = max_of(input->cap);

  IntVarArray cap = int_var_array(sizex-1, 0, maxcap);
  IntVarArray bcap = int_var_array(sizex-1, 0, maxcap);


  for (uint i = 0; i < sizex-1; i++) {
    cap[i] = var(cardinality(oc(i)));
    bcap[i] = var(cardinality(b.oc(i)));
  }

  mat(0,0) = var(0);
  for (uint i = 1; i < sizex; i++) {
    IntVar nw = cap[i-1]; //var(cardinality(oc(i-1)));
    IntVar old = bcap[i-1]; //var(cardinality(b.oc(i-1)));
    mat(i,0) = var( mat(i-1,0) +  nw);
    mat(0,i) = var( mat(0,i-1) + old);
  }

  for (uint i = 1; i < sizex; i++)
    for (uint j = 1; j < sizex; j++) {
      IntVarArgs cs;
      cs << var (cardinality (oc(i-1) - b.oc(j-1)));
      cs << var (cardinality (b.oc(j-1) - oc(i-1)));
      IntVar res = IntVar(*this, 0, maxcap);
      max(*this, cs, res);

      IntVarArgs v;
      v << var (mat(i-1,j) + cap[i-1]); //cardinality(oc(i-1)));
      v << var (mat(i,j-1) + bcap[i-1]); //cardinality(b.oc(j-1)));
      v << var (mat(i-1,j-1) + res);
      min(*this, v, mat(i,j));
    }

  dist = var( mat(sizex-1, sizex-1));
  constraint( dist >= mindist); // Levenshtein distance
}
bool DivModel::is_real_type(int o) {

  return (input->type[o] == BRANCH ||
          input->type[o] == LINEAR ||
          input->type[o] == CALL ||
          input->type[o] == TAILCALL ||
          input->type[o] == COPY);
}

bool DivModel::is_branch_type(int o) {
//  bool may_branch = input->type[o] == BRANCH || input->type[o] == TAILCALL || input->type[o] == CALL;
//  return may_branch;
 
    
  bool is_jal = false;
  bool may_branch = input->type[o] == BRANCH || input->type[o] == TAILCALL || input->type[o] == CALL;
  
  if (may_branch) {
    string ins1 (input->insname[input->instructions[o][0]]);
    if ((ins1.compare(0,4,"JALR") == 0) || 
        (ins1.compare(0,2,"JR") == 0) || 
        (ins1.compare(0,12,"PseudoReturn") == 0))
        is_jal = true;
  }
  return is_jal;

}

void DivModel::constrain_solution(DivModel *b) {

  BoolVarArgs cs;
  for (operation o: real_operations) {
      if (b->hamm(o).assigned())
	cs << var (hamm(o) != b->hamm(o));
    }
  if (cs.size() >0) {           //
      dist = var( sum(cs));
      constraint(dist >= mindist); // 
  }
  //for (operand p: input->P) {
  //  if (b->ry(p).assigned())
  //      cs << var(ry(p) != b->ry(p));
  //}
 
  return;

}



void DivModel::constrain(const Space & _b) {
  const DivModel& b = static_cast<const DivModel&>(_b);


  int maxval = max_of(input->maxc);
  BoolVarArgs bh;
  IntVarArgs ih;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (operation o: real_operations) {
      if (b.hamm(o).assigned())
	bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0) {
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance

    } else {
      cerr << "No constraints @ constrain";
      //exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_DIFF:
    for (int i = 0; i < v_diff.size(); i++) {
      if (b.diff(i).assigned())
	bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0) {
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance
    } else {
      cerr << "No constraints @ constrain";
      //exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_DIFF_BR:
    // for (uint j = 0; j < gadgets.size(); j++) {
    //   gadget_t g = gadgets[j];
    //   operation br = branch_operations[j];
    //   BoolVarArgs btemp;
    //   for (uint i = g.start; i < g.end; i++) {
    //     btemp << var (diff(i) != b.diff(i));
    //   }
    //   // int br = 0;
    //   rel(*this, var(sum(btemp)), IRT_GQ,  var(1), var(a(br) == 1));
    //   // constraint( var(sum(btemp) >= 1));
    // }
    for (int i = 0; i < v_diff.size(); i++) { //
      if (b.diff(i).assigned())
	bh << var (diff(i) != b.diff(i));
    }
    if (bh.size() >0) {
      dist = var(sum(bh));
      constraint(dist >= mindist); // hamming distance
    } else {
      cerr << "No constraints @ constrain";
      //exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR:
    for (operation o : branch_operations) {
      if (b.hamm(o).assigned())
	bh << var (hamm(o) != b.hamm(o));
    }
    if (bh.size() >0) {
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance on the branches
    } else {
      cerr << "No constraints @ constrain";
      // exit(EXIT_FAILURE);
    }
    break;
  case DIST_LEVENSHTEIN:
    post_levenshtein(b);
    break;

  case DIST_LEVENSHTEIN_SET:
    post_levenshtein_set(b);
    break;
  case DIST_REGHAMMING:
    for (operand p : input->P) {
      if (b.reghamm(p).assigned())
	bh << var (reghamm(p) != b.reghamm(p));
      if (b.x(p).assigned())
	bh << var (x(p) != b.x(p));
    }
    if (bh.size() >0) {           //
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance

    } else {
      cerr << "No constraints @ constrain";
      // exit(EXIT_FAILURE);
    }
    break;

  case DIST_REG_CYC_HAMMING:
    for (operation o: real_operations) {
      if (b.hamm(o).assigned())
	bh << var (hamm(o) != b.hamm(o));
    }
    for (operand p : input->P) {
      if (b.reghamm(p).assigned())
	bh << var (reghamm(p) != b.reghamm(p));
      if (b.x(p).assigned())
	bh << var (x(p) != b.x(p));
    }
    if (bh.size() >0) {
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance

    } else {
      cerr << "No constraints @ constrain";
      //exit(EXIT_FAILURE);
    }
    break;

  case DIST_CYC_GADGET:
    {
      int order = 1;
      IntArgs cycleorder = IntArgs::create(v_c.size(),-1,0);
      for (int i = 0; i < b.v_oc.size(); i++) {
	if (b.v_oc[i].assigned() && (b.v_oc[i].lubSize() == 1 || b.v_oc[i].lubSize() == 2)) {
	  int oper = b.v_oc[i].lubMin();
	  int oper2 = b.v_oc[i].lubMax();
	  if (oper < cycleorder.size()) {
	    cycleorder[oper] = order;
	    if ((oper2 < cycleorder.size()) && (oper!= oper2) ) {
              cycleorder[oper2] = order;
	    }
	    order++;
	  }
	} 
      }

      for (uint j = 0; j < gadgets.size(); j++) {
	gadget_t g = gadgets[j];
	operation br = branch_operations[j];
	if (cycleorder[br] == -1) {
	  cout << "No branch" << br << endl;
	  continue;
	}
	BoolVarArgs btemp, rtemp;
	IntArgs bweight;
	//IntArgs rweight;
	for (uint i = g.start; i < g.end; i++) {
	  operation o = gadgets_operations[i];
	  if (cycleorder[o] == -1) continue;
	  // int weight_b = cycleorder[o];
          int orderi = cycleorder[br] - cycleorder[o];
	  int weight_b = (orderi >= 0 && orderi <= (int)options->cyc_gadget_size()) ? 1 : 0; 
	  // cerr << "br" << br << ":" << cycleorder[br] << " o:" << o << ":" << cycleorder[o] << " weight_b: " << weight_b << endl;

	  //weight_b = weight_b == 0 ? 1 : weight_b;
	  if (weight_b == 0) continue;
	  if (b.gadget(i).assigned()) {
	    btemp << var (gadget(i) != b.gadget(i));
	    bweight << weight_b*maxval; //(1*maxval)/(weight_b); //b.gadget(i).val();
	  }
	}
	if (btemp.size() >0) {
	  BoolVar rb = var(a(br) == 1);
	  Reify r(rb, RM_IMP);
	  linear(*this, bweight, btemp, IRT_GQ, maxval, r);
	  ih << var(sum(btemp));
	}
      }
      if (ih.size() >0) {
	dist = var(sum(ih));
	constraint(dist >= mindist);
      } else {
	cerr << "No constraints @ constrain" << endl;
	exit(EXIT_FAILURE);
      }
    }
    break;
  case DIST_CYC_REG_GADGET:
/*    {
      int order = 1;
      IntArgs cycleorder = IntArgs::create(v_c.size(),-1,0);
      for (int i = 0; i < b.v_oc.size(); i++) {
	if (b.v_oc[i].assigned() && (b.v_oc[i].lubSize() == 1 || b.v_oc[i].lubSize() == 2)) {
          //cout << b.v_oc[i] << endl;
	  int oper = b.v_oc[i].lubMin();
	  int oper2 = b.v_oc[i].lubMax();
	  if (oper < cycleorder.size()) {
	    cycleorder[oper] = order;
	    if ((oper2 < cycleorder.size()) && (oper!= oper2) ) {
              cycleorder[oper2] = order;
	    }
	    order++;
	  }
	} 
      }

      for (uint j = 0; j < gadgets.size(); j++) {
	gadget_t g = gadgets[j];
	operation br = branch_operations[j];
	if (cycleorder[br] == -1) {
	  cout << "No branch" << br << endl;
	  continue;
	}
	BoolVarArgs btemp, rtemp;
	IntArgs bweight;
	//IntArgs rweight;
	for (uint i = g.start; i < g.end; i++) {
	  operation o = gadgets_operations[i];
	  //int weight_r = (cycleorder[br] - cycleorder[o]);
	  if (cycleorder[o] == -1) continue;
          int orderi = cycleorder[br] - cycleorder[o];
	  //int weight_r = (orderi == 0) ? 1 : 0; 
	  int weight_b = (orderi >= 0 && orderi <= (int)options->cyc_gadget_size() ) ? 1 : 0; 
	  int weight_r = (orderi >= 0 && orderi <= (int)options->reg_gadget_size() ) ? 1 : 0; 

	  if (weight_r == 0 && weight_b == 0) continue;
	  if (b.gadget(i).assigned()) {
	    btemp << var (gadget(i) != b.gadget(i));
	    bweight << weight_b*maxval; //(1*maxval)/(weight_b); //b.gadget(i).val();
	  }

        
	  for (operand p: input->operands[o]) {
	    if (b.reghamm(p).assigned() && b.reghamm(p).val() !=0 ) {
	      btemp << var (reghamm(p) != b.reghamm(p));
	      bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
	    } 
	    if (b.x(p).assigned()) {
	      btemp << var (x(p) != b.x(p));
	      bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
            }
	  }
	}

	if (btemp.size() >0) {
	  BoolVar rb = var(a(br) == 1);
	  Reify r(rb, RM_IMP);
	  linear(*this, bweight, btemp, IRT_GQ, maxval, r);
	  ih << var(sum(btemp));
	}
      }
      if (ih.size() >0) {
	dist = var(sum(ih));
	constraint(dist >= mindist);
      } else {
	cerr << "No constraints @ constrain" << endl;
	exit(EXIT_FAILURE);
      }
    }*/

    {
      
      for (uint j = 0; j < gadgets.size(); j++) {
	gadget_t g = gadgets[j];
	operation br = branch_operations[j];
	/*if (cycleorder[br] == -1) {
	  cout << "No branch" << br << endl;
	  continue;
	}*/
	BoolVarArgs btemp, rtemp;
	IntArgs bweight;
	//IntArgs rweight;
	for (uint i = g.start; i < g.end; i++) {
	  operation o = gadgets_operations[i];
	  //int weight_r = (cycleorder[br] - cycleorder[o]);
	  //if (cycleorder[o] == -1) continue;
          if (!b.gc(o).assigned()) 
            continue;

          int orderi = b.gc(br).val() - b.gc(o).val(); // cycleorder[br] - cycleorder[o];
        
	  //int weight_r = (orderi == 0) ? 1 : 0; 
	  int weight_b = (orderi >= 0 && orderi <= (int)options->cyc_gadget_size() ) ? 1 : 0; 
	  int weight_r = (orderi >= 0 && orderi <= (int)options->reg_gadget_size() ) ? 1 : 0; 

	  if (weight_r == 0 && weight_b == 0) continue;
	  if (b.gc(o).assigned()) {
	    btemp << var (gc(o) != b.gc(o));
	    bweight << weight_b*maxval; //(1*maxval)/(weight_b); //b.gadget(i).val();
	  }

        
	  for (operand p: input->operands[o]) {
	    if (b.reghamm(p).assigned() && b.reghamm(p).val() !=0 ) {
	      btemp << var (reghamm(p) != b.reghamm(p));
	      bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
	    } 
	    if (b.x(p).assigned()) {
	      btemp << var (x(p) != b.x(p));
	      bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
            }
	  }
	}

	if (btemp.size() >0) {
	  BoolVar rb = var(a(br) == 1);
	  Reify r(rb, RM_IMP);
	  linear(*this, bweight, btemp, IRT_GQ, maxval, r);
	  ih << var(sum(btemp));
	}
      }
      if (ih.size() >0) {
	dist = var(sum(ih));
	constraint(dist >= mindist);
      } else {
	cerr << "No constraints @ constrain" << endl;
	exit(EXIT_FAILURE);
      }
    }
    break;
case DIST_REG_GADGET:
{
  int order = 1;
  IntArgs cycleorder = IntArgs::create(v_c.size(),-1,0);
  for (int i = 0; i < b.v_oc.size(); i++) {
    if (b.v_oc[i].assigned() && (b.v_oc[i].lubSize() == 1 || b.v_oc[i].lubSize() == 2)) {
      //cout << b.v_oc[i] << endl;
      int oper = b.v_oc[i].lubMin();
      int oper2 = b.v_oc[i].lubMax();
      if (oper < cycleorder.size()) {
        cycleorder[oper] = order;
        if ((oper2 < cycleorder.size()) && (oper!= oper2) ) {
          cycleorder[oper2] = order;
        }
        order++;
      }
    } 
  }

  for (uint j = 0; j < gadgets.size(); j++) {
    gadget_t g = gadgets[j];
    operation br = branch_operations[j];
    if (cycleorder[br] == -1) {
      cout << "No branch" << br << endl;
      continue;
    }
    BoolVarArgs btemp, rtemp;
    IntArgs bweight;
    //IntArgs rweight;
    for (uint i = g.start; i < g.end; i++) {
      operation o = gadgets_operations[i];
      //int weight_r = (cycleorder[br] - cycleorder[o]);
      if (cycleorder[o] == -1) continue;
      int orderi = cycleorder[br] - cycleorder[o];
      //int weight_r = (orderi == 0) ? 1 : 0; 
      int weight_r = (orderi >= 0 && orderi <= (int)options->reg_gadget_size() ) ? 1 : 0; 

      //cerr << "br" << br << ":" << cycleorder[br] << " o:" << o << ":" << cycleorder[o] << " weight_r: " << weight_r << endl;
      //weight_r = weight_r == 0 ? 1 : weight_r;
      if (weight_r == 0) continue;
      //int weight = b.gadget(i).val();
    
        //cout << o << endl;
        //cout << (input->insname[input->instructions[o][0]]) << endl;

      for (operand p: input->operands[o]) {
        //cout << "rhamm:" << b.reghamm(p) << endl;
        if (b.reghamm(p).assigned() && b.reghamm(p).val() !=0 ) {
          //cerr << "o" << o << " p:" << p << " reghamm:" << b.reghamm(p) << endl;
          btemp << var (reghamm(p) != b.reghamm(p));
          //bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
          bweight << (weight_r * maxval); //(2*maxval)/(3*weight_r*weight_r); 
        } 
        if (b.x(p).assigned()) {
          btemp << var (x(p) != b.x(p));
          bweight << (weight_r * maxval); 
        }

      }
        //cout << btemp << endl;
        //cout << bweight << endl;
 
    }
    if (btemp.size() >0) {
      BoolVar rb = var(a(br) == 1);
      Reify r(rb, RM_IMP);
      //for (int ii = 0; ii < bweight.size(); ii++)
      //  bweight[ii] = bweight[ii]/bweight.size() + 1;
       //cerr << "Weight" << endl;
       //cerr << bweight << endl;
       //cerr << maxval << endl;

      linear(*this, bweight, btemp, IRT_GQ, maxval, r);
      ih << var(sum(btemp));
    }
  }
  if (ih.size() >0) {
    dist = var(sum(ih));
    constraint(dist >= mindist);
  } else {
    cerr << "No constraints @ constrain" << endl;
    exit(EXIT_FAILURE);
  }
}
break;

/// END TEST
  case DIST_HAMMING_BR_REG:
    for (operation o : branch_operations) {
      BoolVarArgs btemp;
      if (b.hamm(o).assigned()) {
	btemp << var (hamm(o) != b.hamm(o));
	bh << var (hamm(o) != b.hamm(o));
	for (operand p: input->operands[o]) {
	  if (b.reghamm(p).assigned()) {
	    btemp << var (reghamm(p) != b.reghamm(p));
	    bh << var (reghamm(p) != b.reghamm(p));
	  }
	  if (b.x(p).assigned()) {
	    btemp << var (x(p) != b.x(p));
	    bh << var (x(p) != b.x(p));
          }
	}
      }
      if (btemp.size() >0) {
        constraint(sum(btemp) >= 1); // hamming distance on the branches
      }
    }
    if (bh.size() >0) {
      dist = var( sum(bh));
      constraint(dist >= mindist); // hamming distance on the branches
    } else {
      cerr << "No constraints @ constrain";
      // exit(EXIT_FAILURE);
    }
    break;

  case DIST_DIFF_BR:
    for (int i = 0; i < v_diff.size(); i++) { //
      if (b.diff(i).assigned())
	ih << var (abs (diff(i) - b.diff(i)));
    }
    if (ih.size() >0) {
      dist = var(sum(ih));
      constraint(dist >= mindist); // hamming distance
    } else {
      cerr << "No constraints @ constrain";
      // exit(EXIT_FAILURE);
    }
    break;
  case DIST_COST:
    for (uint i = 0; i< input->N; i++)
      if (b.cost()[i].assigned())
	bh << var (cost()[i] != b.cost()[i]);
    
    if (bh.size() > 0) {
        constraint(sum(bh) >= mindist);
    } else {
      cerr << "No constraints @ constrain";
    }
        
    break;
  } // switch

  
  return;

}



bool DivModel::master(const MetaInfo& mi) {
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



bool DivModel::slave(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    post_complete_branchers(mi.asset());
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if ((mi.restart() > 0) && (div_p > 0.0)) {
      if (mi.last() != NULL)// {
        next(static_cast<const DivModel&>(*mi.last()));
      return false;
    } else if (mi.restart() == 0) {
      first();
      return true;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}


void DivModel::first(void) {

  return;
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
    relax(*this, temp, ltemp, div_r, div_p);
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

