/*
 *  Main authors:
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  Copyright (c) 2022, Rodothea Myrsini Tsoupidi
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


#include "seclocalmodel.hpp"


SecLocalModel::SecLocalModel(Parameters * p_input, ModelOptions * p_options,
			     IntPropLevel p_ipl,
			     const SecModel * gs, block p_b) :
  LocalModel(p_input, p_options, p_ipl, gs, p_b)
{
  if (options -> enable_power_constraints()) {
    int temp_size = T().size(); // These come from LocalModel
    int gltemp_size = input -> T.size(); // These come from LocalModel
    int op_size = O().size();
    int maxval = sum_of(input->maxc);
    int reg_size = input->HR.size();

    // Find mem operations
  
    vector<string> memstrings = {"tSTRspi_fi", "tLDRspi_fi", "SW_fi", "LW_fi"}; 
    for (operation o : O()) { 
      if (input -> type[o] == COPY)
	memops.push_back(o);
      else {
	for(instruction i : input-> instructions[o]) {
	  if (contains(memstrings, input -> insname[i])) {
	    memops.push_back(o);
	    break;
	  }
	}
      }
    }

    // for(instruction i1 : input-> instructions[o1]) {
    // 	  if ( contains(memstrings, (input -> insname[i1]))
	    
    // 	  vector<operand> ps = input -> operands[o1];
    // 	  int cls = input -> rclass[o][i][p];
    // 	  char * classname = input -> classname[cls];
    // 	  vector<instruction> ins = input-> instructions[o1];
    // 	}
    // 	  // vector<temporaries> temps = input -> temps[ops[0]];
    // 	for (operation o2 : O()) {
    // 	  if (o1 != o2) {
    // 	    BoolVar ifb  = var(a(o2) && (c(o2) <= c(o1)));
    // 	    IntVar thenb = var( c(o2) );
    // 	    IntVar elseb = var( -1 ); 
    // 	    IntVar res = IntVar(*this, -1, maxval);
    // 	    ite(*this, ifb,  thenb, elseb, res, IPL_BND);
    // 	    lts <<  res;
    // 	  }
    // 	}
    // 	max(*this, lts, v_ok[instr(o1)]);
    //   }
    // }

  
    // Implementation 2
    v_lk = int_var_array(temp_size, -1, maxval);
    v_ok = int_var_array(op_size, -1, maxval);

    v_tat = int_var_array(temp_size, -1, gltemp_size);
    v_tbt = int_var_array(temp_size, -1, gltemp_size);
  
    post_r2_constraints();
    post_m2_constraints();

    // Implementation 1
    v_rtle = int_var_array(reg_size * temp_size, -1, maxval);
    v_rtlemap = int_var_array(reg_size * temp_size, -1, maxval);

    v_opcy = int_var_array(op_size, -1, maxval);
    v_opcymap = int_var_array(op_size, -1, maxval);

    apply_sec_solution(gs);

    post_r1_constraints();
    post_m1_constraints();

  }
  post_security_constraints();
}

SecLocalModel::SecLocalModel(SecLocalModel& cg) :
  LocalModel(cg),
  memops(cg.memops)
{
  // Implementation 1
  v_rtle.update(*this, cg.v_rtle);
  v_rtlemap.update(*this, cg.v_rtlemap);
  v_opcy.update(*this, cg.v_opcy);
  v_opcymap.update(*this, cg.v_opcymap);
  
  // Implementation 2
  v_lk.update(*this, cg.v_lk);
  v_ok.update(*this, cg.v_ok);  
  v_tat.update(*this, cg.v_tat);
  v_tbt.update(*this, cg.v_tbt);  

}


bool SecLocalModel::master(const MetaInfo& mi) {
  assert(mi.type() == MetaInfo::PORTFOLIO);
  return true;
}

bool SecLocalModel::slave(const MetaInfo& mi) {
  assert(mi.type() == MetaInfo::PORTFOLIO);
  string portfolio = options->local_portfolio();
  assert(mi.asset() < portfolio.size());
  char search = portfolio[mi.asset()];
  post_branchers(search);
  return true;
}


void SecLocalModel::post_sec_brancher(void) {
  if (!options-> disable_sec_secret_constraints()) {
    BoolVarArgs ts;

    for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
      temporary tsec = tp.first;
      int size = T().size();
      if (temp(tsec) < size && temp(tsec) >= 0) {
	ts << l(tsec);
      }
    }
    if (ts.size() > 0) 
      branch(*this, ts, BOOL_VAR_NONE(), BOOL_VAL_MIN(),
	     NULL, NULL);
  }
  if (!options-> disable_sec_regreg_constraints()) {
  }
  if (!options-> disable_sec_mem_constraints()) {
  }
}

void SecLocalModel::post_sec_branchers(void) {

  set<operation> is0;
  set<temporary> ts0;
  set<register_atom> rs0;
  BoolVarArgs rrs;
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    int temp_size = T().size();
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    operand p1 = input -> definer[t1];
    operand p2 = input -> definer[t2];
    vector<operand> us1 = input -> users[t1];
    vector<operand> us2 = input -> users[t2];
    operation o1 = input -> oper[p1];
    operation o2 = input -> oper[p2];
    block b1 = input -> oblock[o1];
    block b2 = input -> oblock[o2];

    if (b1 != b  || b2 != b) continue;

    // Enable or not the instruction
    if (is_optional(o1)) is0.insert(o1);
    if (is_optional(o2)) is0.insert(o2);

    // select temporary from uses of t1 and t2
    ts0.insert(p1);
    ts0.insert(p2);
    for (operand p : us1) ts0.insert(p);
    for (operand p : us2) ts0.insert(p);

    rs0.insert(t1);
    rs0.insert(t2);
    // rrs << var(r(t1) == r(t2));

  }
  vector<operation> os(is0.begin(), is0.end());
  BoolVarArgs as;
  IntVarArgs is;
  for (operation o : os) {
    as << a(o);
    is << i(o);
  }

  vector<operation> ts1(ts0.begin(), ts0.end());
  IntVarArgs ts;
  for (operand p : ts1) {
    ts << y(p);
  }

  vector<operation> rs1(rs0.begin(), rs0.end());
  IntVarArgs rs;
  BoolVarArgs ls;
  for (temporary t : rs1) {
    rs << r(t);
    ls << l(t);
  }

  // branch(*this, rrs, BOOL_VAR_DEGREE_MAX(), BOOL_VAL_MIN(),
  //        NULL, NULL);
  branch(*this, ls, BOOL_VAR_DEGREE_MAX(), BOOL_VAL_MIN(),
         NULL, NULL);
  branch(*this, rs, INT_VAR_DEGREE_MAX(), INT_VAL_MIN(),
         NULL, NULL);
  branch(*this, as, BOOL_VAR_DEGREE_MAX(), BOOL_VAL_MIN(),
         NULL, NULL);
  branch(*this, ts, INT_VAR_DEGREE_MAX(), INT_VAL_MIN(),
         NULL, NULL);
  branch(*this, is, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, NULL);
  
  // branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
  //        NULL, &print_instruction_decision);

  // branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  // branch_on_pressure_scheduling(*this, v_c);

  // branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
  //        &print_register_decision);

  // branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);
  
  IntVarArgs ts2;
  for (operand p : input->groupcopyrel[b]) ts2 << y(p);
  branch(*this, ts2, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_temporary_decision);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
         &print_register_decision);
  
  branch(*this, v_a, BOOL_VAR_MERIT_MAX(actionmerit), BOOL_VAL_MIN(),
         NULL, &print_inactive_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_instruction_decision);

  branch(*this, v_c, INT_VAR_MIN_MIN(), INT_VAL_MIN(),
         &schedulable, &print_cycle_decision);


}

void SecLocalModel::post_branchers(char search) {
  post_sec_brancher();
  switch (search) {
  case SECURE_SEARCH:
    post_sec_branchers();
    break;
  default:
    LocalModel::post_branchers(search);
  }
}


SecLocalModel* SecLocalModel::copy(void) {
  return new SecLocalModel(*this);
}



BoolVar SecLocalModel::subseq1(temporary t1, temporary t2) {
  int temp_size = T().size();
  BoolVarArgs b;
  for (register_atom ra: input -> HR) { // Hardware registers
    b << var( (v_rtle[(temp_size*ra) + temp(t1)] != -1)
	      && (v_rtlemap[(temp_size*ra) + temp(t1)] + 1 ==
		  v_rtlemap[(temp_size*ra) + temp(t2)]));

  }
  return var( sum(b) > 0 );
}


BoolVar SecLocalModel::msubseq1(operation o1, operation o2) {
  return var ((v_opcy[instr(o1)] != -1) &&
	      (v_opcymap[instr(o1)] + 1 == v_opcymap[instr(o2)]));
}


BoolVar SecLocalModel::subseq2(temporary t1, temporary t2) {
  return var (l(t1) && l(t2) && (r(t1) == r(t2))
	      && (v_lk[temp(t2)] == le(t1)));
}


BoolVar SecLocalModel::msubseq2(operation o1, operation o2) {
  assert( contains(memops, o1) && contains(memops, o2));
  return var (a(o1) && a(o2) && v_ok[instr(o2)] == c(o1));
}


BoolVar SecLocalModel::msubseq(operation o1, operation o2) {
  if (options-> sec_implementation() == SEC_R1_M1 ||
      options-> sec_implementation() == SEC_R2_M1) 
    return msubseq1(o1, o2);
  else
    return msubseq2(o1, o2);
}


BoolVar SecLocalModel::subseq(temporary t1, temporary t2) {
  if (options-> sec_implementation() == SEC_R1_M1 ||
      options-> sec_implementation() == SEC_R1_M2) 
    return subseq1(t1, t2);
  else
    return subseq2(t1, t2);
}



void SecLocalModel::post_m1_constraints(void) {
  int maxval = sum_of(input->maxc);
  int op_size = O().size();
  if (options -> sec_implementation() == SEC_R1_M1 ||
      options -> sec_implementation() == SEC_R2_M1)
    {
      // IntVarArgs lts;
      IntVarArray sorted_lts = int_var_array(op_size, -1, maxval);
      // IntVarArray os_map = int_var_array(op_size, -1, maxval);
      for (operation o: memops) { // Hardware registers
	int mem = input -> instructions[o][input -> instructions[o].size() - 1];
	BoolVar if1 = (input -> type[o] == COPY) ? var(i(o) == mem) : var(i(o) == i(o));
	BoolVar ifb  = var((a(o) == 1) && if1); 
	IntVar thenb = var(c(o));
	IntVar elseb = var(-1); 
	ite(*this, ifb,  thenb, elseb, v_opcy[instr(o)], IPL_DOM);
      }
      sorted(*this, v_opcy, sorted_lts, v_opcymap, IPL_DOM);
    }
}

 
void SecLocalModel::post_r1_constraints(void) {
  int maxval = sum_of(input->maxc);
  int temp_size = T().size();
  if (options -> sec_implementation() == SEC_R1_M1 ||
      options -> sec_implementation() == SEC_R1_M2)
    {
      for (register_atom ra: input -> HR) { // Hardware registers
	IntVarArgs lts;
	IntVarArray sorted_lts = int_var_array(temp_size, -1, maxval);
	IntVarArray rs_map = int_var_array(temp_size, -1, maxval);
	for (temporary t: T()) {
	  BoolVar ifb  = var(l(t) && (r(t) == ra)); 
	  IntVar thenb = var( ls(t) );
	  IntVar elseb = var( -1 ); 
	  IntVar res = IntVar(*this, -1, maxval);
	  ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
	  constraint(v_rtle[ra*temp_size + temp(t)] == res);
	  constraint(v_rtlemap[ra*temp_size + temp(t)] == rs_map[temp(t)]);
	  lts <<  res;
	}
	sorted(*this, lts, sorted_lts, rs_map, IPL_DOM);
      }
    }
}

void SecLocalModel::post_r2_constraints(void) {

  int maxval = sum_of(input->maxc);
  if (options -> sec_implementation() == SEC_R2_M2 ||
      options -> sec_implementation() == SEC_R2_M1)
    {
      for (temporary t1 : T()) { 
	IntVarArgs lts;
 	for (temporary t2 : T()) {
      	  if (t1 != t2) {
      	    BoolVar ifb  = var(l(t2) && r(t1) == r(t2) &&
      			       le(t2) <= ls(t1));
      	    IntVar thenb = var(le(t2));
      	    IntVar elseb = var(-1); 
      	    IntVar res = IntVar(*this, -1, maxval);
      	    ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
      	    lts << res;
      	  }
	}
	max(*this, lts, v_lk[temp(t1)], IPL_DOM);
      }
    }
}

void SecLocalModel::post_m2_constraints(void) {
  int maxval = sum_of(input->maxc);
  if (options -> sec_implementation() == SEC_R2_M2 ||
      options -> sec_implementation() == SEC_R1_M2)
    {
      for (operation o1 : memops) { 
	IntVarArgs lts;
	for (operation o2 : memops) {
	  if (o1 != o2) {
	    int mem = input -> instructions[o2][input -> instructions[o2].size() - 1];
	    BoolVar if1 = (input -> type[o2] == COPY) ? var(i(o2) == mem) : var(i(o2) == i(o2));
	    BoolVar ifb  = var(a(o2) && if1 && (c(o2) <= c(o1)));
	    IntVar thenb = var( c(o2) );
	    IntVar elseb = var( -1 ); 
	    IntVar res = IntVar(*this, -1, maxval);
	    ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
	    lts <<  res;
	  }
	}
	max(*this, lts, v_ok[instr(o1)], IPL_DOM);
      }
    }
}




void SecLocalModel::post_random_register_constraints(void) {
  // These pairs should not be in the same register or should not be consequent
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    int temp_size = T().size();
    if (temp(t1) < temp_size && temp(t2) < temp_size &&
	temp(t1) >= 0 && temp(t2) >= 0 ) 
      constraint((l(t1) && l(t2) && (r(t1) == r(t2))) >>
		 (!subseq(t1,t2) && !subseq(t2,t1)));
  }
}


void SecLocalModel::apply_sec_solution(const SecModel * gs) {
  // for (operation o : input->ops[b]) {
  //   // copy_domain(*this, gs->v_ok[o], v_ok[instr(o)]);
  // }

  for (temporary t : input->tmp[b]) {
    // copy_domain(*this, gs->v_lk[t], v_lk[temp(t)]);
    copy_domain(*this, gs->v_tat[t], v_tat[temp(t)]);
    copy_domain(*this, gs->v_tbt[t], v_tbt[temp(t)]);
  }

}



// void SecLocalModel::post_random_register_constraints(void) {
//   // These pairs should not be in the same register or should not be consequent
//   for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
//     temporary t1 = tp.first;
//     temporary t2 = tp.second;
//     // int temp_size = T().size();
//     if (is_in(t1) && is_in(t2)) 
//       constraint((l(t1) && l(t2) && (r(t1) == r(t2))) >>
// 		 (!subseq(t1,t2) && !subseq(t2,t1)));
//     else if (is_in(t1) && !is_in(t2)) {
//       // std::cout << t2 << std::endl;
//       operand p2 = input -> definer[t2];
//       operation o2 = input -> oper[p2];
//       block b2 = input -> oblock[o2];
//       if (b2 > b) {
//     	if (input->out[b] <= input->oper[input->definer[t1]] + 2)
//     	  continue;
//     	BoolVarArgs bs;
//     	for (const temporary t: T()) {
//     	  bs << var (l(t) && subseq(t1,t));
//     	}
//     	if (bs.size() > 0)
//     	  constraint( l(t1) >> (sum(bs) > 0));

//       }
//       else {
//       	if (input->in[b] >= input->oper[input->definer[t1]] - 2)
//       	  continue;
//       	BoolVarArgs bs;
//       	for (const temporary t: T()) {
//       	  bs << var (l(t) && subseq(t,t1));
//       	}
//       	if (bs.size() > 0)
//       	  constraint( l(t1) >> (sum(bs) > 0));
//       }

//     }
//     else if (is_in(t2) && !is_in(t1)) {
//       // std::cout << t1 << std::endl;
//       operand p1 = input -> definer[t1];
//       operation o1 = input -> oper[p1];
//       block b1 = input -> oblock[o1];
//       if (b1 > b) {
//     	if (input->out[b] <= input->oper[input->definer[t2]] + 2)
//     	  continue;
//     	BoolVarArgs bs;
//     	for (const temporary t: T()) {
//     	  bs << var (l(t) && subseq(t2,t));
//     	}
//     	if (bs.size() > 0)
//     	  constraint( l(t2) >> (sum(bs) > 0));

//       }
//       else {
//     	if (input->in[b] >= input->oper[input->definer[t2]] - 2)
//     	  continue;
//     	BoolVarArgs bs;
//     	for (const temporary t: T()) {
//     	  bs << var (l(t) && subseq(t,t2));
//     	}
//     	if (bs.size() > 0)
//     	  constraint( l(t2) >> (sum(bs) > 0));
//       }
//     }
//   }
// }



void SecLocalModel::post_secret_register_constraints(void) {
  // Temporaries that are secret should be preceeded by a random
  for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
    BoolVarArgs b1;
    BoolVarArgs b2;
    temporary tsec = tp.first;
    int temp_size = T().size();
    // std::cout << tsec << "| ";
    if (temp(tsec) < temp_size && temp(tsec) >= 0) {
      for (const temporary trand: tp.second) {
	if (temp(trand) < temp_size && temp(trand) >= 0) {
	  b1 << var (l(trand) && subseq(trand,tsec));
	  b2 << var (l(trand) && subseq(tsec,trand));
	}
      }
      if (b1.size() > 0)
	constraint( l(tsec) >> (sum(b1) > 0));
      if (b2.size() > 0)
	constraint( l(tsec) >> (sum(b2) > 0));

    }
  }
}



void SecLocalModel::post_secret_mem_constraints(void) {
  // Memory operations that are secret should be preceeded by a random
  for (std::pair<const vector<operation>, const vector<operation>> tp : input -> mempairs) {
    for (const operation o1: tp.first) {
      BoolVarArgs b1;
      BoolVarArgs b2;
      int op_size = O().size();
      int mem1 = input -> instructions[o1][input -> instructions[o1].size() - 1];
      BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == mem1) : var(i(o1) == i(o1));

      if (instr(o1) < op_size && instr(o1) >= 0) {
	for (const operation o2: tp.second) {
	  if (instr(o2) < op_size && instr(o2) >= 0) {
	    int mem2 = input -> instructions[o2][input -> instructions[o2].size() - 1];
	    BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == mem2) : var(i(o2) == i(o2));
	    b1 << var (a(o2) && if2 && msubseq(o1,o2));
	    b2 << var (a(o2) && if2 && msubseq(o2,o1));
	  }
	}
	if (b1.size() > 0)
	  constraint((a(o1) && if1) >> (sum(b1) > 0));
	if (b2.size() > 0)
	  constraint((a(o1) && if1) >> (sum(b2) > 0));
      }
    }
  }
}


void SecLocalModel::post_random_mem_constraints(void) {
  // These pairs should not be in the same register or should not be consequent
  for (std::pair<const operation, const operation> op : input -> memmempairs) {
    operation o1 = op.first;
    operation o2 = op.second;
    int op_size = O().size();
    if (instr(o1) < op_size && instr(o2) < op_size &&
	instr(o1) >= 0 && instr(o2) >= 0) {
      int mem1 = input -> instructions[o1][input -> instructions[o1].size() - 1];
      BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == mem1) : var(i(o1) == i(o1));
      int mem2 = input -> instructions[o2][input -> instructions[o2].size() - 1];
      BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == mem2) : var(i(o2) == i(o2));
      constraint((a(o1) && if1 && a(o2) && if2) >>
		 (!msubseq(o1,o2) && !msubseq(o2,o1)));
    }
  }
}


void SecLocalModel::post_implied_constraints(void) {
  BoolVarArgs ts;
  IntVarArgs rs;
  IntVarArgs lss;
  // int size = T().size();
  // bool arr[size];
  
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    operand p1 = input -> definer[t1];
    operand p2 = input -> definer[t2];
    vector<operand> us1 = input -> users[t1];
    vector<operand> us2 = input -> users[t2];
    operation o1 = input -> oper[p1];
    operation o2 = input -> oper[p2];
    block b1 = input -> oblock[o1];
    block b2 = input -> oblock[o2];

    // For each of the temps in the pair check if they uses of the other temp
    // happen to be in the same operation.
    // That is the case when using an accumulator.
    if (b1 == b) {
      for (operand pi: us2) {
	operation oi = input -> oper[pi];
	if (o1 == oi) {
	  constraint( a(o1) == 0 || (ry(p1) != ry(pi)));
	}
      }
    }
    if (b2 == b) {
      for (operand pi: us1) {
	operation oi = input -> oper[pi];
	if (o2 == oi) {
	  constraint( a(o2) == 0 || (ry(p2) != ry(pi)));
	}
      }
    }
  }

  // If two operands are preassigned but are in randpairs group
  // then there should be another operand that is assigned to
  // the same register
  int nregs = input -> atoms[0].size();	// safe over-estimation
  vector<temporary> spo[nregs];
  for (vector<int> pa : input -> preassign) {
    operand p = pa[0];
    register_atom r = pa[1];
    for (temporary t: input -> temps[p])
      spo[r].push_back(t);

  }

  int copyrelinv[input->ope[b].size()];
  int copyrelgroup = 0;
  for (vector<operand> ps : input->copyrel) {
    if (input->pb[ps[0]] != b) {
      copyrelgroup++;
      continue;
    }
    for (operand p: ps) {
      copyrelinv[opr(p)] = copyrelgroup;
    }
    copyrelgroup++;
  }


  for (register_atom ind = 0; ind < nregs; ind++) {
    vector<int>sp = spo[ind];
    if (sp.size() > 1) {
        for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
	  temporary t1 = tp.first;
	  temporary t2 = tp.second;
	  operand p1 = input -> definer[t1];
	  operand p2 = input -> definer[t2];

	  if ((input->pb[p1] != b) || (input->pb[p2] != b))
	    continue;
	  bool f1 = false, f2 = false;
	  for (temporary spi: sp) {
	    if (spi == t1) f1 = true;
	    if (spi == t2) f2 = true;
	  }
	  if (f1 && f2) {
	    // todo
	    constraint( subseq(t1,t2) == 0 && subseq(t2,t1) == 0);
	    constraint( v_tat[temp(t1)] != t2 && v_tbt[temp(t1)] != t2 &&
	    		v_tat[temp(t2)] != t1 && v_tbt[temp(t2)] != t1); 

	    // None of the copyrels is subseq
	    int g1 = copyrelinv[opr(p1)];
	    int g2 = copyrelinv[opr(p2)];
	    for (operand pi: input->copyrel[g1]) {
	      for (temporary t: input->temps[pi]) {
		if (t>=0) 
		  constraint(subseq(t,t2) == 0 && subseq(t2,t) == 0);
		
	      }
	    }
	    for (operand pi: input->copyrel[g2]) {
	      for (temporary t: input->temps[pi])
		if (t>=0)
		  constraint( subseq(t,t1) == 0 && subseq(t1,t) == 0);
	    }

	    // exists ti not in randpairs such that r(ti) = r
	  }
	    
	}
    }
  }
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    operand p1 = input -> definer[t1];
    operand p2 = input -> definer[t2];
    vector<operand> us1 = input -> users[t1];
    vector<operand> us2 = input -> users[t2];
    operation o1 = input -> oper[p1];
    operation o2 = input -> oper[p2];
    block b1 = input -> oblock[o1];
    block b2 = input -> oblock[o2];

    // For each of the temps in the pair check if they uses of the other temp
    // happen to be in the same operation.
    // That is the case when using an accumulator.
    if (b1 == b) {
      for (operand pi: us2) {
	operation oi = input -> oper[pi];
	if (o1 == oi) {
	  constraint( a(o1) == 0 || (ry(p1) != ry(pi)));
	}
      }
    }
    if (b2 == b) {
      for (operand pi: us1) {
	operation oi = input -> oper[pi];
	if (o2 == oi) {
	  constraint( a(o2) == 0 || (ry(p2) != ry(pi)));
	}
      }
    }
  }

  // for (temporary t1 : T())
  //   for (temporary t2 : T()) {
  //     constraint( (ls(t1) == 0) >> (subseq(t2,t1) == 0));
  //     // constraint( subseq(t2,t1) >> !subseq(t1,t2));
  //   }

  for (temporary t1 : T()) {
    for (temporary t2 : T()) {
      if (t1 == t2) continue;
      constraint( (v_tat[temp(t1)] == t2) >> (v_tbt[temp(t2)] == t1));
      constraint( (v_tat[temp(t1)] == t2) << (v_tbt[temp(t2)] == t1));
    }
  }
}



// void SecLocalModel::post_implied_subseq(void) {
//   for (temporary t1 : T()) { 
//     for (temporary t2 : T()) {
//       if (t1!=t2) {
// 	constraint((l(t1) && l(t2) && le(t1) == ls(t2)) >> subseq(t1,t2));
//       }
//     }
//   }

// }


void SecLocalModel::post_security_constraints(void) {
  // post_implied_subseq();
  if (options -> enable_power_constraints()) {
    if (!options-> disable_sec_regreg_constraints()) {
      post_random_register_constraints();
      post_implied_constraints();
      post_tt_constraints();
    }
    if (!options-> disable_sec_secret_constraints())
      post_secret_register_constraints();
    if (!options-> disable_sec_mem_constraints())
      post_secret_mem_constraints();
    if (!options-> disable_sec_memmem_constraints())
      post_random_mem_constraints();
  }
  // ct constraints are only global
    
}



void SecLocalModel::post_tt_constraints(void) {
  // if (input -> B.size() == 1) {
    int temp_size = input->T.size();
    for (temporary t1: T()) {
      IntVarArgs ta1s;
      IntVarArgs tb1s;
      for (temporary t2: T()) {
	BoolVar ifb  = subseq(t1,t2);
	IntVar thenb = var( t2 );
	IntVar elseb = var( -1 ); 
	IntVar res = IntVar(*this, -1, temp_size);
	ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
	ta1s <<  res;

	BoolVar ifb2  = subseq(t2,t1);
	IntVar thenb2 = var( t2 );
	IntVar elseb2 = var( -1 ); 
	IntVar res2 = IntVar(*this, -1, temp_size);
	ite(*this, ifb2,  thenb2, elseb2, res2, IPL_DOM);
	tb1s <<  res2;;
      }
      if (!v_tat[temp(t1)].assigned())
	max(*this, ta1s, v_tat[temp(t1)]);
      if (!v_tbt[temp(t1)].assigned())
	max(*this, tb1s, v_tbt[temp(t1)]);
    }
  // }
}
