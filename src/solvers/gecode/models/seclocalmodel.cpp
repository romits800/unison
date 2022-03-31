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

  int temp_size = T().size(); // These come from LocalModel
  int op_size = O().size();
  int maxval = sum_of(input->maxc);
  int reg_size = input->HR.size();

  // Find mem operations
  
  vector<string> memstrings = {"tSTRspi_fi", "tLDRspi_fi"}; 
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
  
  post_r2_constraints();
  post_m2_constraints();

  // Implementation 1
  v_rtle = int_var_array(reg_size * temp_size, -1, maxval);
  v_rtlemap = int_var_array(reg_size * temp_size, -1, maxval);

  v_opcy = int_var_array(op_size, -1, maxval);
  v_opcymap = int_var_array(op_size, -1, maxval);
  
  post_r1_constraints();
  post_m1_constraints();

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

    // BoolVarArgs as;
    // int size = O().size();
    // bool arr[size];
    // for (int i = 0; i< size; i++)
    //   arr[i] = false;

    // for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    //   temporary t1 = tp.first;
    //   temporary t2 = tp.second;
    //   operand p1 = input -> definer[t1];
    //   operand p2 = input -> definer[t2];
    //   vector<operand> us1 = input -> users[t1];
    //   vector<operand> us2 = input -> users[t2];
    //   operation o1 = input -> oper[p1];
    //   operation o2 = input -> oper[p2];
    //   block b1 = input -> oblock[o1];
    //   block b2 = input -> oblock[o2];

    //   if (b1 == b) {
    // 	for (operand pi: us2) {
    // 	  operation oi = input -> oper[pi];
    // 	  if ((o1 == oi) && !arr[oi]) {
    // 	    std::cout << oi << " " << arr[oi] << ",";
    // 	    as << a(oi);
    // 	    arr[oi] = true;
    // 	  }
    // 	}
    //   }
    //   if (b2 == b) {
    // 	for (operand pi: us1) {
    // 	  operation oi = input -> oper[pi];
    // 	  if ((o2 == oi) && !arr[oi]) {
    // 	    std::cout << oi << " " << arr[oi] << ",";
    // 	    as << a(oi);
    // 	    arr[oi] = true;
    // 	  }
    // 	}
    //   }
    // }
    // std::cout << std::endl << as << std::endl;
    // if (as.size() > 0)
    //   branch(*this, as, BOOL_VAR_NONE(), BOOL_VAL_MIN(),
    // 	     NULL, NULL);

  //   {
  //     BoolVarArgs ts;
  //     int size = T().size();
  //     bool arr[size];
  //     for (int i = 0; i< size; i++)
  // 	arr[i] = false;

  //     for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
  // 	temporary t1 = tp.first;
  // 	temporary t2 = tp.second;
  // 	if (temp(t1) < size && temp(t1) >= 0 && temp(t2) < size && temp(t2) >= 0) {
  // 	  if (!arr[temp(t1)]) {
  // 	    ts << l(t1);
  // 	    arr[temp(t1)] = true;
  // 	  }
  // 	  if (!arr[temp(t2)]) {
  // 	    ts << l(t2);
  // 	    arr[temp(t2)] = true;
  // 	  }
  // 	}
  //     }
  //     if (ts.size() > 0) 
  // 	branch(*this, ts, BOOL_VAR_NONE(), BOOL_VAL_MIN(),
  // 	       NULL, NULL);
  //   }
  }
  if (!options-> disable_sec_mem_constraints()) {
  }
}


void SecLocalModel::post_branchers(char search) {
  post_sec_brancher();
  LocalModel::post_branchers(search);
}


SecLocalModel* SecLocalModel::copy(void) {
  return new SecLocalModel(*this);
}



BoolVar SecLocalModel::subseq1(temporary t1, temporary t2) {
  int temp_size = T().size();
  BoolVarArgs b;
  for (register_atom ra: input -> HR) { // Hardware registers
    b << var( (v_rtle[(temp_size*ra) + temp(t1)] != -1)
	      && (v_rtlemap[(temp_size*ra) + temp(t1)] + 1 == v_rtlemap[(temp_size*ra) + temp(t2)]));

  }
  return var( sum(b) > 0 );
}


BoolVar SecLocalModel::msubseq1(operation o1, operation o2) {
  return var ((v_opcy[instr(o1)] != -1) &&
	      (v_opcymap[instr(o1)] + 1 == v_opcymap[instr(o2)]));
}


BoolVar SecLocalModel::subseq2(temporary t1, temporary t2) {
  return var (l(t1) && l(t2) && v_lk[temp(t2)] == le(t1));
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
	ite(*this, ifb,  thenb, elseb, v_opcy[instr(o)], IPL_BND);
      }
      sorted(*this, v_opcy, sorted_lts, v_opcymap);
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
	  ite(*this, ifb,  thenb, elseb, res, IPL_BND);
	  constraint(v_rtle[ra*temp_size + temp(t)] == res);
	  constraint(v_rtlemap[ra*temp_size + temp(t)] == rs_map[temp(t)]);
	  lts <<  res;
	}
	sorted(*this, lts, sorted_lts, rs_map);      
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
      	    ite(*this, ifb,  thenb, elseb, res, IPL_BND);
      	    lts << res;
      	  }
	}
	max(*this, lts, v_lk[temp(t1)]);
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
	    ite(*this, ifb,  thenb, elseb, res, IPL_BND);
	    lts <<  res;
	  }
	}
	max(*this, lts, v_ok[instr(o1)]);
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



void SecLocalModel::post_secret_register_constraints(void) {
  // Temporaries that are secret should be preceeded by a random
  for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
    BoolVarArgs b;
    // BoolVarArgs b1;
    temporary tsec = tp.first;
    int temp_size = T().size();
    // std::cout << tsec << "| ";
    if (temp(tsec) < temp_size && temp(tsec) >= 0) {
      for (const temporary trand: tp.second) {
	if (temp(trand) < temp_size && temp(trand) >= 0) {
	  b << var (l(trand) && subseq(trand,tsec));
	  //b1 << var(l(tsec) ==1);
	  // std::cout << trand << ", ";
	}
      }
      // std::cout << std::endl << b << std::endl;
      // std::cout << b1 << std::endl;
      if (b.size() > 0)
	constraint( l(tsec) >> (sum(b) > 0));
    }
  }
}



void SecLocalModel::post_secret_mem_constraints(void) {
  // Memory operations that are secret should be preceeded by a random
  for (std::pair<const vector<operation>, const vector<operation>> tp : input -> mempairs) {
    for (const operation o1: tp.first) {
      BoolVarArgs b;
      int op_size = O().size();
      if (instr(o1) < op_size && instr(o1) >= 0) {
	for (const operation o2: tp.second) {
	  if (instr(o2) < op_size && instr(o2) >= 0) {
	    b << var (a(o2) && msubseq(o1,o2));
	  }
	}
	if (b.size() > 0)
	  constraint(a(o1) >> (sum(b) >0));
      }
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

  if (!options-> disable_sec_regreg_constraints()) {
    post_random_register_constraints();
    post_implied_constraints();
  }
  if (!options-> disable_sec_secret_constraints())
    post_secret_register_constraints();
  if (!options-> disable_sec_mem_constraints())
    post_secret_mem_constraints();
}
