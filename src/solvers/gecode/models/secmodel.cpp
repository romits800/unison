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


#include "secmodel.hpp"


SecModel::SecModel(Parameters * p_input, ModelOptions * p_options,
                   IntPropLevel p_ipl) :
  GlobalModel(p_input, p_options, p_ipl)
{

  // Implementation 2
  int temp_size = T().size();
  int op_size = O().size();
  int maxval = sum_of(input->maxc);
  int block_number = input -> B.size();

  // Glboal variables
  v_gb = int_var_array(block_number, 0, maxval);
  v_lgs = int_var_array(temp_size, 0, maxval);
  v_lge = int_var_array(temp_size, 0, maxval);
  post_global_cycle_offset();
  init_tts();
  
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
  
  v_lk = int_var_array(temp_size, -1, maxval);
  v_tbt = int_var_array(temp_size, -1, temp_size);
  v_tat = int_var_array(temp_size, -1, temp_size);
  v_ok = int_var_array(op_size, -1, maxval);
  //}
  // v_lk, v_ok uninitialized
  post_r2_constraints();
  post_m2_constraints();
  // Implementation 1
  
  int reg_size = input->HR.size();
  v_rtle = int_var_array(reg_size * temp_size, -1, maxval);
  v_rtlemap = int_var_array(reg_size * temp_size, -1, maxval);


  v_opcy = int_var_array(op_size, -1, maxval);
  v_opcymap = int_var_array(op_size, -1, maxval);

  post_r1_constraints();
  post_m1_constraints();
  
  post_security_constraints();

  post_connecting_constraints();
}


SecModel::SecModel(SecModel& cg) :
  GlobalModel(cg),
  memops(cg.memops),
  tbts(cg.tbts),
  tats(cg.tats)
{
  v_gb.update(*this, cg.v_gb);
  v_lgs.update(*this, cg.v_lgs);
  v_lge.update(*this, cg.v_lge);

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

SecModel* SecModel::copy(void) {
  return new SecModel(*this);
}

void SecModel::init_tts(void) {
  int size = T().size();
  bool arra[size];
  for (int i = 0; i< size; i++)
    arra[i] = false;
  bool arrb[size];
  for (int i = 0; i< size; i++)
    arrb[i] = false;

  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    operand p1 = input -> definer[t1];
    operand p2 = input -> definer[t2];
    operation o1 = input -> oper[p1];
    operation o2 = input -> oper[p2];
    block b1 = input -> oblock[o1];
    block b2 = input -> oblock[o2];
    if (b1 < b2) {
      if (!arra[t1]) {
	tats.push_back(t1);
	arra[t1] = true;
      }
      if (!arra[t2]) {
	tbts.push_back(t2);
	arra[t2] = true;
      }
    }
    else if (b1 > b2) {
      if (!arrb[t1]) {
	tbts.push_back(t1);
	arrb[t1] = true;
      }
      if (!arrb[t2]) {
	tats.push_back(t2);
	arrb[t2] = true;
      }
    }
  }
}


void SecModel::post_global_cycle_offset(void) {
  // VarInt offset;
  for (block b: input->B) {
    if (b == 0)
      constraint(gb(b) == 0);
    else
      constraint(gb(b) == gb(b-1) + c(input->out[b-1]));
  }

  for (temporary t: T()) {
    constraint(lge(t) == le(t) + gb(bot(t)));
    constraint(lgs(t) == ls(t) + gb(bot(t)));
  }
}


void SecModel::post_branchers(void) {
  // std::cout << "SecModel post branchers" << std::endl;
  GlobalModel::post_branchers();
  
  // if (!options-> disable_sec_regreg_constraints()) {
  //   IntVarArgs vtbts, vtats;
  //   for (t: tbts) vtbts << v_tbt[t];
  //   for (t: tats) vtats << v_tat[t];
  //   // Rnd r;
  //   // r.seed(42);
  //   if (vtats.size() > 0) {
  //     std::cout << "TATS: " << tats.size() << std::endl;
  //     std::cout << "VTATS: " << vtats << std::endl;
  //     branch(*this, vtats, INT_VAR_NONE(), INT_VAL(select_value_tat),
  //     	     NULL, NULL);
  //   }

  //   if (vtbts.size() > 0) {
  //     std::cout << "TBTS: " << tbts.size() << std::endl;
  //     std::cout << "VTBTS: " << vtbts << std::endl;
      
  //     branch(*this, vtbts, INT_VAR_NONE(), INT_VAL(select_value_tbt),
  //     	     NULL, NULL);
  //   }
  // }
}


BoolVar SecModel::subseq1(temporary t1, temporary t2) {
  int temp_size = T().size();
  BoolVarArgs b;
  for (register_atom ra: input -> HR) { // Hardware registers
    b << var( (v_rtle[(temp_size*ra) + t1] != -1)
	      && (v_rtlemap[(temp_size*ra) + t1] + 1 == v_rtlemap[(temp_size*ra) + t2]));

  }
  return var( sum(b) > 0 );
}


BoolVar SecModel::msubseq1(operation o1, operation o2) {
  assert( contains(memops, o1) && contains(memops, o2));
  return var ( (v_opcy[o1] != -1) && (v_opcymap[o1] + 1 == v_opcymap[o2]) );
}




BoolVar SecModel::subseq2(temporary t1, temporary t2) {
  return var (l(t1) && l(t2) && (r(t1) == r(t2)) && (v_lk[t2] == lge(t1)));
}


BoolVar SecModel::msubseq2(operation o1, operation o2) {

  return var (a(o1) && a(o2) && (v_ok[o2] == c(o1)));
}


BoolVar SecModel::msubseq(operation o1, operation o2) {
  if (options-> sec_implementation() == SEC_R1_M1 ||
      options-> sec_implementation() == SEC_R2_M1) 
    return msubseq1(o1, o2);
  else
    return msubseq2(o1, o2);
}


BoolVar SecModel::subseq(temporary t1, temporary t2) {
  if (options-> sec_implementation() == SEC_R1_M1 ||
      options-> sec_implementation() == SEC_R1_M2) 
    return subseq1(t1, t2);
  else
    return subseq2(t1, t2);

}

void SecModel::post_m1_constraints(void) {
  int maxval = sum_of(input->maxc);
  int op_size = O().size();
  if (options -> sec_implementation() == SEC_R1_M1 ||
      options -> sec_implementation() == SEC_R2_M1)
    {
      // IntVarArgs lts;
      IntVarArray sorted_lts = int_var_array(op_size, -1, maxval);
      // IntVarArray os_map = int_var_array(op_size, -1, maxval);
      for (operation o: memops) { // Hardware registers
	// a bit hacky - the last instruction is the memory instruction
	int mem = input -> instructions[o][input -> instructions[o].size() - 1];
	BoolVar if1 = (input -> type[o] == COPY) ? var(i(o) == mem) : var(i(o) == i(o));
	BoolVar ifb  = var((a(o) == 1) && if1); 
	IntVar thenb = var(c(o));
	IntVar elseb = var(-1); 
	ite(*this, ifb,  thenb, elseb, v_opcy[o], IPL_BND);
      }
      sorted(*this, v_opcy, sorted_lts, v_opcymap);
    }
}

 
void SecModel::post_r1_constraints(void) {
  int maxval = sum_of(input->maxc);
  int temp_size = T().size();
  if (options -> sec_implementation() == SEC_R1_M1 ||
      options -> sec_implementation() == SEC_R1_M2)
    {
      for (register_atom ra: input -> HR) { // Hardware registers
	IntVarArgs lts;
	IntVarArray sorted_lts = int_var_array(temp_size, -1, maxval);
	IntVarArray rs_map = int_var_array(temp_size, -1, maxval);
	for (temporary t: input -> T) {
	  BoolVar ifb  = var(l(t) && (r(t) == ra)); 
	  IntVar thenb = var( lgs(t) );
	  IntVar elseb = var( -1 );
	  IntVar res = IntVar(*this, -1, maxval);
	  ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
	  constraint(v_rtle[ra*temp_size + t] == res);
	  constraint(v_rtlemap[ra*temp_size + t] == rs_map[t]);
	  lts <<  res;
	}
	sorted(*this, lts, sorted_lts, rs_map);      
      }
    }
}

void SecModel::post_r2_constraints(void) {

  int maxval = sum_of(input->maxc);
  if (options -> sec_implementation() == SEC_R2_M2 ||
      options -> sec_implementation() == SEC_R2_M1)
    {
      for (temporary t1 : input -> T) { 
	IntVarArgs lts;
 	for (temporary t2 : input -> T) {
      	  if (t1 != t2) {
      	    BoolVar ifb  = var(l(t1) && l(t2) && (r(t1) == r(t2)) &&
      			       (lge(t2) <= lgs(t1)));
      	    IntVar thenb = var( lge(t2) );
      	    IntVar elseb = var( -1); 
      	    IntVar res = IntVar(*this, -1, maxval);
      	    ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
      	    lts << res;
      	  }
	}
	if (lts.size() >= 0)
	  max(*this, lts, v_lk[t1], IPL_DOM);
      }
    }
}

void SecModel::post_m2_constraints(void) {
  int maxval = sum_of(input->maxc);
  if (options -> sec_implementation() == SEC_R2_M2 ||
      options -> sec_implementation() == SEC_R1_M2)
    {
      for (operation o1 : memops) { 
	IntVarArgs lts;
 	for (operation o2 : memops) {
      	  if (o1 != o2) {
	    // a bit hacky - the last instruction is the memory instruction
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
	if (lts.size() >= 0)
	  max(*this, lts, v_ok[o1]);
      }
    }
}




void SecModel::post_random_register_constraints(void) {
  // These pairs should not be in the same register or should not be consequent
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    constraint((l(t1) && l(t2) && (r(t1) == r(t2))) >>
	       (!subseq(t1,t2) && !subseq(t2,t1)));
  }
}



void SecModel::post_secret_register_constraints(void) {
  // Temporaries that are secret should be preceeded by a random
  for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
    BoolVarArgs b;
    BoolVarArgs b1;
    BoolVarArgs b2;
    temporary tsec = tp.first;
    // std::cout << tsec << std::endl;
    for (const temporary trand: tp.second) {
      b << var (  (l(trand) && subseq(trand,tsec)));
      // b1 << var(l(trand) ==1);
      // b2 << var(subseq(trand,tsec) ==1);
      // std::cout << trand << ", ";
    }
    // std::cout << std::endl << b << std::endl;
    // std::cout << b1 << std::endl;
    // std::cout << b2 << std::endl;
    // std::cout << v_lk << std::endl;
    if (b.size() > 0)
      constraint(l(tsec)  >> (sum(b) > 0));
  }
}



void SecModel::post_secret_mem_constraints(void) {
  // Memory operations that are secret should be preceeded by a random
  for (std::pair<const vector<operation>, const vector<operation>> tp : input -> mempairs) {
    for (const operation o1: tp.first) {
      BoolVarArgs b;
      for (const operation o2: tp.second) {
	b << var (a(o1) >> (a(o2) && msubseq(o1,o2)));
      }
      if (b.size() > 0)
	constraint(sum(b) >0);
    }
  }
}



void SecModel::post_random_mem_constraints(void) {
  // Memory operations that are random or public and should not follow each other 
  for (std::pair<const operation, const operation> op : input -> memmempairs) {
    operation o1 = op.first;
    operation o2 = op.second;
    constraint((a(o1) && a(o2)) >>
	       (!msubseq(o1,o2) && !msubseq(o2,o1)));

  }
}



void SecModel::post_different_solution(SecModel * g1, bool unsat) {
  std::cout << "posting different solution" << std:: endl;
  BoolVarArgs lits;
  for (temporary t : tbts)
    if (g1->v_tbt[t].assigned())
      lits << var(v_tbt[t] == g1->v_tbt[t].val());

  for (temporary t : tats)
    if (g1->v_tat[t].assigned())
      lits << var(v_tat[t] == g1->v_tat[t].val());
  
  if (lits.size() > 0)
    rel(*this, BOT_AND, lits, 0);

  GlobalModel::post_different_solution(g1, unsat);

}



void SecModel::post_implied_constraints(void) {
  BoolVarArgs ts;
  IntVarArgs rs;
  IntVarArgs lss;
  int size = T().size();
  bool arr[size];
  
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    operand p1 = input -> definer[t1];
    operand p2 = input -> definer[t2];
    vector<operand> us1 = input -> users[t1];
    vector<operand> us2 = input -> users[t2];
    operation o1 = input -> oper[p1];
    operation o2 = input -> oper[p2];
    for (operand pi: us2) {
      operation oi = input -> oper[pi];
      if (o1 == oi) {
  	// std::cout << o1 << ": " << p1 << ", " << pi << std::endl;
  	constraint( a(o1) == 0 || (ry(p1) != ry(pi)));
      }
    }
    for (operand pi: us1) {
      operation oi = input -> oper[pi];
      if (o2 == oi) {
  	// std::cout << o1 << ": " << p2 << ", " << pi << std::endl;
  	constraint( a(o2) == 0 || (ry(p2) != ry(pi)));
      }
    }
  }

  int nregs = input -> atoms[0].size();	// safe over-estimation
  vector<temporary> spo[nregs];
  for (vector<int> pa : input -> preassign) {
    operand p = pa[0];
    register_atom r = pa[1];
    for (temporary t: input -> temps[p])
      spo[r].push_back(t);

  }
  for (register_atom ind = 0; ind < nregs; ind++) {
    vector<int>sp = spo[ind];
    if (sp.size() > 1) {
      for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
	temporary t1 = tp.first;
	temporary t2 = tp.second;
	bool f1 = false, f2 = false;
	for (temporary spi: sp) {
	  if (spi == t1) f1 = true;
	  if (spi == t2) f2 = true;
	}
	if (f1 && f2) {
	  //std::cout << "temps: " << t1 << " " << t2 << std::endl;
	  // todo
	  constraint( subseq(t1,t2) == 0 && subseq(t2,t1) == 0);
	  constraint( v_tat[temp(t1)] != t2 && v_tbt[temp(t1)] != t2 &&
	  		v_tat[temp(t2)] != t1 && v_tbt[temp(t2)] != t1); 
	  // exists ti not in randpairs such that r(ti) = r
	}
	    
      }
    }
  }

  
  for (temporary t1 : T())
    for (temporary t2 : T()) {
      constraint( (lgs(t1) == 0) >> (subseq(t2,t1) == 0));
      // constraint( subseq(t2,t1) >> !subseq(t2,t1));
    }

  // If tbt[t1] = t2 then tat[t2] == t2
  for (temporary t1 : T()) {
    for (temporary t2 : T()) {
      if (t1 == t2) continue;
      constraint( (v_tat[t1] == t2) >> (v_tbt[t2] == t1));
      constraint( (v_tat[t1] == t2) << (v_tbt[t2] == t1));
    }
  }
  
  // If two operands are assigned to the same register then
  // at least one pair of their temps will have a "subseq".
  // constraint( (ry(0) == ry(143) && x(0) && x(143)) >> (v_tat[0] != -1 && (v_tbt[64] != -1 || v_tbt[66]
  // != -1 || v_tbt[67]!= -1)));
  // for (operand p1 : P()) {
  //   for (operand p2 : P()) {
  //     if (p1 == p2) continue;
  //     operation o1 = input -> oper[p1];
  //     operation o2 = input -> oper[p2];
  //     block b1 = input -> oblock[o1];
  //     block b2 = input -> oblock[o2];
  //     BoolVarArgs tps;
  //     bool samets = false;
  //     for (temporary t1 : input -> temps[p1]) {
  // 	if (t1 == -1) continue;
  // 	for (temporary t2 : input -> temps[p2]) {
  // 	  if (t1 == t2) {
  // 	    samets = true;
  // 	    break;
  // 	  }
  // 	  if (t2 == -1) continue;
  // 	  if (b1 < b2) {
  //     	    tps << var((v_tat[t1] != -1) && (v_tbt[t2] != -1));
  // 	  }
  // 	  else if (b1 > b2) {
  // 	    tps << var((v_tbt[t1] != -1) && (v_tat[t2] != -1));
  // 	  }
  // 	  else {
  // 	    tps << var(((v_tbt[t1] != -1) && (v_tat[t2] != -1)) || ((v_tat[t1] != -1) && (v_tbt[t2] != -1)));
  // 	  }
  // 	}
  //     }
  //     if (tps.size() != 0 && !samets) {
  // 	constraint( (ry(p1) == ry(p2) && x(p1) && x(p2)) >> (sum(tps) > 0));
  //     }
  //   }      
  // } // end outer for loop

}

// Strict constraints for decomposition
void SecModel::post_strict_constraints(void) {
  // std::cout << "SecModel post branchers" << std::endl;
  if (!options-> disable_sec_regreg_constraints()) {

    for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
      temporary t1 = tp.first;
      temporary t2 = tp.second;
      operand p1 = input -> definer[t1];
      operand p2 = input -> definer[t2];
      operation o1 = input -> oper[p1];
      operation o2 = input -> oper[p2];
      block b1 = input -> oblock[o1];
      block b2 = input -> oblock[o2];
      if (b1 != b2) {
	constraint((l(t1) && l(t2) && r(t1) != r(t2)) >> ((lgs(t1) > lge(t2) + 2) || (lgs(t2) > lge(t1) + 2 )));
      }
    }
  }
}


void SecModel::post_connecting_constraints(void) {

  for (vector<operand> adj : input->adjacent) {
    operand p = adj[0], q = adj[1];
    BoolVarArgs tps;
    for (temporary t1 : input -> temps[p]) {
      if (t1 == -1) continue; // should actually not happen
      for (temporary t2 : input -> temps[q]) {
	if (t2 == -1) continue; // should actually not happen
	tps << var (v_tat[t1] == t2 && v_tbt[t2] == t1);
	// constraint(v_tbt[q] == p);
      }
    }
    constraint( (x(p) && x(q)) >> (sum(tps) > 0));
  }
}


void SecModel::post_security_constraints(void) {
  if (!options-> disable_sec_regreg_constraints()) {
    post_random_register_constraints();
    post_tt_constraints();
    post_implied_constraints();
    post_connecting_constraints();
    // // post_strict_constraints();
  }
  if (!options-> disable_sec_secret_constraints())
    post_secret_register_constraints();
  if (!options-> disable_sec_mem_constraints())
    post_secret_mem_constraints();
  if (!options-> disable_sec_memmem_constraints())
    post_random_mem_constraints();

}

void SecModel::post_tt_constraints(void) {
  int temp_size = T().size();
  for (temporary t1: T()) {
      // operand p1 = input -> definer[t1];
      // // operand p2 = input -> definer[t2];
      // operation o1 = input -> oper[p1];
      // // operation o2 = input -> oper[p2];
      // block b1 = input -> oblock[o1];
      // // block b2 = input -> oblock[o2];

      // if (t1 > 60 && t1 < 60) continue;
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
    max(*this, ta1s, v_tat[t1]);
    max(*this, tb1s, v_tbt[t1]);
    constraint( v_tat[t1] != t1);
    constraint( v_tbt[t1] != t1);
  }
}


block SecModel::bot (temporary t) {
  operand p = input -> definer[t];
  operation o = input -> oper[p];
  return (input -> oblock[o]);
}
