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

  if (options -> enable_power_constraints()) {
    // Implementation 2
    int temp_size = T().size();
    int op_size = O().size();
    int maxval = sum_of(input->maxc);
    int block_number = input -> B.size();

    for (register_atom ra: input -> HR) { // Hardware registers
        hardware_regs.insert(ra);
    }


    // Glboal variables
    v_gb = int_var_array(block_number, 0, maxval);
    v_lgs = int_var_array(temp_size, 0, maxval);
    v_lge = int_var_array(temp_size, 0, maxval);
    v_gc = int_var_array (op_size, 0, maxval);
    post_global_cycle_offset();

    if (!options-> disable_sec_tts())
        init_tts();
  
    vector<string> memstrings = {"tSTRBi", "tLDRBi", "tSTRspi_fi", 
                                    "tLDRspi_fi", "SW_fi", "LW_fi", 
                                    "SB_fi", "LB_fi"}; 


    for (operation o : O()) { 
       for(instruction i : input-> instructions[o]) {
         if (contains(memstrings, input -> insname[i]) || 
             contains(memcopies, input -> insname[i]) ) {
           memops.push_back(o);
           break;
         }
       }
    }
  
    v_lk = int_var_array(temp_size, -1, maxval);

    if (!options-> disable_sec_tts()) {
        v_tbt = int_var_array(temp_size, -1, temp_size);
        v_tat = int_var_array(temp_size, -1, temp_size);
    }
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
    
    // check this otu
    //post_security_constraints(); //???
    //post_connecting_constraints();

    for (block b: input -> B) {
        set <temporary> tmp;
        set <operation> ops;
        set <operand> opas;
        extmps[b] = tmp;
        exops[b] = ops;
        exopas[b] = opas;
    }

    monolithic = false;

  }
  sec_r.seed(p_options->seed());
  sec_p = p_options->relax();
  
  rng.seed(p_options->seed());
  unif = new std::uniform_real_distribution<double>(0,1);

  post_security_constraints();
}


SecModel::SecModel(SecModel& cg) :
  GlobalModel(cg),
  memops(cg.memops),
  tats(cg.tats),
  tbts(cg.tbts),
  hardware_regs(cg.hardware_regs),
  extmps(cg.extmps),
  exops(cg.exops),
  exopas(cg.exopas),
  rsol(cg.rsol),
  csol(cg.csol),
  isol(cg.isol),
  sec_r(cg.sec_r),
  sec_p(cg.sec_p),
  rng(cg.rng),
  unif(cg.unif),
  monolithic(cg.monolithic),
  memcopies(cg.memcopies)
{
  v_gb.update(*this, cg.v_gb);
  v_lgs.update(*this, cg.v_lgs);
  v_lge.update(*this, cg.v_lge);
  v_gc.update(*this, cg.v_gc);

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

  for (operation o: O()) {
    block b = input -> oblock[o];
    constraint(gc(o) == c(o) + gb(b));
  }
}

void SecModel::set_monolithic(bool val) {
    monolithic = val;
}


void SecModel::post_solution_brancher(SecModel *s) {

    IntArgs sol;
    IntVarArgs vs;
    IntArgs sol2;
    BoolVarArgs vs2;
    //for (int c: solver->cycles) sol2 << ((c == -1) ? 0 : 1);
    //std::cout << "Posting solution brancher" << std::endl;
    for (BoolVar a: v_a) vs2 << a;
    for (BoolVar a: s -> v_a) sol2 << a.val();

    for (IntVar c: s->v_c) sol << (c.assigned() ? c.val() : -1);
    for (IntVar r: s->v_r) sol << (r.assigned() ? r.val() : -1);
    for (IntVar c: v_c) vs << c;
    for (IntVar r: v_r) vs << r;

    solution_branch(*this, vs2, sol2);
    solution_branch(*this, vs, sol);
}

void SecModel::post_branchers(void) {
  // std::cout << "SecModel post branchers" << std::endl;
  GlobalModel::post_branchers();
  
}

void SecModel::post_complete_branchers(unsigned int s) {

  GlobalModel::post_complete_branchers(s);

}



void SecModel::post_unassigned_branchers(unsigned int s) {
  Rnd ran;
  ran.seed(100 + s);
  // Post regular branchers
  for (block b: input -> B) {
      IntVarArgs os;
      IntVarArgs is;
      IntVarArgs rs;
//#ifdef OPERS
      IntVarArgs ys;
//#endif
      BoolVarArgs as;
      for (std::set<operation>::iterator it = exops[b].begin(); it != exops[b].end(); ++it) {
        os << c(*it);
        is << i(*it);
        as << a(*it);
      }
      for (std::set<temporary>::iterator it = extmps[b].begin(); it != extmps[b].end(); ++it) {
        rs << r(*it);
      }

//#ifdef OPERS
      for(operand p: exopas[b]) {
        ys << y(p);
      }
//#endif

      if (as.size() > 0) 
          branch(*this, as, BOOL_VAR_RND(ran), BOOL_VAL_RND(ran),
             NULL, NULL);
      if (is.size() > 0) 
          branch(*this, is, INT_VAR_RND(ran), INT_VAL_MIN(),
             NULL, NULL);

///#ifdef OPERS
      if (ys.size() > 0) 
          branch(*this, ys, INT_VAR_RND(ran), INT_VAL_MIN(),
             NULL, NULL);
//#endif
      

      if (os.size() > 0)
          branch(*this, os, INT_VAR_RND(ran), INT_VAL_MIN(),
             NULL, NULL);

      if (rs.size() > 0) 
          branch(*this, rs, INT_VAR_RND(ran), INT_VAL_RND(ran), //(ran),
             NULL, NULL);

      //std::cout << b << " " << rs << rs.size() << std::endl;
      //std::cout << b << " " << is << is.size() << std::endl;
      //std::cout << b << " " << os << os.size() << std::endl;
      //std::cout << b << " " << as << as.size() << std::endl;
      //std::cout << b << " " << ys << ys.size() << std::endl;

  }



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
  assert( contains(memops, o1) && contains(memops, o2));
  return var (a(o1) && a(o2) && (v_ok[o2] == gc(o1)));
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
	BoolVar if1 = (input -> type[o] == COPY) ? var(i(o) == get_mem_instr(o)) : var(i(o) == i(o));
	BoolVar ifb  = var((a(o) == 1) && if1); 
	IntVar thenb = var(gc(o));
	IntVar elseb = var(-1); 
	ite(*this, ifb,  thenb, elseb, v_opcy[o], IPL_DOM);
      }
      sorted(*this, v_opcy, sorted_lts, v_opcymap, IPL_DOM);
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
	  max(*this, lts, v_lk[t1]);
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
	    //int mem = input -> instructions[o2][input -> instructions[o2].size() - 1];

	    BoolVar if1 = (input -> type[o2] == COPY) ? var(i(o2) == get_mem_instr(o2)) : var(i(o2) == i(o2));
	    //BoolVar if1 = (input -> type[o2] == COPY) ? var(i(o2) == mem) : var(i(o2) == i(o2));
	    BoolVar ifb  = var(a(o2) && if1 && (gc(o2) <= gc(o1)));
      	    IntVar thenb = var( gc(o2) );
      	    IntVar elseb = var( -1 ); 
      	    IntVar res = IntVar(*this, -1, maxval);
      	    ite(*this, ifb,  thenb, elseb, res, IPL_DOM);
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
  IntArgs hrs;
  for (register_atom ra: input -> HR) { // Hardware registers
    hrs << ra;
  }
  IntSet d(hrs);
  for (std::pair<const temporary, const temporary> tp : input -> randpairs) {
    temporary t1 = tp.first;
    temporary t2 = tp.second;
    BoolVar is_hd(*this, 0, 1);
    dom(*this, r(t1), d, is_hd);
    constraint((l(t1) && l(t2) && (r(t1) == r(t2)) && is_hd) >>
    //constraint((l(t1) && l(t2) && (r(t1) == r(t2))) >>
	       (!subseq(t1,t2) && !subseq(t2,t1)));
  }
}



void SecModel::post_secret_register_constraints(void) {
  // Temporaries that are secret should be preceeded by a random
  for (std::pair<const temporary, const vector<temporary>> tp : input -> secpairs) {
    BoolVarArgs b1;
    BoolVarArgs b2;
    temporary tsec = tp.first;
    // std::cout << tsec << std::endl;
    for (const temporary trand: tp.second) {
      b1 << var (  (l(trand) && subseq(trand,tsec)));
    }
    if (b1.size() > 0)
      constraint(l(tsec)  >> (sum(b1) > 0));


    for (const temporary trand: tp.second) {
      b2 << var (  (l(trand) && subseq(tsec,trand)));
    }
    if (b2.size() > 0)
      constraint(l(tsec)  >> (sum(b2) > 0));

  }
}



void SecModel::post_secret_mem_constraints(void) {
  // Memory operations that are secret should be preceeded by a random
  for (std::pair<const vector<operation>, const vector<operation>> tp : input -> mempairs) {
    for (const operation o1: tp.first) {
      BoolVarArgs b1;
      BoolVarArgs b2;
      for (const operation o2: tp.second) {
	//int mem1 = input -> instructions[o1][input -> instructions[o1].size() - 1];
	BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == get_mem_instr(o1)) : var(i(o1) == i(o1));
	//BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == mem1) : var(i(o1) == i(o1));
	//int mem2 = input -> instructions[o2][input -> instructions[o2].size() - 1];
	BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == get_mem_instr(o2)) : var(i(o2) == i(o2));
	//BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == mem2) : var(i(o2) == i(o2));
	b1 << var ((a(o1) && if1) >> (a(o2) && if2 && msubseq(o1,o2)));
      }
      if (b1.size() > 0)
	constraint(sum(b1) >0);

      // the other direction
      for (const operation o2: tp.second) {
	/*int mem1 = input -> instructions[o1][input -> instructions[o1].size() - 1];
	BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == mem1) : var(i(o1) == i(o1));
	int mem2 = input -> instructions[o2][input -> instructions[o2].size() - 1];
	BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == mem2) : var(i(o2) == i(o2));
        */
	BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == get_mem_instr(o1)) : var(i(o1) == i(o1));
	BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == get_mem_instr(o2)) : var(i(o2) == i(o2));
	b2 << var ((a(o1) && if1) >> (a(o2) && if2 && msubseq(o2,o1)));
      }
      if (b2.size() > 0)
	constraint(sum(b2) >0);
    }
  }
}



void SecModel::post_random_mem_constraints(void) {
  // Memory operations that are random or public and should not follow each other 
  for (std::pair<const operation, const operation> op : input -> memmempairs) {
    operation o1 = op.first;
    operation o2 = op.second;

    BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == get_mem_instr(o1)) : var(i(o1) == i(o1));
    //int mem1 = input -> instructions[o1][input -> instructions[o1].size() - 1];
    //BoolVar if1 = (input -> type[o1] == COPY) ? var(i(o1) == mem1) : var(i(o1) == i(o1));
    //int mem2 = input -> instructions[o2][input -> instructions[o2].size() - 1];
   // BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == mem2) : var(i(o2) == i(o2));

    BoolVar if2 = (input -> type[o2] == COPY) ? var(i(o2) == get_mem_instr(o2)) : var(i(o2) == i(o2));
    constraint((a(o1) && if1 && a(o2) && if2) >>
	       (!msubseq(o1,o2) && !msubseq(o2,o1)));

  }
}



void SecModel::post_different_solution(SecModel * g1, bool unsat) {

  if (!options-> disable_sec_tts()) {
      BoolVarArgs lits;
      for (temporary t : tbts)
        if (g1->v_tbt[t].assigned())
          lits << var(v_tbt[t] == g1->v_tbt[t].val());

      for (temporary t : tats)
        if (g1->v_tat[t].assigned())
          lits << var(v_tat[t] == g1->v_tat[t].val());
      
      if (lits.size() > 0)
        rel(*this, BOT_AND, lits, 0);
  }
  GlobalModel::post_different_solution(g1, unsat);

}



void SecModel::post_implied_constraints(void) {
  BoolVarArgs ts;
  IntVarArgs rs;
  IntVarArgs lss;
  int size = T().size();
  
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

  if (!options-> disable_sec_tts()) {
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

  }
  
  for (temporary t1 : T())
    for (temporary t2 : T()) {
      constraint( (lgs(t1) == 0) >> (subseq(t2,t1) == 0));
      // constraint( subseq(t2,t1) >> !subseq(t2,t1));
    }

  // If tbt[t1] = t2 then tat[t2] == t2
  if (!options-> disable_sec_tts()) {
      for (temporary t1 : T()) {
        for (temporary t2 : T()) {
          if (t1 == t2) continue;
          constraint( (v_tat[t1] == t2) >> (v_tbt[t2] == t1));
          constraint( (v_tat[t1] == t2) << (v_tbt[t2] == t1));
        }
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

IntVar SecModel::branch_cost(block b, int n) {
  int fq = 1; // Don't care about the freq
  //std::cout << b << ": " << input-> freq[b] << std::endl;
  IntVar fb = var (f(b,n) - 1);
  int brcount = 0;
  // This is only for MIPS delay slots - temporary solution
  if (options -> extra_branch_cost()) {
    for (operation o: input -> ops[b]) {
      if (input -> type [o] == BRANCH || input -> type [o] == CALL)
	brcount ++;
    }
  }
  return var(fq * (fb + brcount));
}


// Constant resource constraints
void SecModel::post_ct_constraints(void) {
  // The paths that should be ct time
  // for every constraint that consists of many paths
  for (unsigned int n = 0; n < input->N; n++) {
    for (vector<vector<vector<block> > > ci: input -> bbs) {
      // for every path in a constraint
      IntVarArgs cs;
      for (vector<vector<block> > pi: ci) {
	IntVarArgs path;
	for (vector<block> bi: pi) {
	  // std::cout << "Bi" << bi;
	  path << var (branch_cost(bi[0], n) + bi[1]);
	}
	// std::cout << std::endl;
	cs << var (sum(path));
      }
      // std::cout << cs << std::endl;
      // all paths should have the same number of cycles
      rel(*this, cs, IRT_EQ);
    }
  }
}

/*void SecModel::post_security_constraints(void) {
  if (!options-> disable_sec_regreg_constraints()) {
    post_random_register_constraints();

    if (!options-> disable_sec_tts()) {
        post_tt_constraints();
        post_connecting_constraints();
    }
    post_implied_constraints();
    // // post_strict_constraints();
  }
}*/

void SecModel::post_security_constraints(void) {
  if (options -> enable_power_constraints()) {
    if (!options-> disable_sec_regreg_constraints()) {
      post_random_register_constraints();

      if (!options-> disable_sec_tts()) {
          post_tt_constraints();
          post_connecting_constraints();
      }
      post_implied_constraints();
      // // post_strict_constraints();
    }
    if (!options-> disable_sec_secret_constraints())
      post_secret_register_constraints();
    if (!options-> disable_sec_mem_constraints())
      post_secret_mem_constraints();
    if (!options-> disable_sec_memmem_constraints())
      post_random_mem_constraints();
  }
  if (options -> enable_ct_constraints()) {
    post_ct_constraints();
    // TODO
  }
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


block SecModel::bot(temporary t) {
  operand p = input -> definer[t];
  operation o = input -> oper[p];
  return (input -> oblock[o]);
}

instruction SecModel::get_mem_instr(operation o) {

  for (instruction i: input->instructions[o]) {
    if (contains(memcopies, input -> insname[i])) {
        return i;
    }
  }
  return -1;

}

void SecModel::apply_global_solution(SecModel * sm) {

  for (temporary t1 : input->T) {
    if (!sm->is_dead(t1)) {
      constraint(r(t1) == sm->r(t1));
    }
  }

   // std::cout << "St2 " << std::endl;
  for (operation o : input->O) {
    constraint(i(o) == sm->i(o));
  }
  
   // std::cout << "St3 " << std::endl;
  for (operation o : input->O)
    if (!sm->is_inactive(o)) {
      constraint(c(o) == sm->c(o));
    }

  //  std::cout << "St4 " << std::endl;
  for (operand p : input->P) {
    constraint(y(p) == sm->y(p));
  }
}

void SecModel::clear_extmps() {

  extmps.clear();

}

void SecModel::apply_solution(SecLocalModel * ls) {


  block b = ls->b;

  // Not assign border solutions
  // Map for the cycle of every register
  map <int, int> mp;
  map <int, int> mp_up;
  map <int, int> extemps;
  map <int, int> extemps_up;
  set <temporary> extmpsloc;
  set <operation> exopsloc;
  set <operand>   exopasloc;
  set <operation> exopers;
  set <operation> exopers_up;
  set <operand> exoperands;
  set <operand> exoperands_up;


  // Temporaries to exlude from applying the solution
  // init max values for temporaries
  for (temporary t1 : input->tmp[b]) {
    if (!ls->is_dead(t1)) {
      int val = ls -> r(t1).val();
      int maxval = sum_of(input->maxc);
      mp[val] = 0;
      mp_up[val] = maxval;
    }
  }

  for (temporary t1 : input->tmp[b]) {
    if (!ls->is_dead(t1)) {
      int val = ls -> r(t1).val();
      if (!hardware_regs.count(val)) {
        continue;
      }
      //input -> definer[t] the operand that defines t
      //input -> users[t] the operand that defines t
      operation o = input -> def_opr[t1];
      if (ls -> is_inactive(o)) continue;
      int cyc = ls -> c(o).val();
      if (mp[val] < cyc) {
	mp[val] = cyc;
	extemps[val] = t1;
      }
      if (mp_up[val] > cyc) {
        mp_up[val] = cyc;
        extemps_up[val] = t1;
      }
    }
  }
  // the selected ones
  for (auto it = extemps.begin(); it !=extemps.end(); ++it) {
    temporary t = it->second;
    operation o = input -> def_opr[t]; //the operation that defines t
    operand   p = input -> definer[t]; //the operand that defines t

    exopers.insert(o);
//#ifdef OPERS
    for (operand pi : input -> ope[b]) // for all operands in this block
        if (input -> copyreltop[pi] == p) {
            exoperands.insert(pi);
            exopers.insert(input->oper[pi]);
            for (operand pj : input->operands[input->oper[pi]])
                exoperands.insert(pj);
            break;
        }
//#endif
        
    /*for (operand p : input -> users[t])
        exoperands.insert(p);
    */
  }

  for (auto it = extemps_up.begin(); it !=extemps_up.end(); ++it) {
    temporary t = it->second;
    operation o = input -> def_opr[t];
    operand   p = input -> definer[t]; //the operand that defines t
    exopers_up.insert(o);
    /*for (operand p : input -> users[t])
        exoperands_up.insert(p);*/

//#ifdef OPERS
    for (operand pi : input -> ope[b]) // for all operands in this block
        if (input -> copyreltop[pi] == p) {
            exoperands_up.insert(pi);
            exopers_up.insert(input->oper[pi]);
            for (operand pj : input->operands[input->oper[pi]])
                exoperands_up.insert(pj);
  //          break;
        }
//#endif
 
  }

/*  for (temporary t1 : input->tmp[b]) {
    if (!ls->is_dead(t1)) {
      int val = ls -> r(t1).val();
      if (!hardware_regs.count(val)) {
        continue;
      }
      //input -> definer[t] the operand that defines t
      //input -> users[t] the operand that defines t
      operation o = input -> def_opr[t1];
      if (ls -> is_inactive(o)) continue;
      int cyc = ls -> c(o).val();
      if (mp[val] < cyc) {
	mp[val] = cyc;
	extemps[val] = t1;
	exopers.insert(o);
        for (operand p : input -> users[t1])
            exoperands.insert(p);
      }
      if (mp_up[val] > cyc) {
        mp_up[val] = cyc;
        extemps_up[val] = t1;
	exopers_up.insert(o);
        for (operand p : input -> users[t1])
            exoperands_up.insert(p);

      }
    }
  }
*/
  if (b != input->B.size() -1)  {
      for (std::map<int,int>::iterator it = extemps.begin(); it != extemps.end(); ++it) {
        extmps[b].insert(it -> second);
        extmpsloc.insert(it -> second);
      }

      for (std::set<int>::iterator it = exopers.begin(); it != exopers.end(); ++it) {
	exops[b].insert(*it);
	exopsloc.insert(*it);
      }

//#ifdef OPERS
      for (operand p: exoperands) {
        exopas[b].insert(p);
        exopasloc.insert(p);
      }
//#endif
  }

  if (b != 0) {
      for (std::map<int,int>::iterator it = extemps_up.begin(); it != extemps_up.end(); ++it) {
        extmps[b-1].insert(it -> second);
        extmpsloc.insert(it -> second);
      }
      for (std::set<int>::iterator it = exopers_up.begin(); it != exopers_up.end(); ++it) {
	exops[b-1].insert(*it);
	exopsloc.insert(*it);
      }

//#ifdef OPERS
      for(operand p: exoperands_up) {
        exopas[b-1].insert(p);
        exopasloc.insert(p);
      }
//#endif

  }


  for (temporary t1 : input->tmp[b]) {
    if (!ls->is_dead(t1)) {
      if (extmpsloc.count(t1)) {
        rsol[t1] = ls -> r(t1).val();
        continue;
      }
      constraint(r(t1) == ls->r(t1));
    }
  }

  // The instruction decides also if the operation is active
  for (operation o : input->ops[b]) {
    if (exopsloc.count(o)) {
        isol[o] = ls -> i(o).val();
        continue;
    }
    constraint(i(o) == ls->i(o));
  }
  
  for (operation o : input->ops[b]) {
    if (!ls->is_inactive(o)) {
      if (exopsloc.count(o)) {
        csol[o] = ls -> c(o).val();
        continue;
      }
      constraint(c(o) == ls->c(o));
    }
  }
  // The following is exactly the same as in globalmodel
  for (operand p : input->ope[b]) {
#ifdef OPERS
    if (exopasloc.count(p)) {
        //ysol[p] = ls -> y(p).val()
        continue;
    }
#endif
    constraint(y(p) == ls->y(p));
  }
}


bool SecModel::master(const MetaInfo& mi) {
  //std::cout << "Master" << std::endl;
  if (mi.type() == MetaInfo::PORTFOLIO) {
    //std::cout << "Master Portfolio" << std::endl;
    assert(mi.type() == MetaInfo::PORTFOLIO);
    return true; // default return value for portfolio master (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    //std::cout << "Master Restart" << std::endl;
    if (mi.last() != NULL) {
      //std::cout << "Master Restart constrain" << std::endl;
      constrain(*mi.last());
    }
    mi.nogoods().post(* this);
    return true; // forces a restart even if a solution has been found
  }
  GECODE_NEVER;
}

bool SecModel::slave(const MetaInfo& mi) {
  //std::cout << "Slave" << std::endl;
  if (mi.type() == MetaInfo::PORTFOLIO) {
    //std::cout << "Slave portfolio" << std::endl;
    //post_complete_branchers(mi.asset());
    if (monolithic) {
        //std::cout << "Slave Monolithic: " << mi.asset() << std::endl;
        sec_r.seed(options->seed() + mi.asset());
        post_complete_branchers(mi.asset());
    }
    else {
        //std::cout << "Slave non monolithic" << std::endl;
        post_unassigned_branchers(mi.asset());
    }
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    //std::cout << "Slave restart: " << mi.restart() << " is last solution null? " << (mi.last() == NULL) << std::endl;
    if ((mi.last()!= NULL || mi.restart() > 0) && (sec_p > 0.0)) {
      if (mi.last() != NULL) {
        //std::cout << "Calling Next" << std::endl;
        next(static_cast<const SecModel&>(*mi.last()));
      }
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

void SecModel::first(void) {
  //std::cout << "First" << std::endl;
  return;
}


void SecModel::next(const SecModel& b) {

  if (monolithic) {
    //std::cout << "Next monolithic: cost: " << b.cost() << std::endl;
    {
        IntVarArgs instr, linstr;
        for (operation o: input -> O) {
          instr << i(o);
          linstr << b.i(o);
        }

        relax(*this, instr, linstr, sec_r, sec_p);
    }

  // temporaries
    {
        IntVarArgs temp, ltemp;
        for (operand p : input -> P) {
          temp << y(p);
          ltemp << b.y(p);
        }
        relax(*this, temp, ltemp, sec_r, sec_p);
    }


  // Cycles
    {
        // Relax all active variables.
        // relax(*this, v_a, b.v_a, sec_r, 1.0);
        IntVarArgs cycles, lcycles;
        for (operation o : input -> O) {
          if (b.a(o).val()) { // if activated
            cycles << c(o);
            lcycles << b.c(o);
          }
        }
        relax(*this, cycles, lcycles, sec_r, sec_p);
    }

  // Registers
    {
        IntVarArgs lregs, regs;
        for (temporary t : input->T) {
          if (b.l(t).assigned() && b.l(t).val()) { // if the tempoorary is assigned
            lregs << b.r(t);
            regs << r(t);
          }
        }
        // Regs
        relax(*this, regs, lregs, sec_r, sec_p);
    }

    //relax(*this, v_r, b.v_r, sec_r, sec_p);
    //relax(*this, v_c, b.v_c, sec_r, sec_p);
    //relax(*this, v_a, b.v_a, sec_r, sec_p);
    //relax(*this, v_y, b.v_y, sec_r, sec_p);
    //relax(*this, v_i, b.v_i, sec_r, sec_p);
  }
  else { // not monolithic
      // Instructions
      //std::cout << "Next non monolithic" << std::endl;
      {
        IntVarArgs instr, linstr;
        for (block bl: input -> B) {
            for (std::set<operation>::iterator it = exops[bl].begin(); it != exops[bl].end(); ++it) {
                
              instr << i(*it);
              linstr << b.i(*it);
            }
        }
        relax(*this, instr, linstr, sec_r, sec_p);
      }

      // cycles
      {
        IntVarArgs cyc, lcyc;
        for (block bl: input -> B) {
            for (std::set<operation>::iterator it = exops[bl].begin(); it != exops[bl].end(); ++it) {
              if (b.a(*it).val()) {
                  cyc << c(*it);
                  lcyc << b.c(*it);
              }
            }
        }
        relax(*this, cyc, lcyc, sec_r, sec_p);
      }

      // operands

#ifdef OPERS
      {
        IntVarArgs opes, lopes;
        for (block bl: input -> B) {
            for (std::set<operand>::iterator it = exopas[bl].begin(); it != exopas[bl].end(); ++it) {
                opes << y(*it);
                lopes << b.y(*it);
            }
        }
        relax(*this, opes, lopes, sec_r, sec_p);
      }
#endif


      // registers
      {
        IntVarArgs regs, lregs;
        for (block bl: input -> B) {
            for (std::set<temporary>::iterator it = extmps[bl].begin(); it != extmps[bl].end(); ++it) {
              if (b.l(*it).assigned() && b.l(*it).val()) {
                  regs << r(*it);
                  lregs << b.r(*it);
              }
            }
        }
        relax(*this, regs, lregs, sec_r, sec_p);
      }
   }

}


void SecModel::constrain(const Space & _b) {
    const SecModel& b = static_cast<const SecModel&>(_b);

    //std::cout << "constrain with cost: " << b.cost() << std::endl;
    BoolVarArgs bh;
    
    // get better solution
    for (uint i = 0; i < input -> N; i++) 
        if (b.cost()[i].assigned())
            constraint(cost()[i] < b.cost()[i]);
    return;
}




void SecModel::relax_all(const SecModel& b, double relax_rate) {
  //std::cout << "Next" << std::endl;
    set <operand> unops;
    {
        IntVarArgs temp, ltemp;
        for (block bi: input -> B) {
            for (operand p : exopas[bi]) {
              unops.insert(p);
              if (b.y(p).assigned()) {
                  temp << y(p);
                  ltemp << b.y(p);
              }
            }
        }
        relax(*this, temp, ltemp, sec_r, relax_rate);
    }

    double relax_rate_2 = 0.1;

    //double relax_rate = 0.2;
    //std::cout << "Next monolithic: cost: " << b.cost() << std::endl;
    {
        IntVarArgs instr, linstr;
        for (operation o: input -> O) {
          if (b.i(o).assigned()) {
              instr << i(o);
              linstr << b.i(o);
          }
        }
        relax(*this, instr, linstr, sec_r, relax_rate_2);
    }

#if 1
  // temporaries
    {
        IntVarArgs temp, ltemp;
        for (operand p : input -> P) {
          if (b.y(p).assigned() && !unops.count(p)) {
              temp << y(p);
              ltemp << b.y(p);
          }
        }
        if (temp.size()>0)
            relax(*this, temp, ltemp, sec_r, relax_rate);
    }
#endif


  // Cycles
    {
        // Relax all active variables.
        // relax(*this, v_a, b.v_a, sec_r, 1.0);
        IntVarArgs cycles, lcycles;
        for (operation o : input -> O) {
          if (b.a(o).val() && b.c(o).assigned()) { // if activated
            cycles << c(o);
            lcycles << b.c(o);
          }
        }
        relax(*this, cycles, lcycles, sec_r, relax_rate_2);
    }

  // Registers
    {
        IntVarArgs lregs, regs;
        for (temporary t : input->T) {
          if (b.l(t).assigned() && b.l(t).val() && b.r(t).assigned()) { // if the tempoorary is assigned
            lregs << b.r(t);
            regs << r(t);
          }
        }
        // Regs
        relax(*this, regs, lregs, sec_r, relax_rate_2);
    }

}


void SecModel::copy_unassigned(SecModel& b) {

 for (block bi: input -> B) {
    for (temporary t: b.extmps[bi]) 
        extmps[bi].insert(t);
    for (operation o: b.exops[bi]) 
        exops[bi].insert(o);
    for (operand p: b.exopas[bi]) 
        exopas[bi].insert(p);
 }

}
