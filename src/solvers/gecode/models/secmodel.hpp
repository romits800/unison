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


#ifndef __SEC_MODEL__
#define __SEC_MODEL__

#include "completemodel.hpp"
#include "globalmodel.hpp"
#include "seclocalmodel.hpp"
#include "branchers/sec_value.hpp"
#include "branchers/solutionbrancher.hpp"
#include "branchers/boolsolutionbrancher.hpp"
#include <random>

using namespace Gecode;
using namespace std;

class SecLocalModel;

class SecModel : public GlobalModel {

public:

  // global block offset
  IntVarArray v_gb;
  IntVarArray v_lgs;
  IntVarArray v_lge;

  // Implementation 1
  
  // Register Array
  IntVarArray v_rtle;

  // Register Array Sorted Mapping
  IntVarArray v_rtlemap;

  // Operation Array
  IntVarArray v_opcy;

  // Operation Cycle Array Sorted Map
  IntVarArray v_opcymap;

  
  // Implementation 2
  
  // temp size array with max (le(t'))
  IntVarArray v_lk;

  // help variable arrays
  IntVarArray v_tat;
  IntVarArray v_tbt;

  // operation size array with max (le(t'))
  IntVarArray v_ok;

  vector<operation> memops;

  vector<temporary> tats;
  vector<temporary> tbts;


  map <block, set <temporary>> extmps;
  map <block, set <operation>> exops;
  map <block, set <operand>> exopas;
  set <register_atom> hardware_regs;
  // Gecode space methods
  SecModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  SecModel(SecModel& cg);

  SecModel* copy(void);

  // Branchers
  void post_branchers(void);

  // Different Solution
  void post_different_solution(SecModel * g1, bool unsat);
    
  // Security Constraints
  void post_random_register_constraints(void);
  void post_secret_register_constraints(void);
  void post_secret_mem_constraints(void);
  void post_random_mem_constraints(void);
  void post_security_constraints(void);

  void post_implied_constraints(void);
  void post_strict_constraints(void);
  // constraints for the adjacent ops
  void post_connecting_constraints(void);
  void post_tt_constraints(void);
  
  void post_m1_constraints(void);
  void post_m2_constraints(void);
  void post_r1_constraints(void);
  void post_r2_constraints(void);

  void init_tts(void);
  
  BoolVar subseq(temporary t1, temporary t2);
  BoolVar msubseq(operation o1, operation o2);
  
  BoolVar subseq1(temporary t1, temporary t2);
  BoolVar msubseq1(operation o1, operation o2);


  BoolVar subseq2(temporary t1, temporary t2);
  BoolVar msubseq2(operation o1, operation o2);

  // Global constraints
  void post_global_cycle_offset(void);
  IntVar gb(block b) const {return v_gb[b]; }
  IntVar lgs(block b) const {return v_lgs[b]; }
  IntVar lge(block b) const {return v_lge[b]; }
  block bot (temporary t);

  void apply_solution(SecLocalModel * ls);
  void apply_global_solution(SecModel * sm);

  void post_unassigned_branchers(unsigned int s);

  void clear_extmps();
  // int select_value_tt(IntVar x, unsigned int i);
  // void next(const SecModel& l);

 // r: random number for LNS
  Rnd sec_r;
  // p: relax parameter for LNS
  double sec_p;
 
  map <int, int> rsol; // temporary to register map
  map <int, int> csol; // operation to cycle map
  map <int, int> isol; // operation to instruction map
  //map <int, int> isol; // operation to instruction map
  // Restart
  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);
  void first(void);
  void next(const SecModel& b);

  void constrain(const Space & _b);

  std::mt19937_64 rng;

  std::uniform_real_distribution<double> *unif; //(0, 1);


  // solution brancher for monolithic

  void post_solution_brancher(SecModel *sol);
  bool monolithic;
  void set_monolithic(bool val);


  void post_complete_branchers(unsigned int s);

};

#endif
