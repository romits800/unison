/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#ifndef __DIV_MODEL__
#define __DIV_MODEL__

#include "completemodel.hpp"
#include "globalmodel.hpp"
#include "localdivmodel.hpp"
#include "solver-parameters.hpp"
// #include "branchers/merit.hpp"
// #include "branchers/value.hpp"
#include "branchers/solutionbrancher.hpp"
#include "time.h"

using namespace Gecode;
using namespace std;

typedef struct  {
  uint start;
  uint end;
} gadget_t;

class LocalDivModel;

class DivModel : public GlobalModel {

public:

  // Diff array
  IntVarArray v_diff;

  // Hamming distance between operations
  IntVarArray v_hamm;

  // Register Hamming distance between operands
  IntVarArray v_reghamm;

  // Gadgets distance between operations
  IntVarArray v_gadget;

  // Global cycles array
  IntVarArray v_gc;

  // Channel of gc
  SetVarArray v_oc;

  // Distance
  IntVar dist;
  // // Levenshtein distance
  // IntVarArray v_lev;


  // p: relax parameter for LNS
  double div_p;
  // r: random number for LNS
  Rnd div_r;

  // Minimum allowed distance
  int mindist;

  SolverParameters *solver;


  vector<operation> branch_operations;
  vector<operation> real_operations;

  vector<gadget_t> gadgets;
  vector<int> gadgets_operations;
  void set_solver(JSONVALUE root);

  void set_random(Rnd r) {div_r = r;};

  void set_relax(double p) {div_p = p;};

  // Variable accessors

  IntVar diff(operation o) const {return v_diff[o]; }

  IntVar hamm(operation o) const {return v_hamm[o]; }
  
  IntVar reghamm(operand p) const {return v_reghamm[p]; }

  IntVar gadget(operation o) const {return v_gadget[o]; }

  IntVar gc(operation o) const {return v_gc[o]; }

  // Ordered by the issue cycles
  SetVar oc(int c) const {return v_oc[c]; }



  // Gecode space methods

  DivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  DivModel(DivModel& cg);

  DivModel* copy(void);

  // Branchers
  void post_div_branchers(void);
  void post_random_branchers(void);
  void post_clrandom_branchers(void);
  void post_cloriginal_branchers(void);

  // Diversification Constraints
  void post_diversification_channel(void);
  void post_diversification_constraints(void);
  void post_diversification_diffs(void);
  void post_diversification_br_diffs(void);
  void post_diversification_hamming(void);
  void post_diversification_reghamming(void);
  void post_diversification_reg_gadget(void);
  void post_global_cycles(void);
  void post_levenshtein(const DivModel & b);
  void post_levenshtein_set(const DivModel & b);


  // Check if the type of the operation is a branch, i.e. BRANCH or CALL
  bool is_real_type( int o);
  bool is_branch_type(int o);


  // Constrain function

  void constrain(const Space & _b);
  // The same constraints as the constrain function
  void post_constrain(DivModel* b);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Next for relaxing variable for LNS

  void next(const DivModel& l);
  void first(void);

};

#endif
