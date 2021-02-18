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


#ifndef __LOCAL_DIV_MODEL__
#define __LOCAL_DIV_MODEL__

#include "localmodel.hpp"
#include "decompdivmodel.hpp"
//#include "divmodel.hpp"		// 
#include "branchers/filters.hpp"
#include "branchers/printers.hpp"
#include "branchers/routingbrancher.hpp"
#include "branchers/pressureschedulingbrancher.hpp"

#include "solver-parameters.hpp"
#include "branchers/solutionbrancher.hpp"

// #include <gecode/int.hh>

using namespace Gecode;
using namespace std;

// class GlobalModel;
class DecompDivModel;                 //
// class DivModel;                 //

class LocalDivModel : public LocalModel {

public:

  // Diff array
  IntVarArray v_diff;

  // Hamming distance between operations
  IntVarArray v_hamm;

  // p: relax parameter for LNS
  double div_p;
  // r: random number for LNS
  Rnd div_r;

  void set_random(Rnd r) {div_r = r;};

  void set_relax(double p) {div_p = p;};

  SolverParameters *solver;

  void set_solver(JSONVALUE root);
  
  // Variable accessors

  IntVar diff(operation o) const {return v_diff[o]; }

  IntVar hamm(operation o) const {return v_hamm[o]; }

  // Gedode space methods

  // LocalDivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl,
  //               const DecompDivModel * gs, block b);

  LocalDivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl,
                const DecompDivModel * gs, block b, int seed_correction);

  LocalDivModel(LocalDivModel& cg);

  LocalDivModel* copy(void);

  // Constrain cost
  void constrain_total_cost(int cost);
  
  // Post constraints
  void post_diversification_constraints(void); // Diversification constraints
  void post_diversification_diffs(void); // Diversification constraints
  void post_diversification_hamming(void); // Diversification constraints

  // Branch types
  bool is_branch_type(int o);
  bool is_real_type(int o);

  // Constrain function

  void constrain(const Space & _b);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Next for relaxing variable for LNS

  void next(const LocalDivModel& l);

  void post_div_branchers(void);


};

#endif
