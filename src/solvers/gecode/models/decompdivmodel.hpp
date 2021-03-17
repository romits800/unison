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


#ifndef __DECOMP_DIV_MODEL__
#define __DECOMP_DIV_MODEL__

#include "divmodel.hpp"
#include "branchers/boolsolutionbrancher.hpp"
// #include "branchers/merit.hpp"
// #include "branchers/value.hpp"

using namespace Gecode;
using namespace std;

class DecompDivModel : public DivModel {

public:

  // Diff array
  // IntVarArray v_diff;
  
  // // Hamming distance between operations
  // IntVarArray v_hamm;
  

  // // p: relax parameter for LNS
  // double div_p;
  // // r: random number for LNS
  // Rnd div_r;

  // void set_random(Rnd r) {div_r = r;};

  // void set_relax(double p) {div_p = p;};


  // Variable accessors

  // IntVar diff(operation o) const {return v_diff[o]; }

  // IntVar hamm(operation o) const {return v_hamm[o]; }



  // Gecode space methods

  DecompDivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  DecompDivModel(DecompDivModel& cg);

  DecompDivModel* copy(void);

  // Branchers
  void post_div_decomp_branchers(void);
  void apply_div_solution(DivModel * d);
  void relax_div_solution(DecompDivModel * d);
  // void post_div_branchers(void);
  // void post_random_branchers(void);
  // void post_clrandom_branchers(void);
  // void post_cloriginal_branchers(void);
  
  // // Diversification Constraints
  // void post_diversification_constraints(void); // Diversification constraints
  // void post_diversification_diffs(void); // Diversification constraints
  // void post_diversification_hamming(void); // Diversification constraints
  // void post_diversification_channel(void);
  // void post_diversification_br_diffs(void);
  // void post_diversification_reghamming(void);
  // void post_diversification_reg_gadget(void);
  // void post_global_cycles(void);
  // void post_levenshtein(const DecompDivModel & b);
  // void post_levenshtein_set(const DecompDivModel & b);


  
  // Branch types
  // bool is_branch_type(int o);
  // bool is_real_type(int o);

  // Constrain function

  void constrain(const Space & _b); // 
  // The same constraints as the constrain function
  void post_constrain(DecompDivModel* b);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Next for relaxing variable for LNS

  void next(const DecompDivModel& l);


};

#endif
