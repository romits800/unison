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


#ifndef __SEC_MAX_DIV_MODEL__
#define __SEC_MAX_DIV_MODEL__

#include "secdivmodel.hpp"

using namespace Gecode;
using namespace std;

// class LocalDivModel;

class SecMaxDivModel : public SecDivModel {

public:


  // Distance
  IntVar maxdist; //  = IntVar(*this, 1, 10000);
  // // Levenshtein distance
  // IntVarArray v_lev;

  vector<SecMaxDivModel *> input_solutions;

  // Gecode space methods

  SecMaxDivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);
  SecMaxDivModel(SecMaxDivModel& cg);

  SecMaxDivModel* copy(void);

  // Post input solutions

  void post_input_solution_constrain(void);

  //

  // Constrain function

  void constrain(const Space & _b);
  void constrain_levenshtein(const SecMaxDivModel & b);
  void constrain_levenshtein_set(const SecMaxDivModel & b);
  void post_levenshtein_set(void);
  void post_levenshtein(void);




};

#endif
