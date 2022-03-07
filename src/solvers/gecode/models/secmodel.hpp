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
//#include "localdivmodel.hpp"
// #include "solver-parameters.hpp"
// #include "branchers/solutionbrancher.hpp"
// #include "branchers/boolsolutionbrancher.hpp"
// #include "branchers/solutionbrancher_dfs.hpp"
// #include "branchers/boolsolutionbrancher_dfs.hpp"

using namespace Gecode;
using namespace std;

// typedef struct  {
//   uint start;
//   uint end;
// } gadget_t;

// class LocalDivModel;

class SecModel : public GlobalModel {

public:

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

  // operation size array with max (le(t'))
  IntVarArray v_ok;

  // Gecode space methods
  SecModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  SecModel(SecModel& cg);

  SecModel* copy(void);

  // Branchers
  
  // Security Constraints
  void post_random_register_constraints(void);
  void post_secret_register_constraints(void);
  void post_secret_mem_constraints(void);
  void post_security_constraints(void);

  BoolVar subseq(temporary t1, temporary t2);
  BoolVar msubseq(operation o1, operation o2);
  
  BoolVar subseq1(temporary t1, temporary t2);
  BoolVar msubseq1(operation o1, operation o2);


  BoolVar subseq2(temporary t1, temporary t2);
  BoolVar msubseq2(operation o1, operation o2);


 
  // void next(const SecModel& l);

};

#endif
