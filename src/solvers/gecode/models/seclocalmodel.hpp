/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
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


#ifndef __SEC_LOCAL_MODEL__
#define __SEC_LOCAL_MODEL__

// #include "model.hpp"
#include "localmodel.hpp"
#include "secmodel.hpp"

using namespace Gecode;
using namespace std;

class SecModel;

class SecLocalModel : public LocalModel {

public:

  // Register Array
  IntVarArray v_rtle;

  // Register Array Sorted Mapping
  IntVarArray v_rtlemap;

  // Operation Array
  IntVarArray v_opcy;

  // Operation Cycle Array Sorted Map
  IntVarArray v_opcymap;

  
  // Implementation 2

  // help variable array
  // IntVarArray v_tt;
  // temp size array with max (le(t'))
  IntVarArray v_lk;

  // operation size array with max (le(t'))
  IntVarArray v_ok;


  vector<operation> memops;
  
  SecLocalModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl,
		const SecModel * gs, block b);
  
  SecLocalModel(SecLocalModel& cg);

  SecLocalModel* copy(void);

  // Security Constraints
  void post_random_register_constraints(void);
  void post_secret_register_constraints(void);
  void post_secret_mem_constraints(void);
  void post_security_constraints(void);

  void post_implied_constraints(void);
  // void post_implied_subseq(void);
    
  void post_m1_constraints(void);
  void post_m2_constraints(void);
  void post_r1_constraints(void);
  void post_r2_constraints(void);
  
  BoolVar subseq(temporary t1, temporary t2);
  BoolVar msubseq(operation o1, operation o2);
  
  BoolVar subseq1(temporary t1, temporary t2);
  BoolVar msubseq1(operation o1, operation o2);


  BoolVar subseq2(temporary t1, temporary t2);
  BoolVar msubseq2(operation o1, operation o2);

  // Branchers
  void post_sec_brancher(void);
  void post_branchers(char search);

  // Master and Slave Configuration
  virtual bool master(const MetaInfo& mi);
  virtual bool slave(const MetaInfo& mi);

  void apply_sec_solution(const SecModel * gs);

  bool is_in(temporary t) const {
    int temp_size = T().size();
    return temp(t) < temp_size && temp(t) >= 0;
  }

};

#endif
