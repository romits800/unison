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


#ifndef __SEC_DECOMP_DIV_MODEL__
#define __SEC_DECOMP_DIV_MODEL__

#include "secdivmodel.hpp"
#include "branchers/boolsolutionbrancher.hpp"

using namespace Gecode;
using namespace std;

class SecDecompDivModel : public SecDivModel {

public:


  // Gecode space methods

  SecDecompDivModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  SecDecompDivModel(SecDecompDivModel& cg);

  SecDecompDivModel* copy(void);

  // Branchers
  void post_div_decomp_branchers(void);
  void apply_div_solution(SecDivModel * d);
  void relax_div_solution(SecDecompDivModel * d);
  
  // Branch types

  // Constrain function

  void constrain(const Space & _b); // 
  // The same constraints as the constrain function
  void post_constrain(SecDecompDivModel* b);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Next for relaxing variable for LNS

  void next(const SecDecompDivModel& l);


};

#endif
