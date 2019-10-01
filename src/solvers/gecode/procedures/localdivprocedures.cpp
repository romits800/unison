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


#include "localdivprocedures.hpp"

Solution<LocalDivModel> local_problem(DivModel * g1, block b) {
  IntPropLevel ipl = IPL_DOM;
  if (g1->input->ops[b].size() > g1->options->consistency_threshold()) {
    ipl = IPL_BND;
    if (g1->options->verbose())
      cerr << local(b) << "large block (" << g1->input->ops[b].size()
           << " operations), reducing consistency level" << endl;
  }
  LocalDivModel * l =  make_div_local(g1, b, ipl);
  Gecode::SpaceStatus lss = l->status();
  SolverResult r;
  r = lss == SS_FAILED ? UNSATISFIABLE : UNKNOWN;
  return Solution<LocalDivModel>(r, l, 0, 0);
}



LocalDivModel * make_div_local(const DivModel * gs, block b) {
  return make_div_local(gs, b, gs->ipl);
}

LocalDivModel * make_div_local(const DivModel * gs, block b, IntPropLevel p_ipl) {
  return new LocalDivModel(gs->input, gs->options, p_ipl, gs, b);
}


string local(block b) {
  stringstream s;
  s << "[b" << b << "]\t ";
  return s.str();
}
