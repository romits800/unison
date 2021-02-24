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

Solution<LocalDivModel> local_problem(DecompDivModel * g1, block b) {
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



// LocalDivModel * make_div_local(const DecompDivModel * gs, block b) {
//   return make_div_local(gs, b, gs->ipl);
// }

// LocalDivModel * make_div_local(const DecompDivModel * gs, block b, IntPropLevel p_ipl) {
//   return new LocalDivModel(gs->input, gs->options, p_ipl, gs, b);
// }

LocalDivModel * make_div_local(const DecompDivModel * gs, block b, int seed_correction, IntPropLevel p_ipl) {
  return new LocalDivModel(gs->input, gs->options, p_ipl, gs, b, seed_correction);
}


LocalDivModel * make_div_local(const DecompDivModel * gs, block b, int seed_correction) {
  return make_div_local(gs, b, seed_correction, gs->ipl);
}




LocalDivModel *
init_local_problem(DecompDivModel *g, block b, int seed_correction) {

  LocalDivModel * l = (LocalDivModel *) make_div_local(g, b, seed_correction);
  l -> post_div_branchers();
  l -> post_diversification_constraints();
  if (l->status() == SS_FAILED) {
    std::cerr << b << ": Failed completely"  << std::endl;
    return NULL;
  }

  // double sumf = 0;
  // for (int i = 0; i< g->input->freq.size(); i++)
  //   sumf += g->input->freq[i];

  // double correction = l->f(b, 0).max() - l->f(b, 0).min();
  // correction = ((double)g->options->acceptable_gap() * correction )/100.;
  // int max_cost1 = l->f(b, 0).min() + correction; 

  // double correction2 = (1. - (g->input->freq[b] / sumf))*20; // 
  // double ag = 100. + ((double)g->options->acceptable_gap() + correction2);
  // int max_cost2 = ceil((ag*(float)(l->f(b, 0).min()))/100.);

  // int max_cost = max_cost1>max_cost2 ? max_cost1 : max_cost2;
  // std::cerr << b << ":" << l -> f(b,0) << std::endl;
  
  for (int i=1; i< 5; i++) {
    LocalDivModel *l0 = (LocalDivModel *) l->clone();
    double ag = 100. + i*((double)g->options->acceptable_gap());
    int max_bound = floor((ag*(l0->f(b,0).min()))/100.);
    l0-> constrain_total_cost(max_bound > l0->f(b,0).min() ? max_bound : l0->f(b,0).min() + 1);

    if (l0->status() == SS_FAILED) {
      if (l0!=NULL) delete l0;
      continue;
    }
    else {
      if (l!=NULL) delete l;
      // std::cerr << b << ":" << l0 -> f(b,0) << std::endl;
      return l0;
    }
  }
  
  // std::cerr << l -> f(b,0) << std::endl;
  // std::cerr << "Not constraining l: " << b << std::endl;
  return l;


}


RBS<LocalDivModel,BAB> *
init_local_engine(LocalDivModel *l, ModelOptions *options) {

  Search::Options localOptions;
  Gecode::RestartMode restart = options->restart();
  Search::Cutoff* c;
  unsigned long int s_const = options->restart_scale();

  if (restart == RM_LUBY ){
    c = Search::Cutoff::luby(s_const);
  } else if (restart == RM_CONSTANT) {
    c = Search::Cutoff::constant(s_const);
  } else {
    c = Search::Cutoff::constant(1000);
  }

  localOptions.cutoff = c;

  Search::Stop * localStop = new_stop(options->local_limit(), options);
  localOptions.stop = localStop;

  // Restart-based meta-engine
  return new  RBS<LocalDivModel,BAB>(l, localOptions);

}
