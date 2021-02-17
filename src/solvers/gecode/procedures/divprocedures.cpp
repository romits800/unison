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


#include "divprocedures.hpp"


unsigned long int
global_limit_2(Parameters * input, ModelOptions * options, int best) {
  unsigned long int globalLimit =
    options->global_budget() * input->O.size();
  return globalLimit +
    (best == Int::Limits::max ? options->global_setup_limit() : 0);
}


int
find_optimal_solution(DivModel *base, DecompDivModel *dm, ModelOptions *options) {

      base->post_div_branchers();
      base->post_diversification_constraints(); // Diversification constraint


      if (base->status() == SS_FAILED) {
	cerr << div() << "Status failed." << endl;
	return -1;
      }

      // Configuration for the engine 
      Gecode::RestartMode restart = options->restart();
      Search::Cutoff* c;
      Search::Options o;
      unsigned long int s_const = options->restart_scale();

      if (restart == RM_LUBY ){
	c = Search::Cutoff::luby(s_const);
      } else if (restart == RM_CONSTANT) {
	c = Search::Cutoff::constant(s_const);
      } else {
	c = Search::Cutoff::constant(1000);
      }

      o.cutoff = c;

      // Start Engine
      RBS<DivModel,BAB> e(base, o);

      if (base->status() == SS_FAILED) {
	cerr << div() << "Status failed." << endl;
      }

      cerr << div() << "Start generating first solution" << endl;

      DivModel *nextg = e.next();

      cerr << div() << "Done generating first solution" << endl;

      //g->apply_div_solution(nextg);
      dm->post_div_decomp_branchers(nextg);

      if (dm->status() == SS_FAILED) {
	cerr << div() << "Failed applying div solution!" << endl;
	return -1;
      }

      // cerr << div() << "Done applying div solution" << endl;
      return 0;
      //      delete nextg;		// 

}





Solution<DivModel>
solve_global(DivModel * base, IterationState & state, vector<int> & best,
             GIST_OPTIONS * go, int iteration) {
  (void)go; (void)iteration;
  // Create global problem with aggressiveness a
  DivModel * g = (DivModel*) base->clone();

  g->set_aggressiveness(state.a);
  g->set_connect_first(state.cf);
  g->post_branchers();

  if (base->input->B.size() == 1) g->post_callee_saved_branchers();
#ifdef GRAPHICS
  if (base->options->gist_global() &&
     (base->options->gist_iteration() == -1 ||
      base->options->gist_iteration() == iteration)) Gist::dfs(g, *go);
#endif

  // cout << "solve_global 1" << endl;
  // Global options
  // Search::Stop * globalStop =
  //   new_stop(global_limit_2(base->input, base->options, best[0]), base->options);
  Search::Options globalOptions;
  // globalOptions.stop = globalStop;

  // Global search engine
  DFS<DivModel> e(g, globalOptions);
  delete g;

  // Solve the global problem
  DivModel * g1 = e.next();

  SolverResult r;
  if (g1 == NULL)
    r = UNSATISFIABLE; //globalStop->stop(e.statistics(), globalOptions) ? LIMIT : UNSATISFIABLE;
  else {
    r = SOME_SOLUTION;
  }
  // delete globalStop;

  // if (!base->options->disable_global_shaving() && r == SOME_SOLUTION)
  //   r = shave_local_costs(g1);

  // Return result, solution (if applicable) and failures
  return Solution<DivModel>(r, g1, e.statistics().fail, e.statistics().node);
}






string div() { return "[div]\t "; }
