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


#ifndef __SEC_PROCEDURES__
#define __SEC_PROCEDURES__

#include "models/secmodel.hpp"
#include "procedures/commonprocedures.hpp"

using namespace std;
using namespace Gecode;

// Discards ineffective callee-saved copies (that is, copies that cannot be
// useful)
void presolve_effective_callee_saved_spilling(SecModel * base);

// Updates the lower bound of the cost of each block with the minimum resource
// consumption added by copies that must be active
void presolve_minimum_consumption(SecModel * base);

// Computes, posts and propagates local cost lower bounds by relaxation assuming
// different levels of callee-saved spilling
void presolve_relaxation(SecModel * base, GIST_OPTIONS * lo);

// Computes, posts and propagates instruction nogoods conditioned to local costs
void presolve_shaving(SecModel * base);

// Computes and posts lower bounds on the cost of each block conditioned by
// global operand connection decisions
void presolve_global_cluster_impact(SecModel * base, GIST_OPTIONS * lo);

void presolve_global_cluster_impact(
     SecModel * base, global_cluster gc, bool connect, GIST_OPTIONS * lo);

// Computes and posts lower bounds on the global cost function by binary search
void presolve_global_shaving(SecModel * base);

// Computes, posts and propagates activation nogoods
void presolve_global_activation_shaving(SecModel * base);

// Gives the global solver timeout
unsigned long int
global_timeout(Parameters * input, ModelOptions * options, int best);

// Gives a global solution
Solution<SecModel>
solve_global(SecModel * base, IterationState & state, vector<int> & best,
             GIST_OPTIONS * go, int iteration);

// Gives a global solution where the given activation classes are inactive
Solution<SecModel>
deactivate(SecModel * base, SecModel * gs,
           vector<activation_class> & acs);

// Shaves the individual cost of each block
SolverResult shave_local_costs(SecModel * base);

// Solves the entire problem without a decomposition scheme
Solution<SecModel> solve_monolithic(SecModel * base, SecModel * sol, GIST_OPTIONS * go, unsigned int FACTOR);

// Solves the entire problem without a decomposition scheme in parallel
Solution<SecModel>
solve_monolithic_parallel(SecModel * base, GIST_OPTIONS * go);

// Prefix for debug output
string global();

// Prefix for debug output
string pre();

// Prefix for debug output
string monolithic();

#endif
