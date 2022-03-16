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


#ifndef __SEC_LOCAL_DIV_PROCEDURES__
#define __SEC_LOCAL_DIV_PROCEDURES__

#include "models/seclocaldivmodel.hpp"
#include "procedures/commonprocedures.hpp"

using namespace std;
using namespace Gecode;

// Creates a local problem for block b out of the global solution g1
Solution<SecLocalDivModel> local_problem(SecDecompDivModel * g1, block b);

SecLocalDivModel *make_div_local(const SecDecompDivModel * gs, block b, int sc, IntPropLevel p_ipl);

SecLocalDivModel *make_div_local(const SecDecompDivModel * gs, block b, int sc);


SecLocalDivModel *init_local_problem(SecDecompDivModel * g, block b, int sc);


class LocalSolution {
public:
  SecLocalDivModel * solution;
  block b;
  LocalSolution() : solution(0), b(0) {}
  LocalSolution(SecLocalDivModel * solution1, block b1) :
    solution(solution1), b(b1) {}

};


RBS<SecLocalDivModel,BAB> *
init_local_engine(SecLocalDivModel *l, ModelOptions *options);

// Prefix for debug output
string local(block b);

#endif
