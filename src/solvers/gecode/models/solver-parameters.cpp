/*  Main authors:
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


#include "solver-parameters.hpp"

SolverParameters::SolverParameters(Json::Value root) :
  // Program parameters
  solver                     (get_scalar<string>(getRoot(root, "solver"))),
  has_solution               (get_scalar<bool>(getRoot(root, "has_solution"))),
  proven                     (get_scalar<bool>(getRoot(root, "proven"))),
  cost                       (get_vector<int>(getRoot(root, "cost"))),
  presolver_time             (get_scalar<int>(getRoot(root, "presolver_time"))),
  solver_time                (get_scalar<int>(getRoot(root, "solver_time")))

{
  if (has_solution) {
    registers=get_vector<int>(getRoot(root, "registers"));
    instructions=get_vector<int>(getRoot(root, "instructions"));
    cycles=get_vector<int>(getRoot(root, "cycles"));
    temporaries=get_vector<int>(getRoot(root, "temporaries"));
  }
  ;
}

Json::Value SolverParameters::getRoot(Json::Value root, string p) {
  return root.get(p, "null");
}

void SolverParameters::get_element(Json::Value root, bool & b) {
  assert(root.isBool());
  b = root.asBool();
}

void SolverParameters::get_element(Json::Value root, int & i) {
  assert(root.isInt());
  i = root.asInt();
}

void SolverParameters::get_element(Json::Value root, double & d) {
  assert(root.isDouble());
  d = root.asDouble();
}

void SolverParameters::get_element(Json::Value root, string & s) {
  assert(root.isString());
  s = root.asString();
}

