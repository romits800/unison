/*  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  Contributing authors:
 *    Daniel Lundén <daniel.lunden@sics.se>
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


#include "solver-parameters.hpp"

SolverParameters::SolverParameters(JSONVALUE root) :

  // Program parameters

  // registers                  (get_vector<int>(getRoot(root, "registers"))),
  // instructions               (get_vector<int>(getRoot(root, "instructions"))),

  // cycles                     (get_vector<int>(getRoot(root, "cycles"))),
  // temporaries                (get_vector<int>(getRoot(root, "temporaries"))),
  solver                     (get_scalar<string>(getRoot(root, "solver"))),
  has_solution               (get_scalar<bool>(getRoot(root, "has_solution"))),
  proven                     (get_scalar<bool>(getRoot(root, "proven"))),
  cost                       (get_vector<int>(getRoot(root, "cost"))),
  failures                   (get_scalar<int>(getRoot(root, "failures"))),
  nodes                      (get_scalar<int>(getRoot(root, "nodes"))),
  presolver_time             (get_scalar<int>(getRoot(root, "presolver_time"))),
  gecode_presolving_time     (get_scalar<int>(getRoot(root, "gecode_presolving_time"))),
  solver_time                (get_scalar<int>(getRoot(root, "solver_time"))),

  global_int_variables       (get_scalar<int>(getRoot(root, "global_int_variables"))),

  global_bool_variables      (get_scalar<int>(getRoot(root, "global_bool_variables"))),

  global_set_variables       (get_scalar<int>(getRoot(root, "global_set_variables")))

{
  ;
}

QScriptValue SolverParameters::getRoot(QScriptValue root, string p) {
  return root.property(p.c_str());
}

void SolverParameters::get_element(QScriptValue root, bool & b) {
  assert(root.isBoolean());
  b = root.toBoolean();
}

void SolverParameters::get_element(QScriptValue root, int & i) {
  assert(root.isNumber());
  i = root.toInt32();
}

void SolverParameters::get_element(QScriptValue root, double & d) {
  assert(root.isNumber());
  d = root.toNumber();
}

void SolverParameters::get_element(QScriptValue root, string & s) {
  assert(root.isString());
  s = root.toString().toStdString();
}


// string Parameters::emit_json() {
//   stringstream json;
//   json << emit_json_line("B", B)
//        << emit_json_line("O", O)
//        << emit_json_line("P", P)
//        << emit_json_line("T", T)
//        << emit_json_line("block", oblock)
//        << emit_json_line("operands", operands)
//        << emit_json_line("temps", temps)
//        << emit_json_line("use", use)
//        << emit_json_line("adjacent", adjacent)
//        << emit_json_line("preassign", preassign)
//        << emit_json_line("width", width)
//        << emit_json_line("freq", freq)
//        << emit_json_line("aligned", aligned)
//        << emit_json_line("adist", adist)
//        << emit_json_line("minlive", minlive)
//        << emit_json_line("dep", dep)
//        << emit_json_line("activators", activators)
//        << emit_json_line("I", I)
//        << emit_json_line("R", R)
//        << emit_json_line("dist", dist)
//        << emit_json_line("class", rclass)
//        << emit_json_line("atoms", atoms)
//        << emit_json_line("instructions", instructions)
//        << emit_json_line("lat", lat)
//        << emit_json_line("bypass", bypass)
//        << emit_json_line("cap", cap)
//        << emit_json_line("con", con)
//        << emit_json_line("dur", dur)
//        << emit_json_line("off", off);
//   return json.str();
// }
