/*
 *  Main authors:
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
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


#include "solution_parameters.hpp"

SolParameters::SolParameters(JSONVALUE root) :

  // Solution parameters

  registers       (get_vector<int>(getRoot(root, "registers"))),
  instructions    (get_vector<int>(getRoot(root, "instructions"))),
  cycles          (get_vector<int>(getRoot(root, "cycles"))),
  temporaries     (get_vector<int>(getRoot(root, "temporaries"))),
  global_cycles   (get_vector<int>(getRoot(root, "global_cycles")))
  // Program parameters
{
  compute_derived();
}

void SolParameters::compute_derived() {

  // First clear all parameters so that the method is idempotent

  registers.clear();
  instructions.clear();
  cycles.clear();
  temporaries.clear();
  global_cycles.clear();

}



#ifdef GRAPHICS

QScriptValue SolParameters::getRoot(QScriptValue root, string p) {
  return root.property(p.c_str());
}

void SolParameters::get_element(QScriptValue root, int & i) {
  assert(root.isNumber());
  i = root.toInt32();
}

#else

Json::Value SolParameters::getRoot(Json::Value root, string p) {
  return root.get(p, "null");
}

void SolParameters::get_element(Json::Value root, int & i) {
  assert(root.isInt());
  i = root.asInt();
}

#endif
