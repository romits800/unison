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


#include "secmaxdivmodel.hpp"



SecMaxDivModel::SecMaxDivModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  SecDivModel(p_input, p_options, p_ipl)
  // input_solutions(p_sol_input)
{
  // TODO: Fix the upper bound
  maxdist = IntVar(*this, 1, 1000000);
}



SecMaxDivModel::SecMaxDivModel(SecMaxDivModel& cg) :
  SecDivModel(cg),
  input_solutions(cg.input_solutions)
{
  maxdist.update(*this, cg.maxdist);
}

SecMaxDivModel* SecMaxDivModel::copy(void) {
  return new SecMaxDivModel(*this);
}




void SecMaxDivModel::post_levenshtein(void)
{
  uint sizex = v_oc.size(); // size of maxc

  IntVarArgs bh;
  for (SecDivModel *s: input_solutions) {

    IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
    Matrix<IntVarArray> mat(x, sizex, sizex);

    mat(0,0) = var(0);
    for (uint i = 1; i < sizex; i++) {
      mat(i,0) = var(i);
      mat(0,i) = var(i);
    }
    for (uint i = 1; i < sizex; i++)
      for (uint j = 1; j < sizex; j++) {

        BoolVar res = var ( oc(i-1) != s->oc(j-1) );
        IntVarArgs v;
        v << var (mat(i-1,j) + 1);
        v << var (mat(i,j-1) + 1);
        v << var (mat(i-1,j-1) + res);
        min(*this, v, mat(i,j));
      }
    // Add the solutions for optimization
    bh << var( mat(sizex-1, sizex-1));
    // Constraint for every solution
    constraint(var( mat(sizex-1, sizex-1)) > 1);
  }
  if (bh.size() >0) {
    constraint(maxdist == var( sum(bh)));
  }
  else
    exit(EXIT_FAILURE);

}

void SecMaxDivModel::constrain_levenshtein(const SecMaxDivModel & b)
{
  uint sizex = v_oc.size(); // size of maxc

  IntVarArgs bh;
  for (SecMaxDivModel *s: input_solutions) {

    IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
    Matrix<IntVarArray> mat(x, sizex, sizex);

    mat(0,0) = var(0);
    for (uint i = 1; i < sizex; i++) {
      mat(i,0) = var(i);
      mat(0,i) = var(i);
    }
    for (uint i = 1; i < sizex; i++)
      for (uint j = 1; j < sizex; j++) {

        BoolVar res = var ( s->oc(i-1) != b.oc(j-1) );
        IntVarArgs v;
        v << var (mat(i-1,j) + 1);
        v << var (mat(i,j-1) + 1);
        v << var (mat(i-1,j-1) + res);
        min(*this, v, mat(i,j));
    }
    bh << var( mat(sizex-1, sizex-1));
  }

  constraint(maxdist > sum(bh)); // Levenshtein distance
}


void SecMaxDivModel::post_levenshtein_set(void)
{
  uint sizex = v_oc.size();// + 1; // size of maxc
  // int op_size = O().size();
  IntVarArgs bh;
  for (SecDivModel *s: input_solutions) {

    IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
    Matrix<IntVarArray> mat(x, sizex, sizex);
    uint maxcap = max_of(input->cap);

    IntVarArray cap = int_var_array(sizex-1, 0, maxcap);
    IntVarArray bcap = int_var_array(sizex-1, 0, maxcap);


    for (uint i = 0; i < sizex-1; i++) {
      cap[i] = var(cardinality(oc(i)));
      bcap[i] = var(cardinality(s->oc(i)));
    }

    mat(0,0) = var(0);
    for (uint i = 1; i < sizex; i++) {
      IntVar nw = cap[i-1]; //var(cardinality(oc(i-1)));
      IntVar old = bcap[i-1]; //var(cardinality(b.oc(i-1)));
      mat(i,0) = var( mat(i-1,0) +  nw);
      mat(0,i) = var( mat(0,i-1) + old);
    }

    for (uint i = 1; i < sizex; i++)
      for (uint j = 1; j < sizex; j++) {
        IntVarArgs cs;
        cs << var (cardinality (oc(i-1) - s->oc(j-1)));
        cs << var (cardinality (s->oc(j-1) - oc(i-1)));
        IntVar res = IntVar(*this, 0, maxcap);
        max(*this, cs, res);

        IntVarArgs v;
        v << var (mat(i-1,j) + cap[i-1]); //cardinality(oc(i-1)));
        v << var (mat(i,j-1) + bcap[i-1]); //cardinality(b.oc(j-1)));
        v << var (mat(i-1,j-1) + res);
        min(*this, v, mat(i,j));
      }

    constraint(var( mat(sizex-1, sizex-1)) > 1);

    bh << var( mat(sizex-1, sizex-1));
  }
  if (bh.size() >0)
      constraint(maxdist == var( sum(bh)));
  else
      exit(EXIT_FAILURE);

}

void SecMaxDivModel::constrain_levenshtein_set(const SecMaxDivModel & b)
{
  uint sizex = v_oc.size();// + 1; // size of maxc
  // int op_size = O().size();
  IntVarArgs bh;
  for (SecMaxDivModel *s: input_solutions) {

    IntVarArray x = int_var_array(sizex*sizex, 0, sizex);
    Matrix<IntVarArray> mat(x, sizex, sizex);
    uint maxcap = max_of(input->cap);

    IntVarArray cap = int_var_array(sizex-1, 0, maxcap);
    IntVarArray bcap = int_var_array(sizex-1, 0, maxcap);


    for (uint i = 0; i < sizex-1; i++) {
      cap[i] = var(cardinality(oc(i)));
      bcap[i] = var(cardinality(b.oc(i)));
    }

    mat(0,0) = var(0);
    for (uint i = 1; i < sizex; i++) {
      IntVar nw = cap[i-1]; //var(cardinality(oc(i-1)));
      IntVar old = bcap[i-1]; //var(cardinality(b.oc(i-1)));
      mat(i,0) = var( mat(i-1,0) +  nw);
      mat(0,i) = var( mat(0,i-1) + old);
    }

    for (uint i = 1; i < sizex; i++)
      for (uint j = 1; j < sizex; j++) {
        IntVarArgs cs;
        cs << var (cardinality (s->oc(i-1) - b.oc(j-1)));
        cs << var (cardinality (b.oc(j-1) - s->oc(i-1)));
        IntVar res = IntVar(*this, 0, maxcap);
        max(*this, cs, res);

        IntVarArgs v;
        v << var (mat(i-1,j) + cap[i-1]); //cardinality(oc(i-1)));
        v << var (mat(i,j-1) + bcap[i-1]); //cardinality(b.oc(j-1)));
        v << var (mat(i-1,j-1) + res);
        min(*this, v, mat(i,j));
      }

    bh << var( mat(sizex-1, sizex-1));
  }
  constraint( maxdist > sum(bh)); // Levenshtein distance
}




void SecMaxDivModel::post_input_solution_constrain() {

  BoolVarArgs bh;
  IntVarArgs ih;
  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (operation o: real_operations) {
          bh << var (hamm(o) != s->hamm(o));
          bhs << var (hamm(o) != s->hamm(o));
      }
      if (bhs.size() >0) {           //
          constraint(var( sum(bhs)) > 0);
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_DIFF_BR:
  case DIST_HAMMING_DIFF:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (int i = 0; i < v_diff.size(); i++) {
        bh << var (diff(i) != s->diff(i));
        bhs << var (diff(i) != s->diff(i));
      }
      if (bhs.size() >0) {           //
          constraint(var( sum(bhs)) > 0);
      }

    }
    if (bh.size() >0) {
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (operation o : branch_operations) {
        bh << var (hamm(o) != s->hamm(o));
        bhs << var (hamm(o) != s->hamm(o));
      }
      if (bhs.size() >0) {           //
          constraint(var( sum(bhs)) > 0);
      }
    }

    if (bh.size() >0) {
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_LEVENSHTEIN:
    post_levenshtein();
    break;

  case DIST_LEVENSHTEIN_SET:
    post_levenshtein_set();
    break;
  case DIST_REGHAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (operand p : input->P) {
          bh << var (reghamm(p) != s->reghamm(p));
          bh << var (x(p) != s->x(p));
          bhs << var (reghamm(p) != s->reghamm(p));
          bhs << var (x(p) != s->x(p));
      }
      if (bhs.size() >0) {           //
          constraint(var( sum(bhs)) > 0);
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_REG_CYC_HAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (operand p : input->P) {
          bh << var (reghamm(p) != s->reghamm(p));
          bh << var (x(p) != s->x(p));
          bhs << var (reghamm(p) != s->reghamm(p));
          bhs << var (x(p) != s->x(p));
      }

      for (operation o: real_operations) {
          bh << var (hamm(o) != s->hamm(o));
          bhs << var (hamm(o) != s->hamm(o));
      }

      if (bhs.size() >0) {           //
          constraint(var( sum(bhs)) > 0);
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;

    // TODO(Romy): Update    
  case DIST_CYC_REG_GADGET:
  case DIST_REG_GADGET:
  case DIST_CYC_GADGET:
    cerr << "Gadget distances not implemented yet. Exiting.." << endl;
    exit(EXIT_FAILURE);
    break;
  case DIST_DIFF_BR:
    for (SecMaxDivModel *s: input_solutions) {
      IntVarArgs ihs;
      for (int i = 0; i < v_diff.size(); i++) {
        ih << var (abs (diff(i) - s->diff(i)));
        ihs << var (abs (diff(i) - s->diff(i)));
      }
      if (ihs.size() >0) {           //
        constraint(var( sum(ihs)) > 0);
      }

    }
    if (ih.size() >0) {
      constraint(maxdist == var(sum(ih)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR_REG:
    for (SecMaxDivModel *s: input_solutions) {
      BoolVarArgs bhs;
      for (operation o : branch_operations) {
        bh << var (hamm(o) != s->hamm(o));
        bhs << var (hamm(o) != s->hamm(o));
        for (operand p: input->operands[o]) {
          //for (temporary t: input->temps[p]) {
          bh << var (reghamm(p) != s->reghamm(p));
          bh << var (x(p) != s->x(p));
          bhs << var (reghamm(p) != s->reghamm(p));
          bhs << var (x(p) != s->x(p));
          //}
        }
      }
      if (bhs.size() >0) {           //
        constraint(var( sum(bhs)) > 0);
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_COST:
    {IntVarArgs ihs;
    for (SecMaxDivModel *s: input_solutions) {
      for (uint i = 0; i< input->N; i++) {
        ihs << var(abs(cost()[i] - s->cost()[i]));
      }
    }
    if (ihs.size() > 0) {
        constraint(maxdist == var (sum(ihs)));
    } else {
      exit(EXIT_FAILURE);
    }
    }
    break;
  default:
    cerr << "Dinstace not implemented yet." << endl;
    exit(EXIT_FAILURE);
    break;

  } // Switch

  return;

}

void SecMaxDivModel::constrain(const Space & _b) {

  const SecMaxDivModel& b = static_cast<const SecMaxDivModel&>(_b);

  BoolVarArgs bh;
  IntVarArgs ih;

  switch (options->dist_metric()) {
  case DIST_HAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      for (operation o: input -> O) {
        if (is_real_type(o))
          bh << var (b.hamm(o) != s->hamm(o));
      }
    }
    if (bh.size() >0) {           //
      // dist = var( sum(bh));
      constraint(maxdist > sum(bh)); // hamming distance

    } else {
      cerr << "No constraints @ constrain" << endl;
      exit(EXIT_FAILURE);
    }

    break;
  case DIST_HAMMING_DIFF_BR:
  case DIST_HAMMING_DIFF:
    for (SecMaxDivModel *s: input_solutions) {
      for (int i = 0; i < v_diff.size(); i++)
        bh << var (b.diff(i) != s->diff(i));
    }
    if (bh.size() >0) {           //
      // dist = var( sum(bh));
      constraint(maxdist > sum(bh)); // hamming distance

    } else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR:
    for (SecMaxDivModel *s: input_solutions) {
      for (operation o : branch_operations) {
        bh << var (b.hamm(o) != s->hamm(o));
      }
    }
    if (bh.size() >0) {
      constraint(maxdist > sum(bh)); // hamming distance on the branches
    } else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_LEVENSHTEIN:
    constrain_levenshtein(b);
    break;

  case DIST_LEVENSHTEIN_SET:
    constrain_levenshtein_set(b);
    break;
  case DIST_REGHAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      for (operand p : input->P) {
          bh << var (b.reghamm(p) != s->reghamm(p));
          bh << var (b.x(p) != s->x(p));
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist > sum(bh)); // hamming distance

    } else {
      cerr << "No constraints @ constrain" << endl;
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_REG_CYC_HAMMING:
    for (SecMaxDivModel *s: input_solutions) {
      for (operand p : input->P) {
          bh << var (b.reghamm(p) != s->reghamm(p));
          bh << var (b.x(p) != s->x(p));
      }
      for (operation o: input -> O) {
        if (is_real_type(o))
          bh << var (b.hamm(o) != s->hamm(o));
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist > sum(bh)); // hamming distance

    } else {
      cerr << "No constraints @ constrain" << endl;
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_DIFF_BR:
    for (SecMaxDivModel *s: input_solutions) {
      for (int i = 0; i < v_diff.size(); i++)
        ih << var (abs (b.diff(i) - s->diff(i)));
    }
    if (ih.size() >0) {
      constraint(maxdist > sum(ih));
    } else {
      cerr << "No constraints @ constrain";
      exit(EXIT_FAILURE);
    }
    break;
  case DIST_HAMMING_BR_REG:
    for (SecMaxDivModel *s: input_solutions) {
      for (operation o : branch_operations) {
        bh << var (b.hamm(o) != s->hamm(o));
        for (operand p: input->operands[o]) {
          bh << var (b.reghamm(p) != s->reghamm(p));
          bh << var (b.x(p) != s->x(p));
        }
      }
    }
    if (bh.size() >0) {           //
      constraint(maxdist == var( sum(bh)));
    } else {
      exit(EXIT_FAILURE);
    }
    break;

  case DIST_COST:
    {IntVarArgs ihs;
    for (SecMaxDivModel *s: input_solutions) {
      for (uint i = 0; i< input->N; i++)
        if (b.cost()[i].assigned()) {
            ihs << var(abs(b.cost()[i] - s->cost()[i]));
        }
    }
    if (ihs.size() > 0) {
        constraint(maxdist == var (sum(ihs)));
    } else {
      exit(EXIT_FAILURE);
    }
    }
    break;
  default:
    cerr << "Gadget distances not implemented yet. Exiting.." << endl;
    exit(EXIT_FAILURE);
    break;

  } // switch

  return;

}



