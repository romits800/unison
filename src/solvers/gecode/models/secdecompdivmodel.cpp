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


#include "secdecompdivmodel.hpp"


SecDecompDivModel::SecDecompDivModel(Parameters * p_input, ModelOptions * p_options,
				     IntPropLevel p_ipl) :
  SecDivModel(p_input, p_options, p_ipl)
{
  // div_r.seed(p_options->seed());
  div_p = options->decomp_r();

  // int op_size = O().size();
  // int maxval = max_of(input->maxc);
  // // difference between operators
  // v_diff  = int_var_array((op_size*(op_size -1))/2, -maxval, maxval);
  // // Hamming distance between operators
  // v_hamm  = int_var_array(op_size, -1, maxval);

}

SecDecompDivModel::SecDecompDivModel(SecDecompDivModel& cg) :
  SecDivModel(cg)
  // div_p(cg.div_p),
  // div_r(cg.div_r)
{
  // v_diff.update(*this, cg.v_diff);
  // v_hamm.update(*this, cg.v_hamm);
}

SecDecompDivModel* SecDecompDivModel::copy(void) {
  return new SecDecompDivModel(*this);
}


// void SecDecompDivModel::post_div_decomp_solution_branchers(DivModel* solm) {

//   GlobalModel::post_solution_brancher();
  // std::cout << solm -> v_oa << std::endl;
  // std::cout << v_oa << std::endl;
    // IntArgs sol;
    // IntVarArgs vs;
    // for (int c: solver->cycles) sol << c;
    // for (int r: solver->registers) sol << r;
    // // for (int t: solver->temporaries) sol << t;
    // // for (int i: solver->instructions) sol << i;
    // for (IntVar c: v_c) vs << c;
    // for (IntVar r: v_r) vs << r;
    // solution_branch(*this, vs, sol);


  // for (IntVar p: v_pal) vs << p;
  // for (IntVar p: v_pals) vs << p;
  // // for (BoolVar p: v_oa) vs << p; // 
  // for (IntVar p: v_ali) vs << p;

  // for (int i=0; i < solm->v_pal.size(); i++) {
  //   if (solm->v_pal[i].assigned()) {
  // 	sol << solm->v_pal[i].val();
  // 	vs << v_pal[i];
  //   }
  // }
  // for (int i=0; i < solm->v_oa.size(); i++) {
  //   if (solm->v_oa[i].assigned()) {
  //     sol << solm->v_oa[i].val();
  //     vs << v_oa[i];
  //   }
  // }

  // solution_branch(*this, vs, sol);
// } 

void SecDecompDivModel::post_div_decomp_branchers() {
  Rnd r;
  r.seed(options->seed());

  BoolVarArgs xs;
  for (global_cluster gc : input->GC) {
    operand p = input->clusters[gc][0];
    xs << x(p);
  }

  if (cf) {
    branch(*this,
           xs,
           BOOL_VAR_MERIT_MAX(&Merit::cluster_energy),
           BOOL_VAL_MAX(),
           NULL,
           &print_cluster_connection_decision);
  } else {
    branch(*this,
           xs,
           BOOL_VAR_MERIT_MAX(&Merit::cluster_energy),
           BOOL_VAL(&most_effective_connection_decision),
           NULL,
           &print_cluster_disconnection_decision);
  }

  // register allocation
/*  branch(*this,
         v_pals,
         SET_VAR_MERIT_MAX(&Merit::energy),
         SET_VAL(&most_effective),
         &allocatable,
         &print_allocation_decision);
*/

  branch(*this,
         v_pals,
         SET_VAR_RND(r),
         SET_VAL_RND_INC(r),
         &allocatable,
         &print_allocation_decision);

  // activation

  BoolVarArgs as;
  for (activation_class ac : input->AC) {
    operation o = input->activation_class_representative[ac];
    as << a(o);
  }
  branch(*this, as, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
         NULL, &print_activation_decision);

  if (!options->disable_hints()) {

    // hinted register avoidance

    BoolVarArgs avoidhs;
    for (AvoidHint hint : input->avoidhints) {
      operand p = get<0>(hint);
      vector<register_atom> as = get<1>(hint);
      IntArgs ras(as);
      BoolVar h(*this, 0, 1);
      dom(*this, ry(p), IntSet(ras), h);
      avoidhs << h;
    }

    branch(*this, avoidhs, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
           NULL, &print_hinted_avoidance_decision);

    // hinted register assignment

    BoolVarArgs assignhs;
    for (vector<int> hint : input->assignhints) {
      operand p = hint[0];
      register_atom a = hint[1];
      BoolVar h(*this, 0, 1);
      assignhs << var(ry(p) == a);
    }

    branch(*this, assignhs, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
           NULL, &print_hinted_assignment_decision);

  }

  // register alignment

  branch(*this, v_oa, BOOL_VAR_RND(r), BOOL_VAL_RND(r),
         NULL, &print_alignment_decision);

  // slack assignment

  branch(*this, v_s, INT_VAR_RND(r), INT_VAL_RND(r),
         NULL, &print_slack_assignment_decision);

  // register assignment

  IntVarArgs prs;

  for (global_congruence g : input->G)
    prs << ry(input->representative[input->regular[g]]);

  branch(*this, prs, INT_VAR_RND(r), INT_VAL_RND(r),
         NULL, &print_assignment_decision);
}

void SecDecompDivModel::constrain(const Space & _b) {
  const SecDecompDivModel& b = static_cast<const SecDecompDivModel&>(_b);

  BoolVarArgs bh;
  for (int i = 0; i < v_oa.size(); i++) {
    if (b.v_oa[i].assigned())
      bh << var (b.v_oa[i] != v_oa[i]);
  }
  for (int i = 0; i < v_pal.size(); i++) {
    if (b.v_pal[i].assigned())
      bh << var (b.v_pal[i] != v_pal[i]);
  }

  for (int i = 0; i < v_pals.size(); i++) {
    if (b.v_pals[i].assigned())
      bh << var (b.v_pals[i] != v_pals[i]);
  }

  for (int i = 0; i < v_ali.size(); i++) {
    if (b.v_ali[i].assigned())
      bh << var (b.v_ali[i] != v_ali[i]);
  }

  if (bh.size() > 0)
    constraint(sum(bh) >= 1);


  // DivModel::constrain(_b);	// 

  // BoolVarArgs bh;

  // switch (options->dist_metric()) {
  // case DIST_HAMMING:
  //   for (operation o: input -> O) {
  //     bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)           //
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_DIFF:

  //   for (int i = 0; i < v_diff.size(); i++) {
  //     bh << var (diff(i) != b.diff(i));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_BR:
  //   for (operation o : input -> O) {
  //     if (input->type[o] == BRANCH)
  //       bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // }

  return;

}
/*
void copy_domain_v(Home h, SetVar s, SetVar d) {
  Set::ViewRanges<Set::SetView> sr(s);
  //Int::IntView sr(s);
  //Iter::Values::Array sv(sr,sr.size());
  Set::SetView dv(d);
  dv.narrow_r(h, sr);
}
*/

void SecDecompDivModel::relax_div_solution(SecDecompDivModel * d) {

 // block b = ls->b;

/*  for (int o = 0; o < v_c.size(); o++) {

    for (operand p: input->operands[o]) {
        copy_domain(*this, d->ry(p), ry(p));
    }
  } */ 
/*  for (int o = 0; o < v_oa.size(); o++) {
    copy_domain(*this, d->oa(o), oa(o));
  }
*/
/*
  for (int o = 0; o < v_pal.size(); o++) {
    copy_domain(*this, d->v_pal[o], v_pal[o]);
  }
*/
 /*for (operand p: input->P) {
    int t = input->temps[p][y(p).val()];
    if (t >= 0)
        copy_domain(*this, d->r(t), r(t));
 }

 */
  for (global_congruence g : input->G) {
    operand p = input->representative[input->regular[g]];
    int t = input->temps[p][y(p).val()];
    if (t >= 0){
        copy_domain(*this,  d->ry(p),  ry(p));
        copy_domain(*this, d->r(t), r(t));
    }
  }

}


void SecDecompDivModel::apply_div_solution(SecDivModel * d) {

  // for (int i = 0; i < v_pal.size(); i++) {
  //   if (d->v_pal[i].assigned())
  //     constraint(d->v_pal[i] == v_pal[i]);
  // }

  // for (int i = 0; i < v_pals.size(); i++) {
  //   if (d->v_pals[i].assigned())
  //     constraint(d->v_pals[i] == v_pals[i]);
  // }

  for (int i = 0; i < v_oa.size(); i++) {
    if (d->v_oa[i].assigned())
      constraint(d->v_oa[i] == v_oa[i]);
  }

  // for (int i = 0; i < v_ali.size(); i++) {
  //   if (d->v_ali[i].assigned())
  //     constraint(d->v_ali[i] == v_ali[i]);
  // }

  
  // for (temporary t1 : input->tmp[b]) {
  //   if (!ls->is_dead(t1)) {
  //     constraint(r(t1) == ls->r(t1));
  //   }
  // }
  
  // for (operation o : input->ops[b]) {
  //   constraint(i(o) == ls->i(o));
  // }
  
  // for (operation o : input->ops[b])
  //   if (!ls->is_inactive(o)) {
  //     constraint(c(o) == ls->c(o));
  //   }
  // for (operand p : input->ope[b]) {
  //   constraint(y(p) == ls->y(p));
  // }

}



void SecDecompDivModel::post_constrain(SecDecompDivModel* _b) {

  // const SecDecompDivModel& b = static_cast<const SecDecompDivModel&>(*_b);

  // BoolVarArgs bh;

  // switch (options->dist_metric()) {
  // case DIST_HAMMING:
  //   for (operation o: input -> O) {
  //     bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)           //
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_DIFF:

  //   for (int i = 0; i < v_diff.size(); i++) {
  //     bh << var (diff(i) != b.diff(i));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_DIFF_BR:

  //   for (int i = 0; i < v_diff.size(); i++) {
  //     bh << var (diff(i) != b.diff(i));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // case DIST_HAMMING_BR:
  //   for (operation o : input -> O) {
  //     if (is_branch_type(o))
  //       bh << var (hamm(o) != b.hamm(o));
  //   }
  //   if (bh.size() >0)
  //     constraint(sum(bh) >= 1); // hamming distance
  //   break;
  // }

  // return;

}

bool SecDecompDivModel::master(const MetaInfo& mi) {
  //  std::cout << "master: " << div_p << endl;
  if (mi.type() == MetaInfo::PORTFOLIO) {
    assert(mi.type() == MetaInfo::PORTFOLIO);
    return true; // default return value for portfolio master (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
      if (mi.last() != NULL)
      constrain(*mi.last());
    mi.nogoods().post(*this);
    return true; // forces a restart even if a solution has been found
  }
  GECODE_NEVER;
}



bool SecDecompDivModel::slave(const MetaInfo& mi) {
  //  std::cout << "slave: " << div_p << endl;
  if (mi.type() == MetaInfo::PORTFOLIO) {
    post_complete_branchers(mi.asset());
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if (div_p > 0.0) {
      if (mi.last() != NULL)// {
        next(static_cast<const SecDecompDivModel&>(*mi.last()));
      return false;
    } else if (mi.restart() == 0) {
      return true;
    } else {
      return true;
    }

  }
  GECODE_NEVER;
}

void SecDecompDivModel::next(const SecDecompDivModel& l) {

  // std::cout << "next: " << div_p << endl;
  BoolVarArgs toa, ltoa;
  for (int i = 0; i < v_oa.size(); i++) {
    if (l.v_oa[i].assigned()) {
      toa << v_oa[i];
      ltoa << l.v_oa[i];
    }
  }
  relax(*this, toa, ltoa, div_r, div_p);

  BoolVarArgs tpal, ltpal;

  for (int i = 0; i < v_pal.size(); i++) {
    if (l.v_pal[i].assigned()) {
      tpal << v_pal[i];
      ltpal << l.v_pal[i];
    }
  }

  relax(*this, tpal, ltpal, div_r, div_p);

  SetVarArgs tali, ltali;
  for (int i = 0; i < v_ali.size(); i++) {
    if (l.v_ali[i].assigned()) {
      tali << v_ali[i];
      ltali << l.v_ali[i];
    }
  }

  relax(*this, tali, ltali, div_r, div_p);


  SetVarArgs tpals, ltpals;
  for (int i = 0; i < v_pals.size(); i++) {
    if (l.v_pals[i].assigned()) {
      tpals << v_pals[i];
      ltpals << l.v_pals[i];
    }
  }

  relax(*this, tpals, ltpals, div_r, div_p);


  IntVarArgs tc, ltc;

  for (int o = 0; o < v_c.size(); o++) {
    if (l.c(o).assigned()) {
      tc << c(o);
      ltc << l.c(o);
    }
  }
  relax(*this, tc, ltc, div_r, div_p);

  IntVarArgs tr, ltr;

  for (int i = 0; i < v_r.size(); i++) {
    if (l.r(i).assigned()) {
      tr << r(i);
      ltr << l.r(i);
    }
  }
  relax(*this, tr, ltr, div_r, div_p);


  BoolVarArgs ta, lta;

  for (int i = 0; i < v_a.size(); i++) {
    if (l.a(i).assigned()) {
      ta << a(i);
      lta << l.a(i);
    }
  }
  relax(*this, ta, lta, div_r, div_p);

  IntVarArgs prs, lprs;

  for (global_congruence g : input->G) {
    prs << ry(input->representative[input->regular[g]]);
    lprs << l.ry(input->representative[input->regular[g]]);
  }

  relax(*this, prs, lprs, div_r, div_p);

  // relax(*this, v_ali, l.v_ali, div_r, div_p);
  // relax(*this, v_pals, l.v_pals, div_r, div_p);

  //     v_pal.update(*this, cg.v_pal);
  // v_pals.update(*this, cg.v_pals);
  // v_oa.update(*this, cg.v_oa);
  // v_ali.update(*this, cg.v_ali);

}

