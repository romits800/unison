/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  Contributing authors:
 *    Gabriel Hjort Blindell <ghb@kth.se>
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


#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <cmath>

#ifdef GRAPHICS
#include <QtGui>
#include <QtScript/QScriptEngine>
#endif

#include <gecode/search.hh>
#include <gecode/driver.hh>
#include <gecode/kernel.hh>

#ifdef GRAPHICS
#include <gecode/gist.hh>
#endif

#include "common/definitions.hpp"
#include "models/parameters.hpp"
#include "models/solver-parameters.hpp"
#include "models/options.hpp"
#include "models/simplemodel.hpp"
#include "models/globalmodel.hpp"
#include "models/divmodel.hpp"
#include "models/decompdivmodel.hpp"
#include "models/localdivmodel.hpp"
#include "models/localmodel.hpp"
#include "procedures/divprocedures.hpp"
#include "procedures/localdivprocedures.hpp"

#ifndef GRAPHICS
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
#endif

#ifdef GRAPHICS
#include "inspectors/registerarrayinspector.hpp"
#include "inspectors/issuecycleinspector.hpp"
#include "inspectors/liverangeinspector.hpp"
#include "inspectors/assignmentinspector.hpp"
#include "inspectors/allocationinspector.hpp"
#include "inspectors/livedurationinspector.hpp"
#include "inspectors/selectioninspector.hpp"
#include "inspectors/operandassignmentinspector.hpp"
#include "inspectors/resourceconsumptioninspector.hpp"
#include "inspectors/dataflowinspector.hpp"
#include "inspectors/alignmentinspector.hpp"
#include "inspectors/alignmentpartitioninspector.hpp"
#include "inspectors/operandallocationinspector.hpp"
#include "inspectors/congruenceallocationinspector.hpp"
#include "inspectors/precedenceinspector.hpp"
#include "inspectors/precedencematrixinspector.hpp"
#include "inspectors/usersinspector.hpp"
#include "inspectors/operandlatencyinspector.hpp"
#endif

// #include <exception.hpp>
using namespace Gecode;
using namespace std;



class ResultData {

public:

  GlobalModel * solution;
  bool proven;
  long long int fail;
  long long int it_fail;
  long long int node;
  long long int it_node;
  int presolver_time;
  int presolving_time;
  int solving_time;
  int it_solving_time;

  ResultData(GlobalModel * solution, bool proven, long long int it_fail,
             long long int it_node, int presolver_time, int presolving_time,
             int solving_time, int it_solving_time) {
    this->solution = solution;
    this->proven = proven;
    fail = -1;
    this->it_fail = it_fail;
    node = -1;
    this->it_node = it_node;
    this->presolver_time = presolver_time;
    this->presolving_time = presolving_time;
    this->solving_time = solving_time;
    this->it_solving_time = it_solving_time;
  }
};

class GlobalData {

public:

  int global_n_int_vars;
  int global_n_bool_vars;
  int global_n_set_vars;

  GlobalData(int global_n_int_vars0,
             int global_n_bool_vars0,
             int global_n_set_vars0) :
    global_n_int_vars(global_n_int_vars0),
    global_n_bool_vars(global_n_bool_vars0),
    global_n_set_vars(global_n_set_vars0) {}

};

string produce_json(const ResultData& rd,
                    const GlobalData& gd,
                    unsigned int N,
                    unsigned int it_num)
{
    string json;
    if (rd.solution) json = rd.solution->solution_to_json();
    else             json = "{}";
    stringstream ss;
    if (rd.solution) ss << ", ";
    ss << "\"solver\": \"gecode-diversifier\"";
    ss << ", \"has_solution\": " << (rd.solution ? "true" : "false");
    ss << ", \"proven\": " << (rd.proven ? "true" : "false");
    vector<int> ones;
    init_vector(ones, N, -1);

    // Added types to json output
    vector<temporary> ts; //types
    for (operand o : rd.solution->input->O)
      ts.push_back(rd.solution->input->type[o]);

    ss << ", \"type\":" << to_json(ts);
    //

    ss << ", \"cost\": "
       << (rd.solution ? show(var_vector(rd.solution->cost())) : show(ones));
    if (rd.fail >= 0) {
      ss << ", \"failures\": " << rd.fail;
    }
    if (rd.it_fail >= 0) {
      ss << ", \"it_failures\": " << rd.it_fail;
    }
    if (rd.node >= 0) {
      ss << ", \"nodes\": " << rd.node;
    }
    if (rd.it_node >= 0) {
      ss << ", \"it_nodes\": " << rd.it_node;
    }
    if (it_num == 0) {
      if (rd.presolver_time >= 0) {
        ss << ", \"presolver_time\": " << rd.presolver_time;
      }
      ss << ", \"gecode_presolving_time\": " << rd.presolving_time;
    }
    if (rd.solving_time >= 0) {
      ss << ", \"solver_time\": " << rd.solving_time;
    }
    if (rd.it_solving_time >= 0) {
      ss << ", \"it_solving_time\": " << rd.it_solving_time;
    }
    ss << ", \"global_int_variables\": " << gd.global_n_int_vars;
    ss << ", \"global_bool_variables\": " << gd.global_n_bool_vars;
    ss << ", \"global_set_variables\": " << gd.global_n_set_vars;
    string more_json = ss.str();
    json.insert(json.find_last_of("}"), more_json);
    return json;
}


class LocalJob : public Support::Job<LocalDivModel * > {
protected:

  LocalDivModel * l;
  RBS<LocalDivModel,BAB> * e;

public:
  LocalJob(LocalDivModel *l0,
           RBS<LocalDivModel,BAB> * e0):
    l(l0), e(e0){}
  virtual LocalDivModel * run(int) {
    // return 3;
    LocalDivModel * nextl = e->next();

    return nextl;

  }

};

class LocalJobs {
protected:
  map<block, LocalDivModel*> l;
  map<block, RBS<LocalDivModel,BAB> *> e;
  vector<block> blocks;
  unsigned int k;
public:
  LocalJobs(map<block, LocalDivModel*> l0,
            map<block, RBS<LocalDivModel,BAB> *> e0,
            vector<block> blocks0) :
    l(l0), e(e0),
    blocks(blocks0), k(0) {}
  bool operator ()(void) const {
    return k < blocks.size();
  }
  LocalJob * job(void) {
    // FIXME: fork jobs in the order of blocks[b], use blocks[k] instead of k
    block b = k;
    // Base local space to accumulate bounds while the portfolio is applied
    // Solution<LocalDivModel> ls = local_problem(gs.solution, b);
    LocalDivModel * lb = l[b];
    RBS<LocalDivModel,BAB> * eb = e[b];
    k++;
    return new LocalJob(lb, eb); //, local_solutions);
  }
};

string cost_status_report(DivModel * base, const DivModel * sol) {
  vector<double> imps, ogs;
  for (unsigned int n = 0; n < base->input->N; n++) {
    int cost_ub  = base->input->maxf[n] + (n == (base->input->N - 1) ? 1 : 0),
        max_cost = sol->cost()[n].max();
    double imp = ((((double)(cost_ub - max_cost)) / (double)max_cost) * 100.0),
      og  = optimality_gap(base, sol, n);
    imps.push_back(imp);
    ogs.push_back(og);
  }
  stringstream ss;
  ss << "cost: " << sol->cost();
  if (sol != base) {
    ss << ", improvement: ";
    vector<string> percents;
    for (double imp : imps) {
      stringstream ss0;
      ss0 << fixed << setprecision(2);
      ss0 << imp << "%";
      percents.push_back(ss0.str());
    }
    ss << show(percents, ", ", "", "{}");
  }
  if (std::any_of(ogs.begin(), ogs.end(), [](double og){return og > 0.0;})) {
    ss << ", optimality gap: ";
    vector<string> percents;
    for (double og : ogs) {
      stringstream ss0;
      ss0 << fixed << setprecision(2);
      ss0 << og << "%";
      percents.push_back(ss0.str());
    }
    ss << show(percents, ", ", "", "{}");
  }
  return ss.str();
}


bool has_solution(vector<ResultData> & results) {
  for (ResultData rd : results) {
    if (rd.solution) {
      return true;
    }
  }
  return false;
}

int main(int argc, char* argv[]) {

  int argc0 = argc;
  vector<string> argv0;
  for (int i = 0; i < argc; i++) argv0.push_back(argv[i]);

  ModelOptions options;
  // options for LNS
  if (options.div_method() == DIV_MONOLITHIC_LNS || options.div_method() == DIV_DECOMPOSITION_LNS) {
    options.iterations(10);
    options.relax(0.7);
    options.seed(3);
    // options.time(10*1000);
  }
  options.parse(argc, argv);

  if (argc < 2) {
    options.help();
    cout << endl;
    exit(EXIT_FAILURE);
  }

  if (strlen(options.instance()) == 0) {
    cerr << "Null input file, original program arguments:" << endl;
    for (int i = 0; i < argc0; i++) {
      cerr << "arg" << i << ": " << argv0[i] << endl;
    }
    cerr << "Remaining program arguments:" << endl;
    for (int i = 0; i < argc; i++) {
      cerr << "arg" << i << ": " << argv[i] << endl;
    }
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QApplication *app = new QApplication(argc, argv, false);
#endif

  string name(options.instance());
  string prefix = name.substr(0,name.find(".json"))
                      .substr(0,name.find(".ext"));
  ifstream fin;
  fin.open(name.c_str(), ios::in);
  if (fin.fail()) {
    cerr << "Failed to open " << name << ": " << strerror(errno) << endl;
#ifdef GRAPHICS
    cerr << "Working directory: "
         << QDir::currentPath().toStdString() << endl;
#endif
    exit(EXIT_FAILURE);
  }
  string json_input ((std::istreambuf_iterator<char>(fin)),
                     (std::istreambuf_iterator<char>()));
  fin.close();
  if (fin.fail()) {
    cerr << "Failed to close " << name << ": " << strerror(errno) << endl;
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QScriptValue root;
  QScriptEngine engine;
  root = engine.evaluate("(" + QString::fromStdString(json_input) + ")");
  if (engine.hasUncaughtException()) {
    QScriptValue val = engine.uncaughtException();
    if (val.isError()) {
      cerr << "Failed to parse " << name << ": "
           << val.toString().toStdString() << " at line "
           << engine.uncaughtExceptionLineNumber() << endl
           << "Backtrace: "
           << engine.uncaughtExceptionBacktrace().join("\n").toStdString()
           << endl;
    }
    exit(EXIT_FAILURE);
  }
  app->exit();
  delete app;
#else
  Json::Value root;
  Json::CharReaderBuilder reader;
  std::stringstream json_input_stream;
  json_input_stream << json_input;
  std::string errs;
  if (!Json::parseFromStream(reader, json_input_stream, &root, &errs)) {
    cerr << "Failed to parse " << name << endl << errs;
    exit(EXIT_FAILURE);
  }
#endif

  Parameters input(root);


  GIST_OPTIONS * go = new GIST_OPTIONS(),
               * lo  = new GIST_OPTIONS();

#ifdef GRAPHICS

  // Options for global Gist visualization

  GlobalRegisterArrayInspector * grai = new GlobalRegisterArrayInspector();
  go->inspect.click(grai);
  GlobalIssueCycleInspector * gici = new GlobalIssueCycleInspector();
  go->inspect.click(gici);
  GlobalLiveRangeInspector * glri = new GlobalLiveRangeInspector();
  go->inspect.click(glri);
  GlobalAssignmentInspector * gassi = new GlobalAssignmentInspector();
  go->inspect.click(gassi);
  GlobalAllocationInspector * galli = new GlobalAllocationInspector();
  go->inspect.click(galli);
  Gist::Print<DivModel> * gprp =
    new Gist::Print<DivModel>("Problem variables");
  go->inspect.click(gprp);
  GlobalSelectionInspector * gsi = new GlobalSelectionInspector();
  go->inspect.click(gsi);
  GlobalLiveDurationInspector * gldi = new GlobalLiveDurationInspector();
  go->inspect.click(gldi);
  GlobalOperandAssignmentInspector * goassi =
    new GlobalOperandAssignmentInspector();
  go->inspect.click(goassi);
  GlobalResourceConsumptionInspector * grci =
    new GlobalResourceConsumptionInspector();
  go->inspect.click(grci);
  GlobalDataFlowInspector * gdfi = new GlobalDataFlowInspector();
  go->inspect.click(gdfi);
  GlobalAlignmentInspector * goai = new GlobalAlignmentInspector();
  go->inspect.click(goai);
  GlobalAlignmentPartitionInspector * goapi =
    new GlobalAlignmentPartitionInspector();
  go->inspect.click(goapi);
  GlobalOperandAllocationInspector * goali =
    new GlobalOperandAllocationInspector();
  go->inspect.click(goali);
  GlobalCongruenceAllocationInspector * gcai =
    new GlobalCongruenceAllocationInspector();
  go->inspect.click(gcai);
  GlobalUsersInspector * gui = new GlobalUsersInspector();
  go->inspect.click(gui);
  GlobalOperandLatencyInspector * goli = new GlobalOperandLatencyInspector();
  go->inspect.click(goli);
  Gist::VarComparator<DivModel> *gprc =
    new Gist::VarComparator<DivModel>("Compare problem and secondary variables");
  go->inspect.compare(gprc);

  // Options for local Gist visualization

  LocalRegisterArrayInspector * lrai = new LocalRegisterArrayInspector();
  lo->inspect.click(lrai);
  LocalIssueCycleInspector * lici = new LocalIssueCycleInspector();
  lo->inspect.click(lici);
  LocalLiveRangeInspector * llri = new LocalLiveRangeInspector();
  lo->inspect.click(llri);
  LocalAssignmentInspector * lassi = new LocalAssignmentInspector();
  lo->inspect.click(lassi);
  LocalAllocationInspector * lalloi = new LocalAllocationInspector();
  lo->inspect.click(lalloi);
  Gist::Print<LocalModel> * lprp =
    new Gist::Print<LocalModel>("Problem variables");
  lo->inspect.click(lprp);
  Gist::VarComparator<LocalModel> * lprc =
    new Gist::VarComparator<LocalModel>("Compare problem and secondary variables");
  lo->inspect.compare(lprc);
  LocalSelectionInspector * lsi = new LocalSelectionInspector();
  lo->inspect.click(lsi);
  LocalLiveDurationInspector * lldi = new LocalLiveDurationInspector();
  lo->inspect.click(lldi);
  LocalOperandAssignmentInspector * loassi =
    new LocalOperandAssignmentInspector();
  lo->inspect.click(loassi);
  LocalResourceConsumptionInspector * lrci =
    new LocalResourceConsumptionInspector();
  lo->inspect.click(lrci);
  LocalPrecedenceInspector * lpi;
  lpi = new LocalPrecedenceInspector();
  LocalPrecedenceMatrixInspector * lpmi;
  lpmi = new LocalPrecedenceMatrixInspector();
  if (!options.disable_precedence_variables()) {
    lo->inspect.click(lpi);
    lo->inspect.click(lpmi);
  }
  LocalUsersInspector * lui = new LocalUsersInspector();
  lo->inspect.click(lui);
  LocalOperandLatencyInspector * loli = new LocalOperandLatencyInspector();
  lo->inspect.click(loli);
  LocalDataFlowInspector * ldfi = new LocalDataFlowInspector();
  lo->inspect.click(ldfi);

#endif



  // vector<ResultData> results;
  vector<vector<LocalDivModel *> > local_solutions;
  for (unsigned int b = 0; b < input.B.size(); b++)
    local_solutions.push_back(vector<LocalDivModel *>());


  bool proven = false;

  IterationState state(options.initial_aggressiveness(), false);

  Support::Timer t;
  t.start();


  // Code for diversification
  DivModel *d = new DivModel(&input, &options, IPL_DOM);

  GlobalData gd(d->n_int_vars, d->n_bool_vars, d->n_set_vars);


  int count = 0;
  int maxcount = options.number_divs();

  Support::Timer t_it;
  Support::Timer t_solver;


  vector<int> ag_best_cost;
  vector<int> best_cost;
  int bestcost;

  if (options.verbose()) {
    cerr << div() << "Printing cost status report..." << endl;
    cerr << div() << cost_status_report(d, d) << endl;
  }

  if (options.use_optimal_for_diversification()) {

#ifdef GRAPHICS
    QApplication *app = new QApplication(argc, argv, false);
#endif

    string name(options.solver_file());
    string prefix = name.substr(0,name.find(".json")).substr(0,name.find(".out"));
    ifstream fin;
    fin.open(name.c_str(), ios::in);
    if (fin.fail()) {
      cerr << "Failed to open " << name << ": " << strerror(errno) << endl;
      exit(EXIT_FAILURE);
    }
    string json_input ((std::istreambuf_iterator<char>(fin)),
                       (std::istreambuf_iterator<char>()));
    fin.close();
    if (fin.fail()) {
      cerr << "Failed to close " << name << ": " << strerror(errno) << endl;
      exit(EXIT_FAILURE);
    }
#ifdef GRAPHICS
    QScriptValue root;
    QScriptEngine engine;
    root = engine.evaluate("(" + QString::fromStdString(json_input) + ")");
    if (engine.hasUncaughtException()) {
      QScriptValue val = engine.uncaughtException();
      if (val.isError()) {
        cerr << "Failed to parse " << name << ": "
             << val.toString().toStdString() << " at line "
             << engine.uncaughtExceptionLineNumber() << endl
             << "Backtrace: "
             << engine.uncaughtExceptionBacktrace().join("\n").toStdString()
             << endl;
      }
      exit(EXIT_FAILURE);
    }
    app->exit();
    delete app;
#else

    Json::Value root;
    Json::CharReaderBuilder reader;
    std::stringstream json_input_stream;
    json_input_stream << json_input;
    std::string errs;
    if (!Json::parseFromStream(reader, json_input_stream, &root, &errs)) {
      cerr << "Failed to parse " << name << endl << errs;
      exit(EXIT_FAILURE);
    }
#endif
    SolverParameters solver(root);


    bestcost = solver.cost[0];
    if (bestcost < 0) {
      cerr << div() << "Falling back to llvm best solution." << endl;


      // Best cost upper bound
      bestcost = input.maxf[0];
      ag_best_cost.push_back(round((bestcost*(100. + (double)d->options->acceptable_gap()))/100.0));
      for (uint i = 1; i < input.N; i++) {
        ag_best_cost.push_back(input.maxf[i]);
      }

      // Best cost lower bound
      for (uint i = 0; i < input.N; i++) {
        best_cost.push_back(d->cost()[i].min()); //input.maxf[i]);
      }

    } else {
      cout << div() << "Using optimal cost " << bestcost << endl;
      // Best cost upper bound
      ag_best_cost.push_back(round((bestcost*(100. + (double)d->options->acceptable_gap()))/100.0));
      for (uint i = 1; i < input.N; i++) {
        int bcost = solver.cost[i];
        ag_best_cost.push_back(round((bcost*(100. + (double)d->options->acceptable_gap()))/100.0));

      }
      // Best cost lower bound
      if (solver.proven) {
        best_cost.push_back(bestcost);
        for (uint i = 1; i < input.N; i++) {
          best_cost.push_back(solver.cost[i]);
        }
      } else {
        for (uint i = 0; i < input.N; i++)
          best_cost.push_back(d->cost()[i].min());
      }
    }


    // }

  } else {

    cerr << div() << "Using llvm best solution" << endl;

    // Best cost upper bound
    bestcost = input.maxf[0];
    ag_best_cost.push_back(round((bestcost*(100. + (double)d->options->acceptable_gap()))/100.0));
    for (uint i = 1; i < input.N; i++) {
      ag_best_cost.push_back(input.maxf[i]);
    }
    // Best cost lower bound
    for (uint i = 0; i < input.N; i++) {
      best_cost.push_back(d->cost()[i].min());
    }
  }

  d -> post_lower_bound(best_cost);
  d -> post_upper_bound(ag_best_cost);

  if (d->status() == SS_FAILED) {
    cerr << div() << "No better solution!" << endl;
    cerr << div() << "ag_best_cost[0]" << ag_best_cost[0] << endl;
    return -1;
  }

  if (options.verbose()) {
    cerr << div() << "Printing cost status report..." << endl;
    cerr << div() << cost_status_report(d, d) << endl;
  }

  //////////////////////////////// START //////////////////////////////
  cerr << div() << "Starting..." << endl;


  if (options.div_method() == DIV_DECOMPOSITION_LNS) {

    DecompDivModel *dd = new DecompDivModel(&input, &options, IPL_DOM);

    GlobalData dgd(dd->n_int_vars, dd->n_bool_vars, dd->n_set_vars);

    dd -> post_upper_bound(ag_best_cost);
    dd -> post_lower_bound(best_cost);

    if (dd->status() == SS_FAILED) {
      cerr << div() << "No better solution!" << endl;
      return -1;
    }

    vector<block> blocks(dd->input->B);
    map<block, LocalDivModel *> local_problems;
    map<block, RBS<LocalDivModel,BAB> *> local_engines;
    for (block b: blocks) {

      Search::Options localOptions;

      Gecode::RestartMode restart = dd->options->restart();
      Search::Cutoff* c;
      unsigned long int s_const = dd->options->restart_base();

      if (restart == RM_LUBY ){
        c = Search::Cutoff::luby(s_const);
      } else if (restart == RM_CONSTANT) {
        c = Search::Cutoff::constant(s_const);
      } else {
        c = Search::Cutoff::constant(1000);
      }

      localOptions.cutoff = c;
      local_problems[b] = (LocalDivModel *) make_div_local(dd, b);
      local_problems[b]-> post_div_branchers();
      local_problems[b]-> post_diversification_constraints();
      local_problems[b]-> constrain_cost(IRT_LE, ceil((float)ag_best_cost[0]/(float)dd->input->freq[b]));

      // Restart-based meta-engine
      local_engines[b] = new  RBS<LocalDivModel,BAB>(local_problems[b], localOptions);
      // local_engines.push_back(e);
    }



    DecompDivModel * g = (DecompDivModel*) dd -> clone();

    g -> post_branchers();

    g -> post_diversification_constraints(); // Diversification constraints

    DecompDivModel * g1 = NULL;

    Search::Options globalOptions;

    Gecode::RestartMode restart = options.restart();
    Search::Cutoff* c;
    unsigned long int s_const = options.restart_base();

    if (restart == RM_LUBY ){
      c = Search::Cutoff::luby(s_const);
    } else if (restart == RM_CONSTANT) {
      c = Search::Cutoff::constant(s_const);
    } else {
      c = Search::Cutoff::constant(1000);
    }

    globalOptions.cutoff = c;

    RBS<DecompDivModel,DFS> e(g, globalOptions);

    vector<DecompDivModel*> solutions;
    while(count < maxcount) {


      g1 = e.next();

      if (g1 == NULL || g1->status() == SS_FAILED) {
        cerr << div() << "DecompDivModel e.next() failed." << endl;
        return 0;
      }


      DecompDivModel * g2 = g;
      try {
        g = (DecompDivModel *)g1->clone();
      }
      catch(Gecode::Exception e) {
        std::cerr  << div()
                   << "Exception in \"Gecode::" << e.what()
                   << "." << std::endl
                   << "Stopping..." << std::endl;
        return 0;
      }
      delete g2;

      bool found_local_solution = true;
      //Here
      LocalJobs ljs(local_problems, local_engines, blocks);
      unsigned int threads = options.total_threads();
      Support::RunJobs<LocalJobs, LocalDivModel *> js(ljs, threads);

      LocalDivModel *ls;
      while(js.run(ls)) {
        int i;
        LocalDivModel *fls;
        if (js.stopped(i,fls)) {
          block b = fls->b;
          local_problems[b] = fls;
          found_local_solution = false;
          break;
        } else {
          block b = ls->b;
          local_problems[b] = ls;
          if (ls && ls->status() != SS_FAILED) {
            g1->apply_solution(ls);
          }
        }

      }

      if (!found_local_solution) {
        cerr << div() << "Cannot find more solutions." << endl;
        return 0;
      }

      if (g1->status() != SS_FAILED) {
        cerr << div() << "Clone" << endl;
        solutions.push_back(g1);
        ResultData rd = ResultData(g1, false, 0,
                                   0, 0,
                                   0, t_solver.stop(),
                                   t_it.stop());

        ofstream fout;
        fout.open(to_string(count) + "." + g1->options->output_file());
        fout << produce_json(rd, dgd, g1->input->N, 0);
        fout.close();
        count++;
        g->post_constrain(g1);
      }
    }

  } // decompose

  else if (options.div_method() == DIV_MONOLITHIC_DFS) {

    d->post_complete_branchers(0);
    d->post_diversification_constraints(); // Diversification constraint

    BAB<DivModel> e(d);

    t_solver.start();
    t_it.start();


    while (DivModel *nextg = e.next()) {
      cout << div() << "Clone" << endl;

      // if (t.stop() > options.timeout())
      //   timeout_exit(base, results, gd, go, t.stop());

      if (count >= maxcount) break;

      ResultData rd = ResultData(nextg,
                                 proven, // false, /*proven*/
                                 0,
                                 count,
                                 0, //presolver_time,
                                 0, //presolving_time,
                                 t_solver.stop(),
                                 t_it.stop());

      ofstream fout;
      fout.open(to_string(count) + "." + d->options->output_file());
      fout << produce_json(rd, gd, nextg->input->N, 0);
      fout.close();


      DivModel *tmpg = d;
      d = nextg;
      delete tmpg;

      count++;
      t_it.start();

    }
  }
  else if (options.div_method() == DIV_MONOLITHIC_LNS) {

    d->post_complete_branchers(0);
    d->post_diversification_constraints(); // Diversification constraint

  // } else {

    Gecode::RestartMode restart = options.restart();
    Search::Cutoff* c;
    Search::Options o;
    unsigned long int s_const = options.restart_base();

    if (restart == RM_LUBY ){
      c = Search::Cutoff::luby(s_const);
    } else if (restart == RM_CONSTANT) {
      c = Search::Cutoff::constant(s_const);
    } else {
      c = Search::Cutoff::constant(1000);
    }

    o.cutoff = c;
    RBS<DivModel,BAB> e(d, o);

    t_solver.start();
    t_it.start();

    cerr << div() << "Starting" << endl;

    while (DivModel *nextg = e.next()) {
      cerr << div() << "Cloning" << endl;

      if (count >= maxcount) break;

      ResultData rd = ResultData(nextg,
                                 proven, // false, /*proven*/
                                 0,
                                 count,
                                 0, //presolver_time,
                                 0, //presolving_time,
                                 t_solver.stop(),
                                 t_it.stop());
      ofstream fout;
      fout.open(to_string(count) + "." + d->options->output_file());
      fout << produce_json(rd, gd, nextg->input->N, 0);
      fout.close();

      DivModel *tmpg = d;
      d = nextg;
      delete tmpg;

      count++;
      t_it.start();

    }

    // execution_time = t.stop();
  }
  if (d!=NULL) delete d;
  // }
}
