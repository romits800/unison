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
#include "models/localmodel.hpp"
#include "procedures/globalprocedures.hpp"
#include "procedures/localprocedures.hpp"

// #ifndef GRAPHICS
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
// #endif

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

using namespace Gecode;
using namespace std;

class LocalJob : public Support::Job<Solution<LocalModel> > {
protected:
  // Base local space to accumulate bounds while the portfolio is applied
  Solution<LocalModel> ls;
  // visualization options (if any)
  GIST_OPTIONS * lo;
  // current iteration
  int iteration;
  // local solutions in earlier iterations
  vector<vector<LocalModel *> > * local_solutions;
public:
  LocalJob(Solution<LocalModel> ls0, GIST_OPTIONS * lo0, int iteration0,
           vector<vector<LocalModel *> > * local_solutions0) :
    ls(ls0), lo(lo0), iteration(iteration0),
    local_solutions(local_solutions0) {}
  virtual Solution<LocalModel> run(int) {
    block b = ls.solution->b;
    if (ls.result != UNSATISFIABLE) {
      LocalModel * base_local = ls.solution;
      Gecode::SpaceStatus lss = base_local->status();
      assert(lss != SS_FAILED);
      bool single_block = base_local->input->B.size() == 1;
      ls = solved(base_local, (*local_solutions)[b]) && !single_block ?
        // if the local problem is already solved, fetch the cached solution
        fetch_solution(base_local, (*local_solutions)[b]) :
        // otherwise solve
        solve_local_portfolio(base_local, lo, iteration);
      delete base_local;
    }
    if (ls.solution->options->verbose()) {
      if (ls.result == LIMIT) {
        cerr << local(b) << "could not find solution" << endl;
      } else if (ls.result == UNSATISFIABLE) {
        cerr << local(b) << "could not find solution (unsatisfiable)" << endl;
      } else if (ls.result == CACHED_SOLUTION) {
        cerr << local(b) << "repeated solution" << endl;
      }
    }
    if (ls.result == LIMIT || ls.result == UNSATISFIABLE) {
      throw Support::JobStop<Solution<LocalModel> >(ls);
    }
    return ls;
  }
};

class LocalJobs {
protected:
  // global solution from which the local problems are generated
  Solution<GlobalModel> gs;
  // visualization options (if any)
  GIST_OPTIONS * lo;
  // current iteration
  int iteration;
  // local solutions in earlier iterations
  vector<vector<LocalModel *> > * local_solutions;
  // blocks sorted in descending priority
  vector<block> blocks;
  // current block index
  unsigned int k;
public:
  LocalJobs(Solution<GlobalModel> gs0, GIST_OPTIONS * lo0, int iteration0,
            vector<vector<LocalModel *> > * local_solutions0,
            vector<block> blocks0) :
    gs(gs0), lo(lo0), iteration(iteration0), local_solutions(local_solutions0),
    blocks(blocks0), k(0) {}
  bool operator ()(void) const {
    return k < blocks.size();
  }
  LocalJob * job(void) {
    // FIXME: fork jobs in the order of blocks[b], use blocks[k] instead of k
    block b = k;
    // Base local space to accumulate bounds while the portfolio is applied
    Solution<LocalModel> ls = local_problem(gs.solution, b);
    k++;
    return new LocalJob(ls, lo, iteration, local_solutions);
  }
};

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

// void emit_local(LocalModel * local, unsigned long int iteration, string prefix) {
//   block b = local->b;
//   ofstream fout;
//   fout.open(
//     (prefix + ".i" + to_string(iteration) + ".b" + to_string(b) + ".json")
//     .c_str());
//   fout << "{" << endl;
//   Parameters binput = local->input->make_local(b);
//   fout << binput.emit_json();
//   fout << init(init(local->emit_json()));
//   fout << endl << "}" << endl;
//   fout.close();
// }

string unsat_report(const GlobalModel * base) {
  stringstream ss;
  ss << "proven absence of solutions with cost less or equal than "
     << show(base->input->maxf, ", ", "", "{}");
  return ss.str();
}

string cost_status_report(GlobalModel * base, const GlobalModel * sol) {
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

void emit_output_exit(GlobalModel * base, const vector<ResultData> & results,
                      const GlobalData & gd, GIST_OPTIONS * go) {
  (void)go;

  assert(!results.empty());

  ResultData best_rd = results.back();
  // The last result with solution is always the best
  for (ResultData rd : results)
    if (rd.solution) best_rd = rd;

  // Accumulated statistics for the best ResultData
  best_rd.fail = 0;
  best_rd.node = 0;
  for (ResultData rd : results) {
    best_rd.fail += rd.it_fail;
    best_rd.node += rd.it_node;
  }
  best_rd.it_fail = -1;
  best_rd.it_node = -1;

  for (ResultData rd : results) {
    if (rd.solving_time > best_rd.solving_time)
      best_rd.solving_time = rd.solving_time;
  }
  best_rd.it_solving_time = -1;

  if (best_rd.solution) {

#ifdef GRAPHICS
    if (base->options->gist_solution()) Gist::dfs(best_rd.solution, *go);
#endif

    if (base->options->verbose()) {
      cerr << (best_rd.proven ? "optimal" : "best found") << " solution has "
           << cost_status_report(base, best_rd.solution) << endl;
    }
    if (base->options->emit_improvement()) {
      cerr << fixed << setprecision(2);
      cerr << cost_status_report(base, best_rd.solution) << endl;
    }
  }

  emit_lower_bound(base, best_rd.proven);

  if (base->options->output_file() == "") {
    cout << produce_json(best_rd, gd, base->input->N, 0) << endl;
  } else {
    ofstream fout;
    fout.open(base->options->output_file());
    fout << produce_json(best_rd, gd, base->input->N, 0);
    fout.close();
  }

  if (!(base->options->all_solutions() && !best_rd.proven)) exit(EXIT_SUCCESS);

}

void timeout_exit(GlobalModel * base, const vector<ResultData> & results,
                  const GlobalData & gd, GIST_OPTIONS * go, double time) {
  if (base->options->verbose())
    cerr << global() << "timeout (" << time << " ms)" << endl;
  emit_output_exit(base, results, gd, go);
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
  options.iterations(10);
  options.relax(0.7);
  options.seed(3);
  options.time(10*1000);

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

  // bool single_block = input.B.size() == 1;
//   int presolver_time = 0;
// #ifdef GRAPHICS
//   {
//     QScriptValue property = root.property("presolver_time");
//     if (property.isValid()) {
//       presolver_time = property.toInt32();
//     }
//   }
// #else
//   if (root.isMember("presolver_time")) {
//     presolver_time = root["presolver_time"].asInt();
//   }
// #endif

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
  Gist::Print<GlobalModel> * gprp =
    new Gist::Print<GlobalModel>("Problem variables");
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
  Gist::VarComparator<GlobalModel> *gprc =
    new Gist::VarComparator<GlobalModel>("Compare problem and secondary variables");
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



  vector<ResultData> results;
  // vector<vector<LocalModel *> > local_solutions;
  // for (unsigned int b = 0; b < input.B.size(); b++)
    // local_solutions.push_back(vector<LocalModel *>());

  // Best global cost so far
  // vector<int> best_cost;
  // for (unsigned int n = 0; n < input.N; n++)
    // best_cost.push_back(Int::Limits::max);


  bool proven = false;

  IterationState state(options.initial_aggressiveness(), false);

  Support::Timer t;
  t.start();



  // Code for diversification
  GlobalModel *d = new GlobalModel(&input, &options, IPL_DOM);
  GlobalData gd(d->n_int_vars, d->n_bool_vars, d->n_set_vars);

  // GlobalModel *d = (GlobalModel *)base->clone();
  d->post_complete_branchers(0);

  // double execution_time = t.stop();

  int count = 0;
  int maxcount = options.number_divs();

  Support::Timer t_it;
  Support::Timer t_solver;


  vector<int> ag_best_cost;
  int bestcost;

  cout << div() << "Before us optimal for div" << endl;
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


    // if (options.use_optimal_for_diversification()) {

    bestcost = solver.cost[0];
    cout << div() << "Best cost " << bestcost << endl;
    ag_best_cost.push_back(round((bestcost*(100. + (double)d->options->acceptable_gap()))/100.0));
    for (uint i=1; i < input.N; i++) {
      ag_best_cost.push_back(solver.cost[i]);
    }

    // }

  } else {

    bestcost = input.maxf[0];
    ag_best_cost.push_back(round((bestcost*(100. + (double)d->options->acceptable_gap()))/100.0));
    for (uint i=1; i < input.N; i++) {
      ag_best_cost.push_back(input.maxf[i]);
    }
  }


  d -> post_upper_bound(ag_best_cost);

  if (options.verbose())
    cerr << div() << cost_status_report(d, d) << endl;



  if (options.disable_lns_div()) {

    BAB<GlobalModel> e(d);

    t_solver.start();
    t_it.start();
    while (GlobalModel *nextg = e.next()) {

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


      GlobalModel *tmpg = d;
      d = nextg;
      delete tmpg;

      count++;
      t_it.start();

    }

    // execution_time = t.stop();

  } else {

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
    RBS<GlobalModel,BAB> e(d, o);

    t_solver.start();
    t_it.start();

    while (GlobalModel *nextg = e.next()) {

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

      GlobalModel *tmpg = d;
      d = nextg;
      delete tmpg;

      count++;
      t_it.start();

    }

    // execution_time = t.stop();
  }
  if (d!=NULL) delete d;




  // emit_output_exit(base, results, gd, go);

}
