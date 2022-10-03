/*
 *  Authors:
 *    Rodothea Myrsini Tsoupidi <tsoupidi@kth.se>
 *
 *  This file is part of DivCon
 *
 *  Copyright (c) 2020, Rodothea Myrsini Tsoupidi
 *  All rights reserved.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software, to deal in the software without restriction,
 *  including without limitation the rights to use, copy, modify, merge,
 *  publish, distribute, sublicense, and/or sell copies of the software,
 *  and to permit persons to whom the software is furnished to do so, subject
 *  to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */


#include "solutionbrancher.hpp" 


using namespace Gecode;
using namespace Gecode::Int;


class SolutionBrancher : public Brancher {
protected:
  // Variables of the problem (registers/cycles)
  ViewArray<IntView> v;
  // Solution for the variables
  int* sol;
  // Cache of first unassigned view
  mutable int start;
  // Description
  class Description : public Choice {
  public:
    // Position of view
    int pos;
    int sol;


    Description(const SolutionBrancher& b, unsigned int a, int p, int s)
      : Choice(b,a), pos(p), sol(s){}
    // Report size occupied
    virtual size_t size(void) const {
      return sizeof(Description);
    }
    // Archive the choice's information in e
    virtual void archive(Archive& e) const {
      // std::cout << "Archive:" << std::endl;
      Choice::archive(e);
      // You must also archive the additional information
      e << pos << sol;
    }
  };
public:
  static int invoked; // = 0;	// 
  // Construct branching
  SolutionBrancher(Home home,
                   ViewArray<IntView>& v0, int sol0[])
    : Brancher(home), v(v0), sol(sol0), start(0) {}
  // Post branching
  static void post(Home home, ViewArray<IntView>& v, int sol[]) {
    (void) new (home) SolutionBrancher(home,v,sol);
  }

  // Copy constructor used during cloning of b
  SolutionBrancher(Space& home, SolutionBrancher& b)
    : Brancher(home, b), start(b.start) {
    v.update(home, b.v);
    sol = home.alloc<int>(v.size());
    for (int i=v.size(); i--; )
      sol[i]=b.sol[i];
  }
  // Copy brancher
  virtual Actor* copy(Space& home) {
    return new (home) SolutionBrancher(home, *this);
  }

  // Check status of brancher, return true if alternatives left
  virtual bool status(const Space&) const {
    if (invoked == 0) {
      for (int i=0; i < v.size(); i++)
	if ((sol[i] != -1) && (!v[i].assigned())) {
	  return true;
	}
      start++;
      invoked++;
      return false;
    }
    invoked++;
    return false;

  }
  // Return choice as description
  virtual const Choice* choice(Space&) {
    for (int i=0; i<v.size(); i++)
      if ((sol[i]!= -1) && (! v[i].assigned())) {
	return new Description(*this, 1, i, sol[i]);
        }
    GECODE_NEVER;
    return NULL;

  }
  // Construct choice from archive e
  virtual const Choice* choice(const Space&, Archive& e) {
    // Again, you have to take care of the additional information
    int pos, sol;
    e >> pos >> sol;
    return new Description(*this, 1, pos, sol);
  }
  // Perform commit for choice c and alternative a
  virtual ExecStatus commit(Space& home,
                            const Choice& c,
                            unsigned int a) {
    const Description& d = static_cast<const Description&>(c);

    int pos = d.pos, sol = d.sol;

    return me_failed(v[pos].eq(home, sol)) ? ES_FAILED: ES_OK;

  }
  // Print some information on stream o (used by Gist, from Gecode 4.0.1 on)
  virtual void print(const Space&, const Choice& c, unsigned int a,
                     std::ostream& o) const {

    const Description& d = static_cast<const Description&>(c);

    int pos = d.pos, sol = d.sol; // 

    o << "v[" << pos << "] = " << sol; 

  }
};

int SolutionBrancher::invoked = 0;

// This posts the interval branching
void solution_branch(Home home, const IntVarArgs& v, const IntArgs& sol) {
  // Check whether arguments make sense
  if (sol.size() != v.size())
    throw ArgumentSizeMismatch("solution_branch");
  // Never post a branching in a failed space
  if (home.failed()) return;
  // Create an array of integer views
  ViewArray<IntView> vv(home,v);
  // Create an array of integers
  int* vsol = static_cast<Space&>(home).alloc<int>(sol.size());
  for (int i=sol.size(); i--; )
    vsol[i]=sol[i];
  // Post the brancher
  SolutionBrancher::invoked = 0;
  SolutionBrancher::post(home,vv,vsol);
}

