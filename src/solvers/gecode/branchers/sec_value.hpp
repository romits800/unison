#ifndef __SEC_BRANCHER_VALUE__
#define __SEC_BRANCHER_VALUE__

#include "models/secmodel.hpp"


using namespace std;
using namespace Gecode;
using namespace Set;

// -1 or value closest to index
int select_value_tbt(const Space& s, IntVar x, unsigned int i);
int select_value_tat(const Space& s, IntVar x, unsigned int i);
#endif


