#include "sec_value.hpp"

int select_value_tbt(const Space&s, IntVar x, unsigned int i) {
  const SecModel& m = static_cast<const SecModel&>(s);
  // int closest = numeric_limits<int>::max();
  int ret = 0;
  int ind = m.tbts[i];
  Rnd r;
  r.seed(42);
  vector<int> vec;
  for (IntVarValues v(x); v(); ++v) {
    if (v.val() == -1) {
      ret = v.val();
      break;
    }
    else {
      vec.push_back(v.val());
    }      
    // else if (abs(v.val() - ind) < abs(closest)) {
    //   closest = abs(v.val() -ind);
    //   ret = v.val();
    // }
  }
  ret = (ret == -1) ? ret  : vec[r(vec.size())];
  std::cout << "TBT Selected value: " << i << " " << ind << " " <<  x << " " << ret << std::endl;
  return ret;
}

int select_value_tat(const Space&s, IntVar x, unsigned int i) {
  const SecModel& m = static_cast<const SecModel&>(s);
  // int closest = numeric_limits<int>::max();
  int ret = 0;
  unsigned int ind = m.tats[i];
  Rnd r;
  r.seed(42);
  vector<int> vec;
  for (IntVarValues v(x); v(); ++v) {
    if (v.val() == -1) {
      ret = v.val();
      break;
    }
    else {
      vec.push_back(v.val());
    }
  }
  ret = (ret == -1) ? ret : vec[r(vec.size())];
  std::cout << "TAT Selected value: " << i << " " << ind << " " <<  x << " " << ret << std::endl;
  return ret;
}


// int select_value_tat(const Space&s, IntVar x, unsigned int i) {
//   const SecModel& m = static_cast<const SecModel&>(s);
//   int closest = numeric_limits<int>::max();
//   int ret;
//   unsigned int ind = m.tats[i];
//   for (IntVarValues v(x); v(); ++v) {
//     if (v.val() == -1) {
//       ret = v.val();
//       break;
//     }
//     else if (abs(v.val() - ind) < abs(closest)) {
//       closest = abs(v.val() -ind);
//       ret = v.val();
//     }
//   }
//   std::cout << "TAT Selected value: " << i << " " << ind << " " <<  x << " " << ret << std::endl;
//   return ret;
// }
