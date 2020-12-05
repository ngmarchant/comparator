#pragma once

#include "LCS.h"
#include <vector>

template<class ForwardRange>
class Levenshtein : public LCS<ForwardRange> {
protected:
  double sub_weight_;
  void fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dist) const;
public:
  Levenshtein(double deletion = 1.0, double insertion = 1.0, 
              double substitution = 1.0, bool normalize = false, 
              bool similarity = false) : 
  LCS<ForwardRange>(deletion, insertion, normalize, similarity), 
  sub_weight_(substitution) {}
};


template<class ForwardRange>
void Levenshtein<ForwardRange>::fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dmat) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  double sub_cost, del_cost, ins_cost;
  for (size_t i = 1; i <= nx; i++) {
    for (size_t j = 1; j <= ny; j++) {
      sub_cost = dmat[i - 1][j - 1] + ((*firstx == *firsty) ? 0 : sub_weight_);
      ins_cost = dmat[i][j - 1] + this->ins_weight_;
      del_cost = dmat[i - 1][j] + this->del_weight_;
      dmat[i][j] = std::min(sub_cost, std::min(ins_cost, del_cost));
      ++firsty;
    }
    firsty = std::begin(y);
    ++firstx;
  }
}