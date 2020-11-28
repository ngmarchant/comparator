#pragma once

#include "Levenshtein.hpp"
#include <vector>

template<class ForwardRange>
class OSA : public Levenshtein<ForwardRange> {
protected:
  double tra_weight_;
  void fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dist) const;
public:
  OSA(double deletion = 1.0, double insertion = 1.0, double substitution = 1.0, 
      double transposition = 1.0, bool normalize = false, bool similarity = false) : 
  Levenshtein<ForwardRange>(deletion, insertion, substitution, normalize, similarity),
  tra_weight_(transposition)
  {
    this->symmetric_ = this->symmetric_ && this->ins_weight_ == tra_weight_;
  }
};


template<class ForwardRange>
void OSA<ForwardRange>::fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dmat) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  auto prevx = std::begin(x);
  auto prevy = std::begin(y);
  double s, t;
  double sub_cost, del_cost, ins_cost;
  // fill in the rest of H
  for (size_t i = 1; i <= nx; i++) {
    for (size_t j = 1; j <= ny; j++) {
      if (*firstx == *firsty) {
        s = 0;
        t = 0;
      } else {
        s = this->sub_weight_;
        t = this->tra_weight_;
      }
      
      sub_cost = dmat[i-1][j-1] + s;
      ins_cost = dmat[i][j-1] + this->ins_weight_;
      del_cost = dmat[i-1][j] + this->del_weight_;
      dmat[i][j] = std::min(sub_cost, std::min(ins_cost, del_cost));
      if (i > 1 && j > 1 && *firstx == *prevy && *prevx == *firsty) {
        dmat[i][j] = std::min(dmat[i][j], dmat[i-2][j-2] + t);
      }
      if (j != 1) ++prevy;
      ++firsty;
    }
    prevy = std::begin(y);
    firsty = std::begin(y);
    if (i != 1) ++prevx;
    ++firstx;
  }
}