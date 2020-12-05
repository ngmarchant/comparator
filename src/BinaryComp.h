#pragma once

#include "IMeasures.h"
#include <Rcpp.h>

template<class ForwardRange>
class BinaryComp : public Measure<ForwardRange> {
protected:
  double score_;
public:
  BinaryComp(double score = 1.0, bool similarity = false) : 
  Measure<ForwardRange>(true, !similarity, similarity),
  score_(score) {} 
  
  double eval(const ForwardRange& x, const ForwardRange& y) const override;
};

template<class ForwardRange>
double BinaryComp<ForwardRange>::eval(const ForwardRange& x, const ForwardRange& y) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  bool identical = true;
  while (firstx != lastx && firsty != lasty) {
    if (*firstx != *firsty) {
      identical = false;
      break;
    }
    ++firstx;
    ++firsty;
  }
  
  if ((firstx != lastx) || (firsty != lasty)) identical = false; 
  
  if (this->distance_) {
    return identical ? 0.0 : score_;
  } else {
    return identical ? score_ : 0.0;
  }
}