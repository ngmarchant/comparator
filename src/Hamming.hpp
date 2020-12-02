#pragma once

#include <numeric>
#include <limits>
#include "IMeasures.hpp"

template<class ForwardRange>
class Hamming : public NormalizableMeasure<ForwardRange> {
public:
  Hamming(bool normalize = false, bool similarity = false) : 
  NormalizableMeasure<ForwardRange>(normalize || similarity, true, !similarity, similarity) {} 
  
  double eval(const ForwardRange& x, const ForwardRange& y) const override;
};

template<class ForwardRange>
double Hamming<ForwardRange>::eval(const ForwardRange& x, const ForwardRange& y) const {
  double result = 0.0;
  
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  if (nx != ny) {
    result = std::numeric_limits<double>::infinity();
  } else {
    result = nx;
    while (firstx != lastx) {
      result -= *firstx == *firsty;
      ++firstx;
      ++firsty;
    }
  }

  if (this->normalize_) {
    if (nx != ny) {
      result = 1.0;
    } else {
      result = nx == 0 ? 0 : result / nx;
    }
  }
  
  if (this->similarity_) result = 1.0 - result;
  
  return result;
}