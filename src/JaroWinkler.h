#pragma once

#include "Jaro.h"

template<class RandomAccessRange>
class JaroWinkler : public Jaro<RandomAccessRange> {
protected:
  double p_;
  double threshold_;
  size_t max_prefix_;
public:
  JaroWinkler(double p = 0.1, double threshold = 0.7, int max_prefix = 4, 
              bool similarity = true) : 
  Jaro<RandomAccessRange>(similarity), 
  p_(p),
  threshold_(threshold),
  max_prefix_(max_prefix)
  {}
  
  double eval(const RandomAccessRange& x, const RandomAccessRange& y) const;
};

template<class RandomAccessRange>
double JaroWinkler<RandomAccessRange>::eval(const RandomAccessRange& x, const RandomAccessRange& y) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  double result = Jaro<RandomAccessRange>::eval(x, y);
  
  if (result > threshold_) {
    // Get size of common prefix
    size_t common_size = 0;
    while (firstx != lastx && firsty != lasty && common_size < max_prefix_) {
      if (*firstx != *firsty) break;
      common_size++;
      ++firstx;
      ++firsty;
    }
    if (this->similarity_) {
      result = result + common_size * p_ * (1.0 - result);
    } else {
      result = result - common_size * p_ * result;
    }
  }
  
  return result;
}