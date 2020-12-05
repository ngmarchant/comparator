#pragma once

#include "IMeasures.h"

template<class ForwardRange>
class Constant : public Measure<ForwardRange> {
protected:
  double score_;
public:
  Constant(double score = 0.0) : 
  Measure<ForwardRange>(true, false, false),
  score_(score) {} 
  
  double eval(const ForwardRange& x, const ForwardRange& y) const override;
};

template<class ForwardRange>
double Constant<ForwardRange>::eval(const ForwardRange& x, const ForwardRange& y) const {
  return score_;
}