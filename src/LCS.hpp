#pragma once

#include "IMeasures.hpp"
#include <vector>

using Mat = std::vector<std::vector<double> >;

template<class ForwardRange>
class LCS : public NormalizableMeasure<ForwardRange> {
public:
  LCS(bool normalize = false, bool similarity = false) : 
  NormalizableMeasure<ForwardRange>(normalize | similarity, true, !similarity, similarity) {}
  
  double eval(const ForwardRange& x, const ForwardRange& y) const;
};

template<class ForwardRange>
double LCS<ForwardRange>::eval(const ForwardRange& x, const ForwardRange& y) const {
  double result;
  
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  if (nx == 0) {
    result = ny;
  } else if (ny == 0) {
    result = nx;
  } else {
    size_t nrow = nx + 1;
    size_t ncol = ny + 1;
    Mat dmat(nrow, std::vector<double>(ncol));
    
    // initialize left-most column and top-most row
    for (size_t i = 0; i < nrow; i++) {
      dmat[i][0] = i;
    }
    for (size_t j = 0; j < ncol; j++) {
      dmat[0][j] = j;
    }
    
    for (size_t i = 1; i <= nx; i++) {
      for (size_t j = 1; j <= ny; j++) {
        if (*firstx == *firsty){ 
          dmat[i][j] = dmat[i-1][j-1];
        } else {
          dmat[i][j] = std::min(
            dmat[i][j-1] + 1, // insertion
            dmat[i-1][j] + 1  // deletion
          );
        }
        ++firsty;
      }
      firsty = std::begin(y);
      ++firstx;
    }

    result = dmat.back().back();
  }
  
  if (this->normalize_) {
    if (nx == 0 && ny == 0) {
      result = 0.0;
    } else {
      result = 2.0 * result / (nx + ny + result);
    }
  }
  
  if (this->similarity_) result = 1.0 - result;

  return result;
}