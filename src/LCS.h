#pragma once

#include "IComparator.h"
#include <vector>

using Mat = std::vector<std::vector<double> >;

template<class ForwardRange>
class LCS : public NormalizableComparator<ForwardRange> {
protected:
  double ins_weight_;
  double del_weight_;
  virtual Mat init_dmat(size_t nx, size_t ny) const;
  virtual void fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dist) const;
public:
  LCS(double deletion = 1.0, double insertion = 1.0, bool normalize = false, bool similarity = false) : 
  NormalizableComparator<ForwardRange>(normalize, true, !similarity, similarity), 
  ins_weight_(insertion),
  del_weight_(deletion) 
  {
    this->symmetric_ = insertion == deletion;
  }
  
  double eval(const ForwardRange& x, const ForwardRange& y) const;
};

template<class ForwardRange>
Mat LCS<ForwardRange>::init_dmat(size_t nx, size_t ny) const {
  size_t nrow = nx + 1;
  size_t ncol = ny + 1;
  Mat dmat(nrow, std::vector<double>(ncol));
  
  // initialize left-most column and top-most row
  for (size_t i = 0; i < nrow; i++) {
    dmat[i][0] = del_weight_ * i;
  }
  for (size_t j = 0; j < ncol; j++) {
    dmat[0][j] = ins_weight_ * j;
  }
  
  return dmat;
}

template<class ForwardRange>
void LCS<ForwardRange>::fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dmat) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);

  for (size_t i = 1; i <= nx; i++) {
    for (size_t j = 1; j <= ny; j++) {
      if (*firstx == *firsty){ 
        dmat[i][j] = dmat[i-1][j-1];
      } else {
        dmat[i][j] = std::min(
          dmat[i][j-1] + ins_weight_, // insertion
          dmat[i-1][j] + del_weight_  // deletion
        );
      }
      ++firsty;
    }
    firsty = std::begin(y);
    ++firstx;
  }
}

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
    result = ins_weight_ * ny;
  } else if (ny == 0) {
    result = del_weight_ * nx;
  } else {
    auto dmat = this->init_dmat(nx, ny);
    this->fill_dmat(x, y, dmat);
    result = dmat.back().back();
  }

  if (this->similarity_) {
    // Generalized edit similarity as in Yujian and Bo, 10.1109/TPAMI.2007.1078
    result = (ins_weight_ * ny + del_weight_ * nx - result) / 2.0;
  }

  if (this->normalize_) {
    if (nx == 0 && ny == 0) {
      result = this->similarity_ ? 1.0 : 0.0;
    } else {
      if (this->similarity_) {
        result = result / (ins_weight_ * ny + del_weight_ * nx - result);
      } else {
        result = 2.0 * result / (ins_weight_ * ny + del_weight_ * nx + result);
      }
    }
  }

  return result;
}