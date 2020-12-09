#pragma once

#include <vector>
#include <stdexcept>

template <class Vector>
Vector incomplete_to_full(const Vector& incomplete, size_t nrow, bool diag) {
  Vector out(nrow * nrow);
  size_t ii, jj;
  for (size_t i = 0; i < nrow; i++) {
    for (size_t j = 0; j < nrow; j++) {
      if (!diag && i == j) {
        out[i + j * nrow] = 0.0;
      } else {
        ii = i >= j ? i : j;
        jj = i >= j ? j : i;
        size_t idx = ii + jj * (nrow - 1) - jj * (jj - 1) / 2;
        if (!diag) idx += -1 - jj;
        out[i + j * nrow] = incomplete[idx];
      }
    }
  }
  return out;
}

class PairwiseMatrix : private std::vector<double> {
private:
  typedef std::vector<double> base_vector;

  // Entries of the matrix stored in column-major order in the vector. Some entries may be omitted (e.g. upper 
  // triangle) if the comparator is symmetric.
  size_t nrow_;
  size_t ncol_;
  bool is_full_;
  bool diag_;
public:
  typedef typename base_vector::size_type       size_type;
  typedef typename base_vector::iterator        iterator;
  typedef typename base_vector::const_iterator  const_iterator;

  using base_vector::operator[];
  using base_vector::base_vector;
  using base_vector::begin;
  using base_vector::cbegin;
  using base_vector::clear;
  using base_vector::end;
  using base_vector::cend;
  using base_vector::erase;
  using base_vector::push_back;
  using base_vector::reserve;
  using base_vector::resize;
  using base_vector::size;

  PairwiseMatrix(size_t nrow, size_t ncol, bool is_full, bool diag) :
  std::vector<double>(is_full ? nrow * ncol : (diag ? nrow * (nrow + 1) / 2 : nrow * (nrow - 1) / 2 )), 
  nrow_(nrow),
  ncol_(ncol),
  is_full_(is_full),
  diag_(diag)
  {
    if (!is_full && (nrow != ncol)) {
      throw std::invalid_argument( "matrix must be square if not full" );
    }
  }
  
  // Whether all entries of the matrix are stored
  bool is_full() const { return is_full_; }

  // Whether the diagonal entries are stored. These must be stored if the scores represent similarities.
  bool diag() const { return diag_; } 

  size_t nrow() const { return nrow_; }

  size_t ncol() const { return ncol_; }

  // Access matrix entry
  double operator()(size_t i, size_t j) const { 
    if (is_full_) {
      return this->at(i + j * nrow_);
    }
    // Matrix must be square
    if (!diag_ && i == j) return 0.0;
    if (i < j) std::swap(i,j);
    size_t idx = i + j * (nrow_ - 1) - j * (j - 1) / 2;
    if (!diag_) idx += -1 - j;
    return this->at(idx);
  }

  void to_full() {
    if (!is_full()) {
      base_vector new_this = incomplete_to_full(*this, nrow_, diag_);
      this->swap(new_this);
      is_full_ = true;
      diag_ = true;
    }
  }
};