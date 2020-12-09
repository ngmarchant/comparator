#pragma once

#include "IComparator.h"
#include <vector>
#include <algorithm>

inline std::size_t operator "" _sz (unsigned long long int x)
{
  return x;
}

template<class RandomAccessRange>
class Jaro : public Comparator<RandomAccessRange> {
public:
  Jaro(bool similarity = true) : 
  Comparator<RandomAccessRange>(true, !similarity, similarity) {}
  
  double eval(const RandomAccessRange& x, const RandomAccessRange& y) const;
};

template<class RandomAccessRange>
double Jaro<RandomAccessRange>::eval(const RandomAccessRange& x, const RandomAccessRange& y) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  if (nx == 0 && ny == 0) {
    if (this->similarity_) return 1.0;
    return 0.0;
  }
  
  // Ensure x is the smaller range
  if (nx > ny) {
    std::swap(firstx, firsty);
    std::swap(lastx, lasty);
    std::swap(nx, ny);
  }
  
  // One-sided width of window within which to look for matches in y
  size_t match_range = std::max(0_sz, ny / 2 - 1);
  
  // Vector of binary indicators for each element of y. Indicator will be true 
  // if corresponding element in y has been matched with an element in x
  std::vector<bool> matched(ny, false);
  // Location of matched elements in x
  std::vector<size_t> match_idx;
  
  // Greedy matching step
  size_t i, j, j_first, j_last;
  for (i = 0; i < nx; ++i) {
    j_first = match_range > i ? 0 : i - match_range;
    j_last = std::min(ny, i + match_range + 1);
    for (j = j_first; j < j_last; ++j) {
      if (firstx[i] == firsty[j] && !matched[j]) {
        matched[j] = true;
        match_idx.push_back(i);
        break;
      }
    }
  }
  
  size_t n_match = match_idx.size();
  if (n_match == 0) {
    if (this->similarity_) return 0.0;
    return 1.0;
  }
  
  // Count number of transpositions
  int n_trans = 0;
  i = 0;
  for (j = 0; j < ny; ++j) {
    if (matched[j]) {
      n_trans += (firstx[match_idx[i]] != firsty[j]);
      i += 1;
    }
  }
  
  // Similarity
  double result = (n_match / (double) nx + n_match / (double) ny + (n_match - n_trans/2) / (double) n_match) / 3.0;
   
  // Distance
  if (!this->similarity_) result = 1.0 - result;
  
  return result;
}