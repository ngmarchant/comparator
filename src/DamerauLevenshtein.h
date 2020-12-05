#pragma once

#include "OSA.h"
#include <vector>
#include <unordered_map>

template<class ForwardRange>
class DamerauLevenshtein : public OSA<ForwardRange> {
protected:
  Mat init_dmat(size_t nx, size_t ny) const;
  void fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dist) const;
public:
  DamerauLevenshtein(double deletion = 1.0, double insertion = 1.0, 
                     double substitution = 1.0, double transposition = 1.0, 
                     bool normalize = false, bool similarity = false) : 
  OSA<ForwardRange>(deletion, insertion, substitution, transposition, normalize, similarity) {}
};

template<class ForwardRange>
Mat DamerauLevenshtein<ForwardRange>::init_dmat(size_t nx, size_t ny) const {
  size_t nrow = nx + 2;
  size_t ncol = ny + 2;
  Mat dmat(nrow, std::vector<double>(ncol));
  
  // initialize two left-most columns and two top-most rows
  double max_dist = nx + ny;
  dmat[0][0] = max_dist;
  for (size_t i = 0; i <= nx; i++) {
    dmat[i + 1][0] = max_dist;
    dmat[i + 1][1] = this->del_weight_ * i;
  }
  for (size_t j = 0; j <= ny; j++) {
    dmat[0][j + 1] = max_dist;
    dmat[1][j + 1] = this->ins_weight_ * j;
  }
  
  return dmat;
}

namespace std 
{
template<typename T>
struct hash<reference_wrapper<T>>
{
  size_t operator()(const reference_wrapper<T>& r) const
  {
    return std::hash<T>()(r.get());
  }
}; 

template<typename T>
struct hash<const T>
{
  size_t operator()(const T r) const
  {
    return std::hash<T>()(r);
  }
}; 

// template<typename T>
// struct equal_to<reference_wrapper<T>> {
//   bool operator()(const reference_wrapper<T>& lhs, const reference_wrapper<T>& rhs) const {
//     return lhs.get() == rhs.get();
//   }
// };
}

template<class ForwardRange>
void DamerauLevenshtein<ForwardRange>::fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dmat) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  using Key = typename std::remove_reference<typename ForwardRange::value_type>::type;
  std::unordered_map<std::reference_wrapper<const Key>, int> da;
  //std::map<std::reference_wrapper<typename std::add_const<Key>::type>, int> da;
  
  size_t db, k;
  double sub_cost, del_cost, ins_cost, tra_cost;
  // fill in the rest of H
  for (size_t i = 1; i <= nx; i++) {
    db = 0;
    for (size_t j = 1; j <= ny; j++) {
      const auto& temp = *firsty;
      auto search = da.find(temp);
      k = search != da.end() ? search->second : 0;
      tra_cost = dmat[k][db] + (i - k - 1 + j - db) * this->tra_weight_;
      
      if (*firstx != *firsty) {
        sub_cost = dmat[i][j] + this->sub_weight_;
        ins_cost = dmat[i+1][j] + this->ins_weight_;
        del_cost = dmat[i][j+1] + this->del_weight_;
        dmat[i+1][j+1] = std::min(sub_cost, std::min(ins_cost, 
                                                std::min(del_cost, tra_cost)));
      } else {
        db = j;
        dmat[i+1][j+1] = std::min(dmat[i][j], tra_cost);
      }
      ++firsty;
    }
    da.emplace(*firstx, i);
    firsty = std::begin(y);
    ++firstx;
  }
}