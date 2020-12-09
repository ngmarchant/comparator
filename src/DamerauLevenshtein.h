#pragma once

#include "OSA.h"
#include <vector>
#include <unordered_map>

template<class ForwardRange>
class DamerauLevenshtein : public OSA<ForwardRange> {
private:
  // Elements of Rcpp::CharacterVector are of type Rcpp::internal::string_proxy or 
  // Rcpp::internal::const_string_proxy. These can't easily be used as keys in an 
  // unordered_map. So we map these types to Rcpp::String instead.
  template <class T>
  struct type_map { using type = T; };
  template <int RTYPE, template <class> class StoragePolicy>
  struct type_map<Rcpp::internal::string_proxy<RTYPE, StoragePolicy>> { using type = Rcpp::String; };
  template <int RTYPE, template <class> class StoragePolicy>
  struct type_map<Rcpp::internal::const_string_proxy<RTYPE, StoragePolicy>> { using type = Rcpp::String; };
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

template<class ForwardRange>
void DamerauLevenshtein<ForwardRange>::fill_dmat(const ForwardRange &x, const ForwardRange &y, Mat& dmat) const {
  auto firstx = std::begin(x);
  auto firsty = std::begin(y);
  auto lastx = std::end(x);
  auto lasty = std::end(y);
  
  size_t nx = std::distance(firstx, lastx);
  size_t ny = std::distance(firsty, lasty);
  
  // Get value type, with special behavior for Rcpp::CharacterVector
  using ValueType = typename type_map<typename ForwardRange::value_type>::type;
  using Key = typename std::remove_reference<ValueType>::type;
  std::unordered_map<Key, int> da;
  
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
    auto const result = da.emplace(*firstx, i);
    if (not result.second) { result.first->second = i; }
    firsty = std::begin(y);
    ++firstx;
  }
}