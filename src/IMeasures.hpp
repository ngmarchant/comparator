#pragma once

#include "PairwiseMatrix.hpp"

template<class Range> 
class Measure {
protected:
  bool symmetric_;
  bool distance_;
  bool similarity_;
public:
  Measure(bool symmetric, bool distance, bool similarity) : 
  symmetric_(symmetric), 
  distance_(distance), 
  similarity_(similarity) {}
  
  virtual double eval(const Range& x, const Range& y) const = 0;
  
  template <class ForwardIterator1, class ForwardIterator2>
  PairwiseMatrix pairwise(ForwardIterator1 first1, ForwardIterator1 last1, 
                          ForwardIterator2 first2, ForwardIterator2 last2) const;
  
  template <class ForwardIterator>
  PairwiseMatrix pairwise(ForwardIterator first1, ForwardIterator last1) const;
  
  template <class ForwardIterator1, class ForwardIterator2>
  std::vector<double> elementwise(ForwardIterator1 first1, ForwardIterator1 last1, 
                                  ForwardIterator2 first2, ForwardIterator2 last2) const;
  
  bool is_symmetric() const { return symmetric_; }
  
  bool is_distance() const { return distance_; }
  
  bool is_similarity() const { return similarity_; }
};

template<class Range> 
class NormalizableMeasure : public Measure<Range> {
protected:
  bool normalize_;
public:
  NormalizableMeasure(bool normalize, bool symmetric, bool distance, bool similarity) : 
  Measure<Range>(symmetric, distance, similarity),
  normalize_(normalize) {}
  
  bool normalized() const { return normalize_; }
};


template <class RangeIterator>
template <class ForwardIterator1, class ForwardIterator2>
PairwiseMatrix Measure<RangeIterator>::pairwise(ForwardIterator1 first1, ForwardIterator1 last1, 
                                                ForwardIterator2 first2, ForwardIterator2 last2) const {
  ForwardIterator1 curr1;
  size_t nrow = std::distance(first1, last1);
  size_t ncol = std::distance(first2, last2);
  
  PairwiseMatrix result(nrow, ncol, true, true);
  auto itresult = result.begin();
  while (first2 != last2) {
    curr1 = first1;
    while (curr1 != last1) {
      *itresult = eval(*curr1, *first2);
      ++itresult;
      ++curr1;
    }
    ++first2;
  }
  return result;
}


template <class RangeIterator>
template <class ForwardIterator>
PairwiseMatrix Measure<RangeIterator>::pairwise(ForwardIterator first1, 
                                                ForwardIterator last1) const {
  if (!is_symmetric()) {
    return pairwise(first1, last1, first1, last1);
  } else {
    // only need to compute lower triangle
    ForwardIterator curr1;

    size_t n = std::distance(first1, last1);
    PairwiseMatrix result(n, n, false, !is_distance());
    auto itresult = result.begin();
    while (first1 != last1) {
      curr1 = first1;
      if (is_distance()) ++curr1; // only need to compute lower triangle
      while (curr1 != last1) {
        *itresult = eval(*curr1, *first1);
        ++itresult;
        ++curr1;
      }
      ++first1;
    }
    return result;
  }
}


template <class RangeIterator>
template <class ForwardIterator1, class ForwardIterator2>
std::vector<double> Measure<RangeIterator>::elementwise(ForwardIterator1 first1, ForwardIterator1 last1, 
                                                        ForwardIterator2 first2, ForwardIterator2 last2) const {
  std::vector<double> result;
  std::size_t size1 = std::distance(first1, last1);
  std::size_t size2 = std::distance(first2, last2);
  
  // Can't compare if either range is empty
  if (size1 == 0 || size2 == 0) return result;
  
  // Ensure first range is larger than second
  if (size2 > size1) {
    std::swap(first1, first2);
    std::swap(last1, last2);
    std::swap(size1, size2);
  }
  
  result.resize(size1);
  auto itresult = result.begin();

  // Iterate over first range, recycling elements from second range
  ForwardIterator2 curr2 = first2;
  while (first1 != last1) {
    if (curr2 == last2) curr2 = first2; // recycle
    *itresult = eval(*first1, *curr2);
    ++itresult;
    ++curr2;
    ++first1;
  }

  return result;
}