#include "Comparators.h"
#include "util.h"
#include "IComparator.h"

using namespace Rcpp;

const static std::unordered_map<std::string,int> classname_to_int {
  {"Hamming", 1},
  {"DamerauLevenshtein", 2},
  {"Levenshtein", 3},
  {"OSA", 4},
  {"LCS", 5},
  {"Constant", 6},
  {"BinaryComp", 7},
  {"JaroWinkler", 8},
  {"Jaro", 9},
};

template<class V>
std::unique_ptr<Comparator<V>> get_comparator(const Rcpp::S4& m) {
  std::string classname = as<std::string>(m.slot("class"));
  int class_int = classname_to_int.count(classname) ? classname_to_int.at(classname) : 0;
  switch (class_int) {
  case 1: { // Hamming
    return make_unique<Hamming<V>>(as<bool>(m.slot("normalize")), 
                                   as<bool>(m.slot("similarity")));
  }
  case 2: { // DamerauLevenshtein
    return make_unique<DamerauLevenshtein<V>>(as<double>(m.slot("deletion")), 
                                              as<double>(m.slot("insertion")),
                                              as<double>(m.slot("substitution")), 
                                              as<double>(m.slot("transposition")),
                                              as<bool>(m.slot("normalize")), 
                                              as<bool>(m.slot("similarity")));
  }
  case 3: { // Levenshtein
    return make_unique<Levenshtein<V>>(as<double>(m.slot("deletion")), 
                                       as<double>(m.slot("insertion")),
                                       as<double>(m.slot("substitution")),
                                       as<bool>(m.slot("normalize")), 
                                       as<bool>(m.slot("similarity")));
  }
  case 4: { // OSA
    return make_unique<OSA<V>>(as<double>(m.slot("deletion")), 
                               as<double>(m.slot("insertion")),
                               as<double>(m.slot("substitution")), 
                               as<double>(m.slot("transposition")), 
                               as<bool>(m.slot("normalize")), 
                               as<bool>(m.slot("similarity")));
  }
  case 5: { // LCS
    return make_unique<LCS<V>>(as<double>(m.slot("deletion")), 
                               as<double>(m.slot("insertion")),
                               as<bool>(m.slot("normalize")), 
                               as<bool>(m.slot("similarity")));
  }
  case 6: { // Constant
    return make_unique<Constant<V>>(as<double>(m.slot("constant")));
  }
  case 7: { // BinaryComp
    return make_unique<BinaryComp<V>>(as<double>(m.slot("score")), 
                                      as<bool>(m.slot("similarity")));
  }
  case 8: { // JaroWinkler
    return make_unique<JaroWinkler<V>>(as<double>(m.slot("p")), 
                                       as<double>(m.slot("threshold")), 
                                       as<int>(m.slot("max_prefix")), 
                                       as<bool>(m.slot("similarity")));
  }
  case 9: { // Jaro
    return make_unique<Jaro<V>>(as<bool>(m.slot("similarity")));
  }
  default: {
    stop("Unrecognized Comparator");
    return make_unique<Constant<V>>();
  }
  }
}


// Convert incomplete representation of a PairwiseMatrix to a full representation
// [[Rcpp::export]]
S4 sparse_to_full(S4& pmat) {
  NumericVector values = pmat.slot(".Data");
  IntegerVector dim = pmat.slot("Dim");
  int nrow = dim[0];
  int ncol = dim[1];
  if (nrow * ncol == values.size()) {
    return pmat;
  } else {
    LogicalVector diag = pmat.slot("Diag");
    NumericVector values_new = incomplete_to_full(values, nrow, diag[0]);
    S4 pmat_new("PairwiseMatrix");
    pmat_new.slot(".Data") = values_new;
    pmat_new.slot("Dim") = pmat.slot("Dim");
    pmat_new.slot("Diag") = LogicalVector::create(true);
    return pmat_new;
  }
}


// Create an S4 representation of a PairwiseMatrix
S4 pairwiseMatrix_to_S4(PairwiseMatrix& pmat) {
  S4 out("PairwiseMatrix");
  NumericVector values(pmat.cbegin(), pmat.cend());
  out.slot(".Data") = values;
  IntegerVector dim(2);
  dim[0] = pmat.nrow(); 
  dim[1] = pmat.ncol();
  out.slot("Dim") = dim;
  out.slot("Diag") = LogicalVector::create(pmat.diag());
  return out;
}

template<class V>
S4 pairwise_impl(const Comparator<V>* m, List& x, Nullable<List> y_, const LogicalVector& full) {
  S4 out;
  if (y_.isNotNull()) {
    const List y(y_);
    PairwiseMatrix pmat = m->pairwise(x.begin(), x.end(), y.begin(), y.end());
    out = pairwiseMatrix_to_S4(pmat);
  } else {
    PairwiseMatrix pmat = m->pairwise(x.begin(), x.end());
    if (full[0]) pmat.to_full();
    out = pairwiseMatrix_to_S4(pmat);
  }
  return out;
}

template<class V>
NumericVector elementwise_impl(const Comparator<V>* m, List& x, List& y) {
  std::vector<double> evec = m->elementwise(x.begin(), x.end(), y.begin(), y.end());
  NumericVector out(evec.begin(), evec.end());
  return out;
}

// [[Rcpp::export(elementwisecpp)]]
NumericVector elementwise(List& x, List& y, const S4& m_S4) {
  // Check inputs
  if (x.size() == 0) stop("`x` must be a non-empty list");
  if (y.size() == 0) stop("`y` must be a non-empty list");
  
  SEXP x0 = x[0];
  SEXP y0 = y[0];
  
  // Resolve type of vectors. Assume type is the same for all vectors in the 
  // list.
  int vec_type = TYPEOF(x0);
  if (TYPEOF(x0) != TYPEOF(y0)) {
    if (TYPEOF(x0) == NILSXP && TYPEOF(y0) != NILSXP) { 
      vec_type = TYPEOF(y0); 
    } else if (TYPEOF(y0) == NILSXP && TYPEOF(x0) != NILSXP) { 
      vec_type = TYPEOF(x0); 
    } else {
      vec_type = STRSXP;
    }
  }
  
  switch (vec_type) {
  case INTSXP: {
    auto comp = get_comparator<IntegerVector>(m_S4);
    return elementwise_impl(comp.get(), x, y);
  }
  case REALSXP: {
    auto comp = get_comparator<NumericVector>(m_S4);
    return elementwise_impl(comp.get(), x, y);
  }
  case STRSXP: {
    auto comp = get_comparator<CharacterVector>(m_S4);
    return elementwise_impl(comp.get(), x, y);
  }
  case LGLSXP: {
    auto comp = get_comparator<LogicalVector>(m_S4);
    return elementwise_impl(comp.get(), x, y);
  }
  case RAWSXP: {
    auto comp = get_comparator<RawVector>(m_S4);
    return elementwise_impl(comp.get(), x, y);
  }
  default: {
    stop("encountered unsupported vector type");
  }
  }
  
  return NumericVector();
}

// [[Rcpp::export(pairwisecpp)]]
S4 pairwise(List& x, Nullable<List> y_, const S4& m_S4, const LogicalVector& full) {
  // Check inputs
  if (x.size() == 0) stop("`x` must be a non-empty list");
  SEXP x0 = x[0];
  
  // Resolve type of vectors. Assume type is the same for all vectors in the 
  // list.
  int vec_type = TYPEOF(x0);
  
  if (y_.isNotNull()) {
    Rcpp::List y(y_);
    if (y.size() == 0) stop("`y` must be a non-empty list");
    
    // If types differ, use character vector type since it has the broadest 
    // representation
    SEXP y0 = y[0];
    if (TYPEOF(x0) != TYPEOF(y0)) vec_type = STRSXP;
  } 
  
  switch (vec_type) {
  case INTSXP: {
    auto comp = get_comparator<IntegerVector>(m_S4);
    return pairwise_impl(comp.get(), x, y_, full);
  }
  case REALSXP: {
    auto comp = get_comparator<NumericVector>(m_S4);
    return pairwise_impl(comp.get(), x, y_, full);
  }
  case STRSXP: {
    auto comp = get_comparator<CharacterVector>(m_S4);
    return pairwise_impl(comp.get(), x, y_, full);
  }
  case LGLSXP: {
    auto comp = get_comparator<LogicalVector>(m_S4);
    return pairwise_impl(comp.get(), x, y_, full);
  }
  case RAWSXP: {
    auto comp = get_comparator<RawVector>(m_S4);
    return pairwise_impl(comp.get(), x, y_, full);
  }
  default: {
    stop("encountered unsupported vector type");
  }
  }
  
  return S4("PairwiseMatrix");
}


