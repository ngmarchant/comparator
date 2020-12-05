#include "Measures.h"
#include "util.h"
#include "IMeasures.h"

using namespace Rcpp;

template<class V>
std::unique_ptr<Measure<V>> get_measure(const Rcpp::List& params) {
  std::string classname = as<std::string>(params["class"]);
  if (classname == "Hamming") {
    return make_unique<Hamming<V>>(as<bool>(params["normalize"]), 
                                   as<bool>(params["similarity"]));
  }
  if (classname == "DamerauLevenshtein") {
    return make_unique<DamerauLevenshtein<V>>(as<double>(params["deletion"]), 
                                              as<double>(params["insertion"]),
                                              as<double>(params["substitution"]), 
                                              as<double>(params["transposition"]),
                                              as<bool>(params["normalize"]), 
                                              as<bool>(params["similarity"]));
  }
  if (classname == "Levenshtein") {
    return make_unique<Levenshtein<V>>(as<double>(params["deletion"]), 
                                       as<double>(params["insertion"]),
                                       as<double>(params["substitution"]),
                                       as<bool>(params["normalize"]), 
                                       as<bool>(params["similarity"]));
  }
  if (classname == "OSA") {
    return make_unique<OSA<V>>(as<double>(params["deletion"]), 
                               as<double>(params["insertion"]),
                               as<double>(params["substitution"]), 
                               as<double>(params["transposition"]), 
                               as<bool>(params["normalize"]), 
                               as<bool>(params["similarity"]));
  }
  if (classname == "LCS") {
    return make_unique<LCS<V>>(as<double>(params["deletion"]), 
                               as<double>(params["insertion"]),
                               as<bool>(params["normalize"]), 
                               as<bool>(params["similarity"]));
  }
  if (classname == "Constant") {
    return make_unique<Constant<V>>(as<double>(params["constant"]));
  }
  if (classname == "BinaryComp") {
    return make_unique<BinaryComp<V>>(as<double>(params["score"]), 
                                      as<double>(params["similarity"]));
  }
  stop("unrecognized measure name");
  return make_unique<Hamming<V>>();
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
S4 pairwise_impl(const Measure<V>* m, List& x, Nullable<List> y_, const LogicalVector& full) {
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
NumericVector elementwise_impl(const Measure<V>* m, List& x, List& y) {
  std::vector<double> evec = m->elementwise(x.begin(), x.end(), y.begin(), y.end());
  NumericVector out(evec.begin(), evec.end());
  return out;
}

// [[Rcpp::export(elementwisecpp)]]
NumericVector elementwise(List& x, List& y, const List& attrs) {
  // Check inputs
  if (x.size() == 0) stop("`x` must be a non-empty list");
  if (y.size() == 0) stop("`y` must be a non-empty list");
  
  SEXP x0 = x[0];
  SEXP y0 = y[0];
  
  // Resolve type of vectors. Assume type is the same for all vectors in the 
  // list.
  int vec_type = TYPEOF(x0) == TYPEOF(y0) ? TYPEOF(x0) : STRSXP;
  
  switch (vec_type) {
  case INTSXP: {
    auto m = get_measure<IntegerVector>(attrs);
    return elementwise_impl(m.get(), x, y);
  }
  case REALSXP: {
    auto m = get_measure<NumericVector>(attrs);
    return elementwise_impl(m.get(), x, y);
  }
  // case STRSXP: {
  //   auto m = get_measure<CharacterVector>(attrs);
  //   return elementwise_impl(m.get(), x, y);
  // }
  case LGLSXP: {
    auto m = get_measure<LogicalVector>(attrs);
    return elementwise_impl(m.get(), x, y);
  }
  case RAWSXP: {
    auto m = get_measure<RawVector>(attrs);
    return elementwise_impl(m.get(), x, y);
  }
  default: {
    stop("encountered unsupported vector type");
  }
  }
  
  return NumericVector();
}

// [[Rcpp::export(pairwisecpp)]]
S4 pairwise(List& x, Nullable<List> y_, const List& attrs, const LogicalVector& full) {
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
    auto m = get_measure<IntegerVector>(attrs);
    return pairwise_impl(m.get(), x, y_, full);
  }
  case REALSXP: {
    auto m = get_measure<NumericVector>(attrs);
    return pairwise_impl(m.get(), x, y_, full);
  }
  // case STRSXP: {
  //   auto& m = get_measure<CharacterVector>(attrs);
  //   return pairwise_impl(m.get(), x, y_, full);
  // }
  case LGLSXP: {
    auto m = get_measure<LogicalVector>(attrs);
    return pairwise_impl(m.get(), x, y_, full);
  }
  case RAWSXP: {
    auto m = get_measure<RawVector>(attrs);
    return pairwise_impl(m.get(), x, y_, full);
  }
  default: {
    stop("encountered unsupported vector type");
  }
  }
  
  return S4("PairwiseMatrix");
}


