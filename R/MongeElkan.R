#' @include TokenComparator.R StringComparator.R Levenshtein.R
NULL

# Shared implementation for scoring of a single pair of values for 
# sim_MongeElkan and dist_MongeElkan
.impl_inner.MongeElkan <- function(t1, t2, inner_comparator, inner_opt, agg_function, 
                                   symmetrize) {
  scores <- pairwise(inner_comparator, t1, t2)
  scores <- as.matrix(scores)
  optScores1 <- apply(scores, 1, inner_opt)
  if (symmetrize) {
    optScores2 <- apply(scores, 2, inner_opt)
    return(inner_opt(agg_function(optScores1), agg_function(optScores2)))
  } else {
    return(agg_function(optScores1))
  }
}

setClass("MongeElkan", contains = "TokenComparator", 
         slots = c(inner_comparator = "StringComparator",
                   agg_function = "function", 
                   inner_opt = "function", 
                   symmetrize = "logical"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           inner_comparator = Levenshtein(similarity = TRUE),
           agg_function = base::mean,
           inner_opt = base::max, 
           symmetrize = FALSE,
           similarity = TRUE,
           ordered = FALSE
         ), 
         validity = function(object) {
           errs <- character()
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           if (object@ordered)
             errs <- c(errs, "`ordered` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Monge-Elkan Token Comparator
#' 
#' @description 
#' Compares a pair of token sets \eqn{x} and \eqn{y} by computing similarity 
#' scores between all pairs of tokens using an internal string comparator, 
#' then taking the mean of the maximum scores for each token in \eqn{x}.
#' 
#' @details
#' A token set is an unordered enumeration of tokens, which may include 
#' duplicates.
#' Given two token sets \eqn{x} and \eqn{y}, the Monge-Elkan comparator is 
#' defined as:
#' \deqn{\mathrm{ME}(x, y) = \frac{1}{|x|} \sum_{i = 1}^{|x|} \max_j \mathrm{sim}(x_i, y_j)}{ME(x, y) = 1/|x| sum_i{ max_j sim(x_i, y_j) }}
#' where \eqn{x_i} is the i-th token in \eqn{x}, \eqn{|x|} is the 
#' number of tokens in \eqn{x} and \eqn{\mathrm{sim}}{sim} is an internal 
#' string similarity comparator. 
#' 
#' A generalization of the original Monge-Elkan comparator is implemented here, 
#' which allows for distance comparators in place of similarity comparators, 
#' and/or more general aggregation functions in place of the arithmetic mean. 
#' The generalized Monge-Elkan comparator is defined as:
#' \deqn{\mathrm{ME}(x, y) = \mathrm{agg}(\mathrm{opt}_j \ \mathrm{inner}(x_i, y_j))}{ME(x, y) = agg(opt_j inner(x_i, y_j))}
#' where \eqn{\mathrm{inner}}{inner} is an internal distance or similarity 
#' comparator, \eqn{\mathrm{opt}}{opt} is \eqn{\max}{max} if 
#' \eqn{\mathrm{inner}}{inner} is a similarity comparator or \eqn{\min}{min} if 
#' it is a distance comparator, and \eqn{\mathrm{agg}}{agg} is an aggregation 
#' function which takes a vector of scores for each token in \eqn{x} and 
#' returns a scalar.
#' 
#' By default, the Monge-Elkan comparator is asymmetric in its arguments \eqn{x} 
#' and \eqn{y}. If `symmetrize = TRUE`, a symmetric version of the comparator 
#' is obtained as follows
#' \deqn{\mathrm{ME}_{sym}(x, y) = \mathrm{opt} \ \{\mathrm{ME}(x, y), \mathrm{ME}(y, x)\}}{ME_sym(x, y) = opt { ME(x, y), ME(y, x) }}
#' where \eqn{\mathrm{opt}}{opt} is defined above.
#' 
#' @param inner_comparator internal string comparator of class 
#'   [`StringComparator-class`]. Defaults to [`Levenshtein`] similarity.
#' @param agg_function aggregation function to use when aggregating internal 
#'   similarities/distances between tokens. Defaults to [`mean`], 
#'   however [`hmean`] may be a better choice when the comparator returns 
#'  normalized similarity scores.
#' @param symmetrize logical indicating whether to use a symmetrized version 
#'   of the Monge-Elkan comparator. Defaults to FALSE.
#' 
#' @return
#' A `MongeElkan` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @references 
#' Monge, A. E., & Elkan, C. (1996), "The Field Matching 
#' Problem: Algorithms and Applications", In \emph{Proceedings of the Second 
#' International Conference on Knowledge Discovery and Data Mining (KDD'96)},
#' pp. 267-270.
#' 
#' Jimenez, S., Becerra, C., Gelbukh, A., & Gonzalez, F. (2009), "Generalized 
#' Monge-Elkan Method for Approximate Text String Comparison", In 
#' \emph{Computational Linguistics and Intelligent Text Processing}, 
#' pp. 559-570.
#' 
#' @examples
#' ## Compare names with heterogenous representations
#' x <- "The University of California - San Diego"
#' y <- "Univ. Calif. San Diego"
#' # Tokenize strings on white space
#' x <- strsplit(x, '\\s+')
#' y <- strsplit(y, '\\s+')
#' MongeElkan()(x, y)
#' 
#' ## The symmetrized variant is arguably more appropriate for this example
#' MongeElkan(symmetrize = TRUE)(x, y) 
#' 
#' ## Using a different internal comparator changes the result
#' MongeElkan(inner_comparator = BinaryComp(), symmetrize=TRUE)(x, y)
#' 
#' @export
MongeElkan <- function(inner_comparator = Levenshtein(similarity = TRUE, normalize = TRUE), 
                       agg_function = base::mean, 
                       symmetrize = FALSE) {
  distance <- inner_comparator@distance
  symmetric <- inner_comparator@symmetric & symmetrize
  similarity <- inner_comparator@similarity
  inner_opt <- ifelse(inner_comparator@distance, base::min, base::max)
  arguments <- c(as.list(environment()))
  do.call("new", append("MongeElkan", arguments))
}

#' @describeIn elementwise Specialization for [`MongeElkan`] where `x` and `y` 
#' lists of token vectors to compare.
setMethod(elementwise, signature = c(comparator = "MongeElkan", x = "list", y = "list"), 
          function(comparator, x, y, ...) {
            if (length(x) < length(y)) {
              x <- rep_len(x, length(y))
            } else if (length(y) < length(x)) {
              y <- rep_len(y, length(x))
            }
            
            out <- vector(mode = "numeric", length = length(x))
            for (i in seq_along(out)) {
              out[i] <- .impl_inner.MongeElkan(x[[i]], y[[i]], 
                                               comparator@inner_comparator, comparator@inner_opt, 
                                               comparator@agg_function, comparator@symmetric)
            }
            out
          }
)


#' @describeIn pairwise Specialization for [`MongeElkan`] where `x` and `y` are 
#' lists of token vectors to compare.
setMethod(pairwise, signature = c(comparator = "MongeElkan", x = "list", y = "list"), 
          function(comparator, x, y, return_matrix, ...) {
            # preallocate matrix for output
            scores <- matrix(0.0, nrow = length(x), ncol = length(y))
            
            # loop over all combinations in input character vectors
            for (i in seq_len(length(x))) {
              for (j in seq_len(length(y))) {
                scores[i,j] <- .impl_inner.MongeElkan(x[[i]], y[[j]], 
                                                      comparator@inner_comparator, comparator@inner_opt, 
                                                      comparator@agg_function, comparator@symmetric)
              }
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`MongeElkan`] where `x` is a list 
#' of token vectors to compare among themselves.
setMethod(pairwise, signature = c(comparator = "MongeElkan", x = "list", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(comparator, x, x, return_matrix)
          }
)