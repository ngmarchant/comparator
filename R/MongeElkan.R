#' @include StringMeasure.R Levenshtein.R
NULL

# Shared implementation for scoring of a single pair of values for 
# sim_MongeElkan and dist_MongeElkan
.impl_inner.MongeElkan <- function(t1, t2, inner_measure, inner_opt, agg_function, 
                                   symmetrize) {
  scores <- pairwise(inner_measure, t1, t2)
  scores <- as.matrix(scores)
  optScores1 <- apply(scores, 1, inner_opt)
  if (symmetrize) {
    optScores2 <- apply(scores, 2, inner_opt)
    return(inner_opt(agg_function(optScores1), agg_function(optScores2)))
  } else {
    return(agg_function(optScores1))
  }
}

setClass("MongeElkan", contains = "StringMeasure", 
         slots = c(separator = "character",
                   inner_measure = "StringMeasure",
                   agg_function = "function", 
                   inner_opt = "function", 
                   symmetrize = "logical"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           separator = "\\s+",
           inner_measure = Levenshtein(similarity = TRUE),
           agg_function = base::mean,
           inner_opt = base::max, 
           symmetrize = FALSE,
           symmetric = FALSE,
           similarity = TRUE
         ), 
         validity = function(object) {
           errs <- character()
           if (length(object@separator) != 1)
             errs <- c(errs, "`separator` must be a character vector of length 1")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Monge-Elkan Measure
#' 
#' @description 
#' The Monge-Elkan measure is a hybrid token-character measure for comparing 
#' a pair of multi-token (multi-word) strings \eqn{x} and \eqn{y}. It uses an 
#' internal character-based measure to compute similarity scores between pairs 
#' of tokens in \eqn{x} and \eqn{y}, then takes the average of the maximum 
#' score for each token in \eqn{x}.
#' 
#' @details
#' Given two token vectors \eqn{x} and \eqn{y}, the Monge-Elkan measure is 
#' defined as:
#' \deqn{\mathrm{ME}(x, y) = \frac{1}{|x|} \sum_{i = 1}^{|x|} \max_j \mathrm{sim}(x_i, y_j)}{ME(x, y) = 1/|x| Σ_i{ max_j sim(x_i, y_j) }}
#' where \eqn{x_i} is the i-th token (string) in \eqn{x}, \eqn{|x|} is the 
#' number of tokens in \eqn{x} and \eqn{\mathrm{sim}}{sim} is an internal 
#' character-based similarity measure. 
#' 
#' A generalization of the original Monge-Elkan measure is implemented here, 
#' which allows for internal distance measures in place of similarity measures, 
#' and/or more general aggregation functions in place of the arithmetic mean. 
#' The generalized Monge-Elkan measure is defined as:
#' \deqn{\mathrm{ME}(x, y) = \mathrm{agg}(\mathrm{opt}_j \mathrm{inner}(x_i, y_j))}{ME(x, y) = agg(opt_j inner(x_i, y_j))}
#' where \eqn{\mathrm{inner}}{inner} is an internal distance or similarity 
#' measure, \eqn{\mathrm{opt}} is \eqn{\max}{max} if 
#' \eqn{\mathrm{inner}}{inner} is a similarity measure or \eqn{\min}{min} if 
#' it is a distance measure, and \eqn{\mathrm{agg}}{agg} is an aggregation 
#' function which takes a vector of scores for each token in \eqn{x} and 
#' returns a scalar.
#' 
#' By default, the Monge-Elkan measure is asymmetric in its arguments \eqn{x} 
#' and \eqn{y}. If `symmetrize = TRUE`, a symmetric version of the measure 
#' is obtained as follows
#' \deqn{\mathrm{ME}_sym(x, y) = \mathrm{opt} (\mathrm{ME}(x, y), \mathrm{ME}(y, x))}{ME_sym(x, y) = opt(ME(x, y), ME(y, x))}
#' where \eqn{\mathrm{opt}} is defined above.
#' 
#' @param inner_measure internal string measure of class 
#'   [`StringMeasure-class`]. Defaults to [`Levenshtein`] similarity.
#' @param separator separator for tokens/words. Defaults to whitespace.
#' @param agg_function aggregation function to use when aggregating internal 
#'   similarities/distances between tokens. Defaults to [`mean`], 
#'   however [`hmean`] may be a better choice for normalized similarity 
#'   measures.
#' @param symmetrize logical indicating whether to use a symmetrized version 
#'   of the Monge-Elkan measure. Defaults to FALSE.
#' 
#' @return
#' A `MongeElkan` instance is returned, which is an S4 class inheriting from 
#' [`StringMeasure-class`].
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
#' MongeElkan()(x, y)
#' 
#' ## The symmetrized variant is arguably more appropriate for this example
#' MongeElkan(symmetrize = TRUE)(x, y) 
#' 
#' ## Using a different internal measure changes the result
#' MongeElkan(inner_measure = BinaryComp(), symmetrize=TRUE)(x, y)
#' 
#' @export
MongeElkan <- function(inner_measure = Levenshtein(similarity = TRUE, normalize = TRUE), 
                       separator = "\\s+", agg_function = base::mean, 
                       symmetrize = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$distance <- inner_measure@distance
  arguments$symmetric <- inner_measure@symmetric && symmetrize
  arguments$distance <- inner_measure@distance
  arguments$similarity <- inner_measure@similarity
  if (inner_measure@distance) {
    arguments$inner_opt <- base::min
  } else {
    arguments$inner_opt <- base::max
  }
  do.call("new", append("MongeElkan", arguments))
}

#' @describeIn elementwise Specialization for [`MongeElkan`] where `x` and `y` 
#' are vectors of strings to compare.
setMethod(elementwise, signature = c(measure = "MongeElkan", x = "vector", y = "vector"), 
          function(measure, x, y, ...) {
            # tokenize
            tokens1 <- strsplit(x, measure@separator)
            tokens2 <- strsplit(y, measure@separator)
            
            n <- max(length(tokens1), length(tokens2))
            
            # TODO: wrong. Want proper recycling
            if (length(tokens1) < length(tokens2)) {
              tokens1 <- rep(tokens1, times = length(tokens2))
            } else if (length(tokens2) < length(tokens1)) {
              tokens2 <- rep(tokens2, times = length(tokens1))
            }
            
            out <- vector(mode = "numeric", length = n)
            for (i in seq_len(n)) {
              out[i] <- .impl_inner.MongeElkan(tokens1[[i]], tokens2[[i]], 
                                               measure@inner_measure, measure@inner_opt, 
                                               measure@agg_function, measure@symmetric)
            }
            out
          }
)


#' @describeIn pairwise Specialization for [`MongeElkan`] where `x` and `y` are 
#' vectors of strings to compare.
setMethod(pairwise, signature = c(measure = "MongeElkan", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            # Tokenize
            tokens1 <- strsplit(x, measure@separator)
            tokens2 <- strsplit(y, measure@separator)
            
            # preallocate matrix for output
            scores <- matrix(0.0, nrow = length(tokens1), ncol = length(tokens2))
            
            # loop over all combinations in input character vectors
            for (i in seq_len(length(tokens1))) {
              for (j in seq_len(length(tokens2))) {
                scores[i,j] <- .impl_inner.MongeElkan(tokens1[[i]], tokens2[[j]], 
                                                      measure@inner_measure, measure@inner_opt, 
                                                      measure@agg_function, measure@symmetric)
              }
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`MongeElkan`] where `x` is a vector 
#' of strings to compare among themselves.
setMethod(pairwise, signature = c(measure = "MongeElkan", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)