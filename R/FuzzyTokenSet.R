#' @include StringMeasure.R Levenshtein.R PairwiseMatrix.R
NULL

#' @importFrom clue solve_LSAP
#' @noRd
.impl_inner.OptimalMatch <- function(x, y, inner_measure, agg_function,
                                     maximize, insertion, deletion, substitution) {
  seq_x <- seq_along(x)
  seq_y <- seq_along(y)
  nx <- length(x)
  ny <- length(y)
  
  # Add empty tokens to allow for insertion/deletion operations
  ins_id <- seq(from=nx+1, length.out = ny)
  del_id <- seq(from=ny+1, length.out = nx)
  x[ins_id] <- ""
  y[del_id] <- ""
  
  scores <- pairwise(inner_measure, x, y)
  scores <- as.matrix(scores)
  
  # Apply weights
  scores[seq_x,del_id] <- deletion * scores[seq_x,del_id]
  scores[ins_id,seq_y] <- insertion * scores[ins_id,seq_y]
  scores[seq_x,seq_y] <- substitution * scores[seq_x,seq_y]
  scores[ins_id,del_id] <- substitution * scores[ins_id,del_id]
  
  # Guard against passing NA scores to LSAP function.
  if (anyNA(scores)) return(NA)
  assignments <- solve_LSAP(scores, maximum = maximize)
  
  # Extract scores for operations that involved the original tokens only
  sel_x <- union(seq_x, which(assignments <= ny))
  sel_y <- assignments[sel_x]
  scores <- scores[cbind(sel_x, sel_y)]
  agg_function(scores)
}

setClass("FuzzyTokenSet", contains = "StringMeasure", 
         slots = c(separator = "character",
                   inner_measure = "StringMeasure",
                   agg_function = "function", 
                   deletion = "numeric", 
                   insertion = "numeric", 
                   substitution = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           separator = "\\s+",
           inner_measure = Levenshtein(),
           agg_function = base::mean,
           deletion = 1.0, 
           insertion = 1.0,
           substitution = 1.0,
           symmetric = TRUE,
           distance = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (object@deletion < 0 | length(object@deletion) != 1)
             errs <- c(errs, "`deletion` must be a non-negative numeric vector of length 1")
           if (object@insertion < 0 | length(object@insertion) != 1)
             errs <- c(errs, "`insertion` must be a non-negative numeric vector of length 1")
           if (object@substitution < 0 | length(object@substitution) != 1)
             errs <- c(errs, "`substitution` must be a non-negative numeric vector of length 1")
           
           symmetric_weights <- object@deletion == object@insertion
           if (!object@symmetric & symmetric_weights & object@inner_measure@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE when `deletion==insertion` and `inner_measure` is symmetric")
           if (object@symmetric & (!symmetric_weights | !object@inner_measure@symmetric))
           errs <- c(errs, "`symmetric` must be FALSE when `deletion!=insertion` or `inner_measure` is not symmetric")
           
           if (length(object@separator) != 1)
             errs <- c(errs, "`separator` must be a character vector of length 1")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Fuzzy Token Set Distance
#' 
#' @description 
#' This hybrid token-character measure is suitable for comparing a pair of 
#' multi-token (multi-word) strings \eqn{x} and \eqn{y}. It measures the 
#' minimum cost of transforming the token set representation of \eqn{x} into 
#' the token set representation of \eqn{y} using single-token operations 
#' (insertions, deletions and substitutions). The cost of the operations are 
#' determined using an internal user-specified distance measure.
#' 
#' @details
#' This measure compares the input strings \eqn{x} and \eqn{y} using a token 
#' set representation. A token set is an unordered enumeration of tokens, 
#' which may include duplicates. For example, the token set representation 
#' of the string "Paw Paw Illinois" is {"Illinois", "Paw", "Paw"}. After 
#' mapping \eqn{x} and \eqn{y} to token sets, the minimum cost of transforming 
#' \eqn{x} into \eqn{y} is computed based on three single-token operations: 
#' insertions, deletions and substitutions. The costs of the operations are 
#' specified as follows:
#' 
#' * cost of deleting a token \eqn{a} from \eqn{x}: \eqn{w_d \times \mathrm{inner}(a, "")}{w_d * inner(a, "")}
#' * cost of inserting a token \eqn{b} in \eqn{y}: \eqn{w_i \times \mathrm{inner}("", b)}{w_i * inner("", b)}
#' * cost of substituting a token \eqn{a} in \eqn{x} for a token \eqn{b} 
#' in \eqn{y}: \eqn{w_s \times \mathrm{inner}(a, b)}{w_s * inner(a, b)}
#' 
#' where \eqn{\mathrm{inner}}{inner} is a user-specified internal string 
#' measure and \eqn{w_d, w_i, w_s} are user-specified weights, referred to as 
#' `deletion`, `insertion` and `substitution` in the parameter list. By 
#' default, the mean cost of the optimal (cost-minimizing) set of operations is 
#' returned as a measure of the distance between \eqn{x} and \eqn{y}. Other 
#' methods of aggregating the costs are supported by specifying a non-default 
#' `agg_function`.
#' 
#' The optimization problem---of finding the minimum total cost under the 
#' allowed operations---is solved exactly using a linear sum assignment 
#' solver. 
#' 
#' @note This measure is qualitatively similar to the [`MongeElkan`] measure, 
#'   however it is arguably more principled, since it is formulated as a 
#'   cost minimization problem. It also offers more control over the costs 
#'   of missing tokens (by varying the `deletion` and `insertion` weights). 
#'   This is useful for comparing full names, where dropping a name (e.g. 
#'   middle name) should not be severely penalized.
#' 
#' @param inner_measure inner string distance measure of class 
#'   [`StringMeasure-class`]. Defaults to normalized [`Levenshtein`] distance.
#' @param separator separator for tokens/words. Defaults to whitespace.
#' @param agg_function function used to aggregate the costs of the optimal 
#'   operations. Defaults to [`base::mean`].
#' @param deletion positive weight associated with deletion of a token. 
#'   Defaults to unit cost.
#' @param insertion positive weight associated insertion of a token.
#'   Defaults to unit cost.
#' @param substitution positive weight associated with substitution of a 
#'   token. Defaults to unit cost.
#' 
#' @examples
#' ## Compare names with heterogenous representations
#' x <- "The University of California - San Diego"
#' y <- "Univ. Calif. San Diego"
#' FuzzyTokenSet()(x, y)
#' 
#' # Reduce the cost associated with missing words
#' FuzzyTokenSet(deletion = 0.5, insertion = 0.5)(x, y)
#' 
#' ## Compare full names, recognizing that it is common to omit names
#' x <- "JOSE ELIAS TEJADA BASQUES"
#' y <- "JOSE BASQUES"
#' FuzzyTokenSet(deletion = 0.5, insertion = 0.5)(x, y)
#' 
#' @export
FuzzyTokenSet <- function(inner_measure = Levenshtein(normalize = TRUE), 
                          separator = "\\s+", agg_function = base::mean, 
                          deletion = 1, insertion = 1, substitution = 1) {
  distance <- inner_measure@distance
  similarity <- inner_measure@similarity
  symmetric <- inner_measure@symmetric & deletion == insertion
  arguments <- c(as.list(environment()))
  do.call("new", append("FuzzyTokenSet", arguments))
}

#' @describeIn elementwise Specialization for [`FuzzyTokenSet`] where `x` and `y` 
#' are vectors of strings to compare.
setMethod(elementwise, signature = c(measure = "FuzzyTokenSet", x = "vector", y = "vector"), 
          function(measure, x, y, ...) {
            # tokenize
            tokens1 <- strsplit(x, measure@separator)
            tokens2 <- strsplit(y, measure@separator)
            
            # TODO: wrong. Want proper recycling
            if (length(tokens1) < length(tokens2)) {
              tokens1 <- rep(tokens1, times = length(tokens2))
            } else if (length(tokens2) < length(tokens1)) {
              tokens2 <- rep(tokens2, times = length(tokens1))
            }
            
            # Pre-allocate score vector for output
            out <- vector(mode = "numeric", length = length(tokens1))
            for (i in seq_along(tokens1)) {
              out[i] <- .impl_inner.OptimalMatch(tokens1[[i]], tokens2[[i]], 
                                                 measure@inner_measure, measure@agg_function, 
                                                 !measure@distance, measure@insertion, 
                                                 measure@deletion, measure@substitution)
            }
            
            out
          }
)

#' @describeIn pairwise Specialization for [`FuzzyTokenSet`] where `x` and `y` are 
#' vectors of strings to compare.
setMethod(pairwise, signature = c(measure = "FuzzyTokenSet", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            # tokenize
            tokens1 <- strsplit(x, measure@separator)
            tokens2 <- strsplit(y, measure@separator)
            
            if (length(tokens1) < length(tokens2)) {
              tokens1 <- rep(tokens1, times = length(tokens2))
            } else if (length(tokens2) < length(tokens1)) {
              tokens2 <- rep(tokens2, times = length(tokens1))
            }
            
            # Pre-allocate pairwise score matrix for output
            scores <- matrix(0.0, nrow = length(tokens1), ncol = length(tokens2))
            
            # Loop over all combinations in input character vectors
            for (i in seq_len(length(tokens1))) {
              for (j in seq_len(length(tokens2))) {
                scores[i,j] <- .impl_inner.OptimalMatch(tokens1[[i]], tokens2[[j]], 
                                                        measure@inner_measure, measure@agg_function, 
                                                        !measure@distance, measure@insertion, 
                                                        measure@deletion, measure@substitution)
              }
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`FuzzyTokenSet`] where `x` is a vector 
#' of strings to compare among themselves.
setMethod(pairwise, signature = c(measure = "FuzzyTokenSet", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)