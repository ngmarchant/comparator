#' @include StringComparator.R Levenshtein.R PairwiseMatrix.R
NULL

#' @importFrom clue solve_LSAP
#' @noRd
.impl_inner.OptimalMatch <- function(x, y, inner_comparator, agg_function,
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
  
  scores <- pairwise(inner_comparator, x, y)
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

setClass("FuzzyTokenSet", contains = "StringComparator", 
         slots = c(inner_comparator = "StringComparator",
                   agg_function = "function", 
                   deletion = "numeric", 
                   insertion = "numeric", 
                   substitution = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           inner_comparator = Levenshtein(),
           agg_function = base::mean,
           deletion = 1.0, 
           insertion = 1.0,
           substitution = 1.0,
           symmetric = TRUE,
           distance = TRUE,
           ordered = FALSE
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
           if (!object@symmetric & symmetric_weights & object@inner_comparator@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE when `deletion==insertion` and `inner_comparator` is symmetric")
           if (object@symmetric & (!symmetric_weights | !object@inner_comparator@symmetric))
           errs <- c(errs, "`symmetric` must be FALSE when `deletion!=insertion` or `inner_comparator` is not symmetric")
           
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           if (object@ordered)
             errs <- c(errs, "`ordered` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Fuzzy Token Set Comparator
#' 
#' @description 
#' Compares a pair of token sets \eqn{x} and \eqn{y} by computing the 
#' optimal cost of transforming \eqn{x} into \eqn{y} using single-token 
#' operations (insertions, deletions and substitutions). The cost of  
#' single-token operations is determined at the character-level using an 
#' internal string comparator.
#' 
#' @details
#' A token set is an unordered enumeration of tokens, which may include 
#' duplicates. Given two token sets \eqn{x} and \eqn{y}, this comparator 
#' computes the optimal cost of transforming \eqn{x} into \eqn{y} using the 
#' following single-token operations:
#' 
#' * deleting a token \eqn{a} from \eqn{x} at cost \eqn{w_d \times \mathrm{inner}(a, "")}{w_d * inner(a, "")}
#' * inserting a token \eqn{b} in \eqn{y} at cost \eqn{w_i \times \mathrm{inner}("", b)}{w_i * inner("", b)}
#' * substituting a token \eqn{a} in \eqn{x} for a token \eqn{b} 
#'   in \eqn{y} at cost \eqn{w_s \times \mathrm{inner}(a, b)}{w_s * inner(a, b)}
#' 
#' where \eqn{\mathrm{inner}}{inner} is an internal string comparator and 
#' \eqn{w_d, w_i, w_s} are non-negative weights, referred to as `deletion`, 
#' `insertion` and `substitution` in the parameter list. By default, the 
#' _mean_ cost of the optimal set of operations is returned. Other methods of 
#' aggregating the costs are supported by specifying a non-default 
#' `agg_function`.
#' 
#' If the internal string comparator is a _distance_ function, then the optimal 
#' set of operations _minimize_ the cost. Otherwise, the optimal set of 
#' operations _maximize_ the cost. The optimization problem is solved exactly 
#' using a linear sum assignment solver. 
#' 
#' @note This comparator is qualitatively similar to the [`MongeElkan`] 
#'   comparator, however it is arguably more principled, since it is formulated 
#'   as a cost optimization problem. It also offers more control over the costs 
#'   of missing tokens (by varying the `deletion` and `insertion` weights). 
#'   This is useful for comparing full names, when dropping a name (e.g. 
#'   middle name) shouldn't be severely penalized.
#' 
#' @param inner_comparator inner string distance comparator of class 
#'   [`StringComparator-class`]. Defaults to normalized [`Levenshtein`] 
#'   distance.
#' @param agg_function function used to aggregate the costs of the optimal 
#'   operations. Defaults to [`base::mean`].
#' @param deletion non-negative weight associated with deletion of a token. 
#'   Defaults to 1.
#' @param insertion non-negative weight associated insertion of a token.
#'   Defaults to 1.
#' @param substitution non-negative weight associated with substitution of a 
#'   token. Defaults to 1.
#' 
#' @examples
#' ## Compare names with heterogenous representations
#' x <- "The University of California - San Diego"
#' y <- "Univ. Calif. San Diego"
#' # Tokenize strings on white space
#' x <- strsplit(x, '\\s+')
#' y <- strsplit(y, '\\s+')
#' FuzzyTokenSet()(x, y)
#' # Reduce the cost associated with missing words
#' FuzzyTokenSet(deletion = 0.5, insertion = 0.5)(x, y)
#' 
#' ## Compare full name with abbreviated name, reducing the penalty 
#' ## for dropping parts of the name
#' fullname <- "JOSE ELIAS TEJADA BASQUES"
#' name <- "JOSE BASQUES"
#' # Tokenize strings on white space
#' fullname <- strsplit(fullname, '\\s+')
#' name <- strsplit(name, '\\s+')
#' comparator <- FuzzyTokenSet(deletion = 0.5)
#' comparator(fullname, name) < comparator(name, fullname) # TRUE
#' 
#' @export
FuzzyTokenSet <- function(inner_comparator = Levenshtein(normalize = TRUE), 
                          agg_function = base::mean, deletion = 1, 
                          insertion = 1, substitution = 1) {
  distance <- inner_comparator@distance
  similarity <- inner_comparator@similarity
  symmetric <- inner_comparator@symmetric & deletion == insertion
  arguments <- c(as.list(environment()))
  do.call("new", append("FuzzyTokenSet", arguments))
}

#' @describeIn elementwise Specialization for [`FuzzyTokenSet`] where `x` and `y` 
#' are lists of token vectors to compare.
setMethod(elementwise, signature = c(comparator = "FuzzyTokenSet", x = "list", y = "list"), 
          function(comparator, x, y, ...) {
            if (length(x) < length(y)) {
              x <- rep_len(x, length(y))
            } else if (length(y) < length(x)) {
              y <- rep_len(y, length(x))
            }
            
            # Pre-allocate score vector for output
            out <- vector(mode = "numeric", length = length(x))
            for (i in seq_along(out)) {
              out[i] <- .impl_inner.OptimalMatch(x[[i]], y[[i]], 
                                                 comparator@inner_comparator, comparator@agg_function, 
                                                 !comparator@distance, comparator@insertion, 
                                                 comparator@deletion, comparator@substitution)
            }
            
            out
          }
)

#' @describeIn pairwise Specialization for [`FuzzyTokenSet`] where `x` and `y` are 
#' lists of token vectors to compare.
setMethod(pairwise, signature = c(comparator = "FuzzyTokenSet", x = "list", y = "list"), 
          function(comparator, x, y, return_matrix, ...) {
            # Pre-allocate pairwise score matrix for output
            scores <- matrix(0.0, nrow = length(x), ncol = length(y))
            
            # Loop over all combinations in input character vectors
            for (i in seq_len(length(x))) {
              for (j in seq_len(length(y))) {
                scores[i,j] <- .impl_inner.OptimalMatch(x[[i]], y[[j]], 
                                                        comparator@inner_comparator, comparator@agg_function, 
                                                        !comparator@distance, comparator@insertion, 
                                                        comparator@deletion, comparator@substitution)
              }
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`FuzzyTokenSet`] where `x` is a list of token 
#' vectors to compare among themselves.
setMethod(pairwise, signature = c(comparator = "FuzzyTokenSet", x = "vector", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(comparator, x, x, return_matrix)
          }
)