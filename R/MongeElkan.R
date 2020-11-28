#' @include abstract.R Levenshtein.R
NULL

def_attr_me <- list(
  separator = "\\s+",
  inner_measure = Levenshtein(similarity = TRUE),
  agg_function = base::mean,
  inner_opt = base::max, 
  symmetrize = FALSE,
  symmetric = FALSE,
  similarity = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_me)] <- def_attr_me

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

elementwise_me_builder <- function(attrs) {
  function(x, y) {
    # tokenize
    tokens1 <- strsplit(x, attrs$separator)
    tokens2 <- strsplit(y, attrs$separator)
    
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
                                       attrs$inner_measure, attrs$inner_opt, 
                                       attrs$agg_function, attrs$symmetric)
    }
    
    out
  }
}

setClass("MongeElkan", contains = "StringMeasure", 
         slots = c(separator = "character",
                   inner_measure = "StringMeasure",
                   agg_function = "function", 
                   inner_opt = "function", 
                   symmetrize = "logical"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_me_builder(def_attr_me)), attrs)), 
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
#' TODO A hybrid string measure. Token- and character-based. 
#' 
#' @details
#' TODO
#' \deqn{\mathrm{sim_MongeElkan}(v_1, v_2) = \frac{1}{|v_1|} \sum_{i = 1}^{|v_1|} \max_{j} \mathrm{sim.inner}(v_{1,i}, v_{2,j})}
#' {sim_MongeElkan(v1,v2) = 1/|v1| Σ_i{ max_j sim.inner(v1_i, v2_j) }}
#' 
#' @param inner_measure inner string measure of class [`StringMeasure-class`]. 
#'   Defaults to [`Levenshtein`] similarity.
#' @param separator separator for tokens/words. Defaults to whitespace.
#' @param agg_function aggregation function to use when aggregating inner 
#'   similarities/distances between tokens. Defaults to arithmetic mean. 
#'   Harmonic mean may be a better choice for normalized similarity measures.
#' @param symmetrize logical indicating whether to use a symmetrized version 
#'   of the Monge-Elkan measure. Defaults to FALSE.
#'   
#' @references Monge, A. E., & Elkan, C. (1996, August). The Field Matching 
#' Problem: Algorithms and Applications. In \emph{KDD} (Vol. 2, pp. 267-270).
#' 
#' @export
MongeElkan <- function(inner_measure = Levenshtein(similarity = TRUE), 
                       separator = "\\s+", agg_function = base::mean, 
                       symmetrize = TRUE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$distance <- inner_measure@distance
  attrs$symmetric <- inner_measure@symmetric && symmetrize
  attrs$distance <- inner_measure@distance
  attrs$similarity <- inner_measure@similarity
  if (inner_measure@distance) {
    attrs$inner_opt <- base::min
  } else {
    attrs$inner_opt <- base::max
  }
  arguments <- list("MongeElkan", ".Data" = elementwise_me_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
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

#' @export
setMethod(pairwise, signature = c(measure = "MongeElkan", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)