#' @include Measure.R
NULL

#' Virtual String Measure Class 
#' 
#' @description This class represents a measure for comparing pairs of strings.
#' 
#' @slot .Data a function which takes a pair of arguments `x` and `y`, and 
#'   returns the elementwise measure scores.
#' @slot symmetric a logical of length 1. If TRUE, the measure is symmetric 
#'   in its arguments---i.e. `measure(x, y)` is identical to `measure(y, x)`.
#' @slot distance a logical of length 1. If `TRUE`, the measure produces  
#'   distances and satisfies `measure(x, x) = 0`. The measure may not satisfy 
#'   all of the properties of a distance metric.
#' @slot similarity a logical of length 1. If `TRUE`, the measure produces 
#'   similarity scores.
#' @slot tri_inequal a logical of length 1. If `TRUE`, the measure satisfies 
#'   the triangle inequality. This is only possible (but not guaranteed) if 
#'   `distance = TRUE` and `symmetric = TRUE`.
#' @slot ignore_case a logical of length 1. If TRUE, case is ignored when 
#'   comparing strings. Defaults to FALSE.
#' @slot use_bytes a logical of length 1. If TRUE, strings are compared
#'   byte-by-byte rather than character-by-character.
#' 
#' @export
StringMeasure <- setClass("StringMeasure", 
         slots = c(
           ignore_case = "logical", 
           use_bytes = "logical"
         ), 
         prototype = structure(
           .Data = function(x, y) 0,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE,
           ignore_case = FALSE,
           use_bytes = FALSE
         ),
         contains = c("VIRTUAL", "Measure"), 
         validity = function(object) {
           errs <- character()
           if (length(object@ignore_case) != 1)
             errs <- c(errs, "`ignore_case` must be a logical vector of length 1")
           if (length(object@use_bytes) != 1) 
             errs <- c(errs, "`use_bytes` must be a logical vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' @describeIn pairwise Specialization for [`StringMeasure`] where `x` and `y` are vectors of strings to compare
setMethod(pairwise, signature = c(measure = "StringMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            scores <- matrix(0.0, nrow = length(x), ncol = length(y))
            for (i in seq_along(x)) {
              scores[i,] <- measure(x[i], y)
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)