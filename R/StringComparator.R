#' @include SequenceComparator.R
NULL

#' Virtual String Comparator Class 
#' 
#' @description Represents a comparator for pairs of strings.
#' 
#' @slot .Data a function that calls the elementwise method for this class, 
#'   with arguments `x`, `y` and `...`.
#' @slot symmetric a logical of length 1. If TRUE, the comparator is symmetric 
#'   in its arguments---i.e. `comparator(x, y)` is identical to 
#'   `comparator(y, x)`.
#' @slot distance a logical of length 1. If `TRUE`, the comparator produces  
#'   distances and satisfies `comparator(x, x) = 0`. The comparator may not 
#'   satisfy all of the properties of a distance metric.
#' @slot similarity a logical of length 1. If `TRUE`, the comparator produces 
#'   similarity scores.
#' @slot tri_inequal a logical of length 1. If `TRUE`, the comparator satisfies 
#'   the triangle inequality. This is only possible (but not guaranteed) if 
#'   `distance = TRUE` and `symmetric = TRUE`.
#' @slot ignore_case a logical of length 1. If TRUE, case is ignored when 
#'   comparing strings. Defaults to FALSE.
#' @slot use_bytes a logical of length 1. If TRUE, strings are compared
#'   byte-by-byte rather than character-by-character.
#' 
#' @export
setClass("StringComparator", 
         slots = c(
           ignore_case = "logical", 
           use_bytes = "logical"
         ), 
         prototype = structure(
           .Data = function(x, y, ...) 0,
           ignore_case = FALSE,
           use_bytes = FALSE
         ),
         contains = c("VIRTUAL", "SequenceComparator"), 
         validity = function(object) {
           errs <- character()
           if (length(object@ignore_case) != 1)
             errs <- c(errs, "`ignore_case` must be a logical vector of length 1")
           if (length(object@use_bytes) != 1) 
             errs <- c(errs, "`use_bytes` must be a logical vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' @describeIn elementwise Specialization for [`StringComparator-class`] where 
#' `x` and `y` are vectors of strings to compare.
setMethod(elementwise, signature = c(comparator = "StringComparator", x = "vector", y = "vector"), 
          function(comparator, x, y, ...) {
            xy = strings_to_code_vectors(x, y, ignore_case = comparator@ignore_case, 
                                         use_bytes = comparator@use_bytes)
            elementwise(comparator, xy$x, xy$y, ...)
          }
)

#' @describeIn pairwise Specialization for [`StringComparator-class`] where `x` 
#' and `y` are vectors of strings to compare.
setMethod(pairwise, signature = c(comparator = "StringComparator", x = "vector", y = "vector"), 
          function(comparator, x, y, return_matrix, ...) {
            xy = strings_to_code_vectors(x, y, ignore_case = comparator@ignore_case, 
                                         use_bytes = comparator@use_bytes)
            pairwise(comparator, xy$x, xy$y, return_matrix, ...)
          }
)

#' @describeIn pairwise Specialization for [`StringComparator-class`] where `x` 
#' is a vector of strings to compare.
setMethod(pairwise, signature = c(comparator = "StringComparator", x = "vector", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            xy = strings_to_code_vectors(x, y = NULL, ignore_case = comparator@ignore_case, 
                                         use_bytes = comparator@use_bytes)
            pairwise(comparator, xy$x, NULL, return_matrix, ...)
          }
)