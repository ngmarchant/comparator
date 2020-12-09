#' @include Comparator.R
NULL

#' Virtual Numeric Comparator Class 
#' 
#' @description Represents a comparator for comparing pairs of numeric vectors.
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
#' 
#' @export
setClass("NumericComparator",
         contains = c("VIRTUAL", "Comparator"))

#' @describeIn elementwise Specialization for [`NumericComparator-class`] where 
#' `x` is a matrix of rows (interpreted as vectors) to compare with a vector 
#' `y`. 
setMethod(elementwise, signature = c(comparator = "NumericComparator", x = "matrix", y = "vector"),
          function(comparator, x, y, ...) {
            dim(y) <- c(1, length(y))
            elementwise(comparator, x, y, ...)
          }
)

#' @describeIn elementwise Specialization for [`NumericComparator-class`] where 
#' `x` is a vector to compare with a matrix `y` of rows (interpreted as 
#' vectors).
setMethod(elementwise, signature = c(comparator = "NumericComparator", x = "vector", y = "matrix"),
          function(comparator, x, y, ...) {
            dim(x) <- c(1, length(x))
            elementwise(comparator, x, y, ...)
          }
)

#' @describeIn elementwise Specialization for [`NumericComparator-class`] where 
#' `x` and `y` are vectors to compare.
setMethod(elementwise, signature = c(comparator = "NumericComparator", x = "vector", y = "vector"),
          function(comparator, x, y, ...) {
            dim(x) <- c(1, length(x))
            dim(y) <- c(1, length(y))
            elementwise(comparator, x, y, ...)
          }
)

#' @describeIn pairwise Specialization for [`NumericComparator-class`] where `x` 
#' is a matrix of rows (interpreted as vectors) to compare with a vector `y`. 
setMethod(pairwise, signature = c(comparator = "NumericComparator", x = "matrix", y = "vector"),
          function(comparator, x, y, return_matrix, ...) {
            dim(y) <- c(1, length(y))
            pairwise(comparator, x, y, return_matrix)
          }
)

#' @describeIn pairwise Specialization for [`NumericComparator-class`] where `x` 
#' is a vector to compare with a matrix `y` of rows (interpreted as vectors).
setMethod(pairwise, signature = c(comparator = "NumericComparator", x = "vector", y = "matrix"),
          function(comparator, x, y, return_matrix, ...) {
            dim(x) <- c(1, length(x))
            pairwise(comparator, x, y, return_matrix)
          }
)