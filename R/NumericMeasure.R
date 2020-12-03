#' @include Measure.R
NULL

#' Numeric Measure Class 
#' 
#' @description This class represents a measure for comparing pairs of 
#'   numeric vectors.
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
#' 
#' @export
NumericMeasure <- setClass("NumericMeasure", 
         prototype = structure(
           .Data = function(x, y) 0,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("Measure"), 
         validity = function(object) {
           errs <- character()
           ifelse(length(errs) == 0, TRUE, errs)
         })

# #' @describeIn elementwise Specialization for [`NumericMeasure`] where `x` and 
# #' `y` are matrices of rows (interpreted as vectors) to compare elementwise. 
# #' If one of the matrices has fewer rows than the other, the rows in the 
# #' smaller matrix are recycled following standard `R` behavior. 
# setMethod(elementwise, signature = c(measure = "NumericMeasure", x = "matrix", y = "matrix"), 
#           function(measure, x, y, ...) {
#             # Using the fact that the elementwise measure is stored in the .Data slot
#             measure(x, y)
#           }
# )

#' @describeIn pairwise Specialization for [`NumericMeasure`] where `x` is a 
#' matrix of rows (interpreted as vectors) to compare with a vector `y`. 
setMethod(pairwise, signature = c(measure = "NumericMeasure", x = "matrix", y = "vector"),
          function(measure, x, y, return_matrix, ...) {
            dim(y) <- c(1, length(y))
            pairwise(measure, x, y, return_matrix)
          }
)

#' @describeIn pairwise Specialization for [`NumericMeasure`] where `x` is a 
#' vector to compare with a matrix `y` of rows (interpreted as vectors).
setMethod(pairwise, signature = c(measure = "NumericMeasure", x = "vector", y = "matrix"),
          function(measure, x, y, return_matrix, ...) {
            dim(x) <- c(1, length(x))
            pairwise(measure, x, y, return_matrix)
          }
)