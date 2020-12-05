#' @include StringMeasure.R PairwiseMatrix.R CppMeasure.R
NULL

setClass("Constant", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(constant = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           constant = 0.0, 
           symmetric = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (length(object@constant) != 1)
             errs <- c(errs, "`constant` must be a numeric vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (object@distance)
             errs <- c(errs, "`distance` must be FALSE")
           if (object@similarity)
             errs <- c(errs, "`similarity` must be FALSE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Constant Measure
#' 
#' @description 
#' A trivial string measure that returns a constant for any pair of values.
#' 
#' @param constant a non-negative numeric vector of length 1. Defaults to zero.
#' 
#' @return 
#' A `Constant` instance is returned, which is an S4 class inheriting 
#' from [`StringMeasure-class`].
#' 
#' @export
Constant <- function(constant = 0.0) {
  arguments <- c(as.list(environment()))
  do.call("new", append("Constant", arguments))
}