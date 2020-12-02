#' @include PairwiseMatrix.R Minkowski.R

attrs <- attributes(getClassDef("Minkowski")@prototype)[-1]

setClass("Euclidean", contains = "Minkowski", 
         prototype = structure(
           .Data = elementwise_minkowski_builder(attrs)
         ),
         validity = function(object) {
           errs <- character()
           if (object@p != 2)
             errs <- c(errs, "`p` must be 2 for Euclidean distance")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Euclidean Distance
#' 
#' @description 
#' The Euclidean distance (a.k.a. L-2 distance) between two vectors \eqn{x} and 
#' \eqn{y} is the square root of the sum of the squared differences of the
#' Cartesian coordinates: 
#' \deqn{Euclidean(x, y) = \sqrt(\sum_{i = 1}^{n} (x_i - q_i)^2.}
#' 
#' @note The Euclidean distance is a special case of the [`Minkowski`] 
#' distance with \eqn{p = 2}.
#' 
#' @return 
#' A `Euclidean` instance is returned, which is an S4 class inheriting 
#' from [`NumericMeasure-class`].
#' 
#' @seealso 
#' Other numeric measures include [`Manhattan`], [`Minkowski`] and 
#' [`Chebyshev`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Euclidean()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' measure <- Euclidean()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(measure, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(measure, x, y)
#' 
#' @export
Euclidean <- function(...) {
  attrs <- list(p = 2)
  arguments <- list("Euclidean", ".Data" = elementwise_minkowski_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}