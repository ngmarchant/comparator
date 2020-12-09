#' @include PairwiseMatrix.R Minkowski.R

setClass("Euclidean", contains = "Minkowski", 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...)
         ),
         validity = function(object) {
           errs <- character()
           if (object@p != 2)
             errs <- c(errs, "`p` must be 2 for Euclidean distance")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Euclidean Numeric Comparator
#' 
#' @description 
#' The Euclidean distance (a.k.a. L-2 distance) between two vectors \eqn{x} and 
#' \eqn{y} is the square root of the sum of the squared differences of the
#' Cartesian coordinates: 
#' \deqn{\mathrm{Euclidean}(x, y) = \sqrt{\sum_{i = 1}^{n} (x_i - y_i)^2}.}{Euclidean(x, y) = sqrt(sum_i { (x_i - y_i)^2 })}
#' 
#' @note The Euclidean distance is a special case of the [`Minkowski`] 
#' distance with \eqn{p = 2}.
#' 
#' @return 
#' A `Euclidean` instance is returned, which is an S4 class inheriting 
#' from [`Minkowski`].
#' 
#' @seealso 
#' Other numeric comparators include [`Manhattan`], [`Minkowski`] and 
#' [`Chebyshev`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Euclidean()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' comparator <- Euclidean()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(comparator, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(comparator, x, y)
#' 
#' @export
Euclidean <- function() {
  arguments <- list(p = 2)
  do.call("new", append("Euclidean", arguments))
}