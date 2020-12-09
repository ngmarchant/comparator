#' @include PairwiseMatrix.R Minkowski.R

setClass("Manhattan", contains = "Minkowski", 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...)
         ),
         validity = function(object) {
           errs <- character()
           if (object@p != 1)
             errs <- c(errs, "`p` must be 1 for Manhattan distance")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Manhattan Numeric Comparator
#' 
#' @description 
#' The Manhattan distance (a.k.a. L-1 distance) between two vectors \eqn{x} and 
#' \eqn{y} is the sum of the absolute differences of their Cartesian 
#' coordinates: 
#' \deqn{\mathrm{Manhattan}(x,y) = \sum_{i = 1}^{n} |x_i - y_i|.}{Manhattan(x,y) = sum_i { |x_i - y_i| }.}
#' 
#' @note The Manhattan distance is a special case of the [`Minkowski`] 
#' distance with \eqn{p = 1}.
#' 
#' @return 
#' A `Manhattan` instance is returned, which is an S4 class inheriting 
#' from [`Minkowski`].
#' 
#' @seealso 
#' Other numeric comparators include [`Euclidean`], [`Minkowski`] and 
#' [`Chebyshev`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Manhattan()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' comparator <- Manhattan()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(comparator, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(comparator, x, y)
#' 
#' @export
Manhattan <- function() {
  arguments <- list(p = 1)
  do.call("new", append("Manhattan", arguments))
}