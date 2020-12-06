#' @include NumericMeasure.R PairwiseMatrix.R

setClass("Chebyshev", contains = "NumericMeasure", 
         slots = c(p = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           symmetric = TRUE,
           distance = TRUE, 
           tri_inequal = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!object@distance)
             errs <- c(errs, "`distance` must be TRUE")
           if (object@similarity)
             errs <- c(errs, "`similarity` must be FALSE")
           if (!object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Chebyshev Distance
#' 
#' @description 
#' The Chebyshev distance (a.k.a. L-Inf distance or ) between two vectors 
#' \eqn{x} and \eqn{y} is the greatest of the absolute differences between each 
#' coordinate: 
#' \deqn{\mathrm{Chebyshev}(x,y) = \max_i |x_i - y_i|.}{Chebyshev(x, y) = max_i |x_i - y_i|.}
#' 
#' @return 
#' A `Chebyshev` instance is returned, which is an S4 class inheriting 
#' from [`NumericMeasure-class`].
#' 
#' @note The Chebyshev distance is a limiting case of the [`Minkowski`] 
#' distance where \eqn{p \to \infty}{p -> Inf}.
#' 
#' @seealso 
#' Other numeric measures include [`Manhattan`], [`Euclidean`] and 
#' [`Minkowski`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Chebyshev()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' measure <- Chebyshev()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(measure, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(measure, x, y)
#' 
#' @export
Chebyshev <- function() {
  arguments <- c(as.list(environment()))
  do.call("new", append("Chebyshev", arguments))
}

#' @importFrom proxy dist
#' @describeIn elementwise Specialization for [`Chebyshev`] where `x` and `y` 
#' matrices of rows (interpreted as vectors) to compare. If `x` any `y` do 
#' not have the same number of rows, rows are recycled in the smaller matrix. 
setMethod(elementwise, signature = c(measure = "Chebyshev", x = "matrix", y = "matrix"),
          function(measure, x, y, ...) {
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            # recycle as needed if one matrix has fewer rows than the other
            if (nrow(x) < nrow(y)) {
              x <- x[rep_len(seq_len(nrow(x)), nrow(y)),]
            } else if (nrow(x) > nrow(y)) {
              y <- y[rep_len(seq_len(nrow(y)), nrow(x)),]
            }
            result <- dist(x, y, method="Chebyshev", by_rows = TRUE, pairwise=TRUE)
            as.numeric(result)
          }
)

#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for [`Chebyshev`] where `x` and `y` 
#' matrices of rows (interpreted as vectors) to compare. 
setMethod(pairwise, signature = c(measure = "Chebyshev", x = "matrix", y = "matrix"), 
          function(measure, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            scores <- dist(x, y, method="Chebyshev", pairwise = FALSE, by_rows = TRUE)
            if (return_matrix) {
              as.matrix(scores)
            } else {
              scores <- unclass(scores)
              as.PairwiseMatrix(scores)
            }
          }
)

#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for [`Minkowski`] where `x` is a matrix 
#' of rows (interpreted as vectors) to compare among themselves.
setMethod(pairwise, signature = c(measure = "Chebyshev", x = "matrix", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            scores <- dist(x, y=NULL, method="Chebyshev", pairwise = FALSE, by_rows = TRUE)
            if (return_matrix) {
              as.matrix(scores)
            } else {
              scores <- unclass(scores)
              Dim <- rep_len(attr(scores, "Size"), 2)
              as.PairwiseMatrix(scores, Dim, FALSE)
            }
          }
)