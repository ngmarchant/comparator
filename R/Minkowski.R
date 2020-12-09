#' @include NumericComparator.R PairwiseMatrix.R

setClass("Minkowski", contains = "NumericComparator", 
         slots = c(p = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           p = 2.0,
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
           if (length(object@p) != 1 | object@p <= 0)
             errs <- c(errs, "`p` must be a positive numeric vector of length 1")
           if (!object@tri_inequal & object@p >= 1)
             errs <- c(errs, "`tri_inequal` must be TRUE if `p >= 1`")
           if (object@tri_inequal & object@p < 1)
             errs <- c(errs, "`tri_inequal` must be FALSE if `p < 1`")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Minkowski Numeric Comparator
#' 
#' @description 
#' The Minkowski distance (a.k.a. L-p distance) between two vectors \eqn{x} and 
#' \eqn{y} is the p-th root of the sum of the absolute differences of their 
#' Cartesian coordinates raised to the p-th power: 
#' \deqn{\mathrm{Minkowski}(x,y) = \left(\sum_{i = 1}^{n} |x_i - y_i|^p\right)^{1/p}.}{Minkowski(x,y) = (sum_i { |x_i - y_i|^p })^(1/p).}
#' 
#' @param p a positive numeric specifying the order of the distance. Defaults 
#'   to 2 (Euclidean distance). If `p < 1` the Minkowski distance does not 
#'   satisfy the triangle inequality and is therefore not a proper distance 
#'   metric.
#' 
#' @return 
#' A `Minkowski` instance is returned, which is an S4 class inheriting 
#' from [`NumericComparator-class`].
#' 
#' @seealso 
#' Other numeric comparators include [`Manhattan`], [`Euclidean`] and 
#' [`Chebyshev`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Minkowski()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' comparator <- Minkowski()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(comparator, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(comparator, x, y)
#' 
#' @export
Minkowski <- function(p = 2.0) {
  arguments <- c(as.list(environment()))
  arguments$tri_inequal <- p >= 1
  do.call("new", append("Minkowski", arguments))
}

#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for a [`Minkowski`] where `x` and `y` 
#' matrices of rows (interpreted as vectors) to compare. 
setMethod(elementwise, signature = c(comparator = "Minkowski", x = "matrix", y = "matrix"), 
          function(comparator, x, y, ...) {
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            # recycle as needed if one matrix has fewer rows than the other
            if (nrow(x) < nrow(y)) {
              x <- x[rep_len(seq_len(nrow(x)), nrow(y)),]
            } else if (nrow(x) > nrow(y)) {
              y <- y[rep_len(seq_len(nrow(y)), nrow(x)),]
            }
            p <- comparator@p
            if (is.infinite(p)) {
              result <- dist(x, y, method="Chebyshev", by_rows = TRUE, pairwise=TRUE)
            } else {
              result <- dist(x, y, method="Minkowski", p=p, by_rows = TRUE, pairwise=TRUE)
            }
            as.numeric(result)
          }
)


#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for a [`Minkowski`] where `x` and `y` 
#' matrices of rows (interpreted as vectors) to compare. 
setMethod(pairwise, signature = c(comparator = "Minkowski", x = "matrix", y = "matrix"), 
          function(comparator, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            p <- comparator@p
            if (is.infinite(p)) {
              score <- dist(x, y, method="Chebyshev", pairwise = FALSE, by_rows = TRUE)
            } else {
              score <- dist(x, y, method="Minkowski", p=p, pairwise = FALSE, by_rows = TRUE)
            }
            if (return_matrix) {
              as.matrix(score)
            } else {
              score <- unclass(score)
              as.PairwiseMatrix(score)
            }
          }
)

#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for [`Minkowski`] where `x` is a matrix 
#' of rows (interpreted as vectors) to compare among themselves.
setMethod(pairwise, signature = c(comparator = "Minkowski", x = "matrix", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            p <- comparator@p
            if (is.infinite(p)) {
              score <- dist(x, y, method="Chebyshev", pairwise = FALSE, by_rows = TRUE)
            } else {
              score <- dist(x, y=NULL, method="Minkowski", p=p, pairwise = FALSE, by_rows = TRUE)
            }
            if (return_matrix) {
              as.matrix(score)
            } else {
              score <- unclass(score)
              Dim <- rep_len(attr(score, "Size"), 2)
              as.PairwiseMatrix(score, Dim, FALSE)
            }
          }
)