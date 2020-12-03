#' @include NumericMeasure.R PairwiseMatrix.R

def_attr_minkowski <- list(
  p = 2.0,
  symmetric = TRUE,
  distance = TRUE, 
  tri_inequal = TRUE
)

attrs <- attributes(getClassDef("NumericMeasure")@prototype)[-1]
attrs[names(def_attr_minkowski)] <- def_attr_minkowski

#' @importFrom proxy dist
#' @noRd
elementwise_minkowski_builder <- function(attrs) {
  function(x, y) {
    # proxy::dist expects rows, not vectors
    if (is.null(dim(x))) dim(x) <- c(1, length(x))
    if (is.null(dim(y))) dim(y) <- c(1, length(x))
    mode(x) <- "numeric"
    mode(y) <- "numeric"
    # recycle as needed if one matrix has fewer rows than the other
    if (nrow(x) < nrow(y)) {
      x <- x[rep_len(seq_len(nrow(x)), nrow(y)),]
    } else if (nrow(x) > nrow(y)) {
      y <- y[rep_len(seq_len(nrow(y)), nrow(x)),]
    }
    p <- attrs$p
    if (is.infinite(p)) {
      result <- dist(x, y, method="Chebyshev", by_rows = TRUE, pairwise=TRUE)
    } else {
      result <- dist(x, y, method="Minkowski", p=p, by_rows = TRUE, pairwise=TRUE)
    }
    as.numeric(result)
  }
}

setClass("Minkowski", contains = "NumericMeasure", 
         slots = c(p = "numeric"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_minkowski_builder(attrs)), def_attr_minkowski)),
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


#' Minkowski Distance
#' 
#' @description 
#' The Minkowski distance (a.k.a. L-p distance) between two vectors \eqn{x} and 
#' \eqn{y} is the p-th root of the sum of the absolute differences of their 
#' Cartesian coordinates raised to the p-th power: 
#' \deqn{Minkowski(x,y) = (\sum_{i = 1}^{n} |x_i - q_i|^p)^{1/p}.}
#' 
#' @param p a positive numeric specifying the order of the distance. Defaults 
#'   to 2 (Euclidean distance). If `p < 1` the Minkowski distance does not 
#'   satisfy the triangle inequality and is therefore not a proper distance 
#'   metric.
#' 
#' @return 
#' A `Minkowski` instance is returned, which is an S4 class inheriting 
#' from [`NumericMeasure-class`].
#' 
#' @seealso 
#' Other numeric measures include [`Manhattan`], [`Euclidean`] and 
#' [`Chebyshev`].
#' 
#' @examples 
#' ## Distance between two vectors
#' x <- c(0, 1, 0, 1, 0)
#' y <- seq_len(5)
#' Minkowski()(x, y)
#' 
#' ## Distance between rows (elementwise) of two matrices
#' measure <- Minkowski()
#' x <- matrix(rnorm(25), nrow = 5)
#' y <- matrix(rnorm(5), nrow = 1)
#' elementwise(measure, x, y)
#' 
#' ## Distance between rows (pairwise) of two matrices
#' pairwise(measure, x, y)
#' 
#' @export
Minkowski <- function(p = 2.0) {
  attrs <- c(as.list(environment()))
  attrs$tri_inequal <- p >= 1
  arguments <- list("Minkowski", ".Data" = elementwise_minkowski_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @importFrom proxy dist as.matrix
#' @describeIn pairwise Specialization for a [`Minkowski`] where `x` and `y` 
#' matrices of rows (interpreted as vectors) to compare. If `x` any `y` do 
#' not have the same number of rows, rows are recycled in the smaller matrix. 
setMethod(pairwise, signature = c(measure = "Minkowski", x = "matrix", y = "matrix"), 
          function(measure, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            p <- measure@p
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
setMethod(pairwise, signature = c(measure = "Minkowski", x = "matrix", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            mode(x) <- "numeric"
            p <- measure@p
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