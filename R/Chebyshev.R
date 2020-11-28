#' @include abstract.R PairwiseMatrix.R

def_attr_chebyshev <- list(
  symmetric = TRUE,
  distance = TRUE, 
  tri_inequal = TRUE
)

attrs <- attributes(getClassDef("NumericMeasure")@prototype)[-1]
attrs[names(def_attr_chebyshev)] <- def_attr_chebyshev

#' @importFrom proxy dist
#' @noRd
elementwise_chebyshev_builder <- function(attrs) {
  function(x, y) {
    # proxy::dist expects rows, not vectors
    if (is.null(dim(x))) dim(x) <- c(1, length(x))
    if (is.null(dim(y))) dim(y) <- c(1, length(x))
    mode(x) <- "numeric"
    mode(y) <- "numeric"
    # TODO: recycling
    result <- dist(x, y, method="Chebyshev", by_rows = TRUE, pairwise=TRUE)
    as.numeric(result)
  }
}

setClass("Chebyshev", contains = "NumericMeasure", 
         slots = c(p = "numeric"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_chebyshev_builder(attrs)), def_attr_chebyshev)),
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
#' TODO
#' 
#' @export
Chebyshev <- function(...) {
  attrs <- c(as.list(environment()), list(...))
  arguments <- list("Chebyshev", ".Data" = elementwise_chebyshev_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @importFrom proxy dist as.matrix
#' @export
setMethod(pairwise, signature = c(measure = "Chebyshev", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            # proxy::dist expects rows, not vectors
            if (is.null(dim(x))) dim(x) <- c(1, length(x))
            if (is.null(dim(y))) dim(y) <- c(1, length(x))
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
#' @export
setMethod(pairwise, signature = c(measure = "Chebyshev", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (is.null(dim(x))) dim(x) <- c(1, length(x))
            mode(x) <- "numeric"
            scores <- dist(x, y=NULL, method="Chebyshev", pairwise = FALSE, by_rows = TRUE)
            if (return_matrix) {
              as.matrix(scores)
            } else {
              scores <- unclass(scores)
              Dim <- rep_len(attr(temp, "Size"), 2)
              as.PairwiseMatrix(scores, Dim, FALSE)
            }
          }
)