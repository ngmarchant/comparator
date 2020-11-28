#' @include abstract.R PairwiseMatrix.R

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
    # TODO: recycling
    result <- dist(x, y, method="Minkowski", p=attrs$p, by_rows = TRUE, pairwise=TRUE)
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
           if (!object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Minkowski Distance
#' 
#' @description 
#' Compares a pair of strings based on whether they are in exact agreement or 
#' not. Behaves as a distance measure by default.
#' 
#' @param p TODO
#' 
#' @export
Minkowski <- function(p = 2.0, ...) {
  attrs <- c(as.list(environment()), list(...))
  arguments <- list("Minkowski", ".Data" = elementwise_minkowski_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @importFrom proxy dist as.matrix
#' @export
setMethod(pairwise, signature = c(measure = "Minkowski", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            # proxy::dist expects rows, not vectors
            if (is.null(dim(x))) dim(x) <- c(1, length(x))
            if (is.null(dim(y))) dim(y) <- c(1, length(x))
            mode(x) <- "numeric"
            mode(y) <- "numeric"
            score <- dist(x, y, method="Minkowski", p=measure@p, pairwise = FALSE, by_rows = TRUE)
            if (return_matrix) {
              as.matrix(score)
            } else {
              score <- unclass(score)
              as.PairwiseMatrix(score)
            }
          }
)

#' @importFrom proxy dist as.matrix
#' @export
setMethod(pairwise, signature = c(measure = "Minkowski", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (is.null(dim(x))) dim(x) <- c(1, length(x))
            mode(x) <- "numeric"
            score <- dist(x, y=NULL, method="Minkowski", p=measure@p, pairwise = FALSE, by_rows = TRUE)
            if (return_matrix) {
              as.matrix(score)
            } else {
              score <- unclass(score)
              Dim <- rep_len(attr(temp, "Size"), 2)
              as.PairwiseMatrix(score, Dim, FALSE)
            }
          }
)