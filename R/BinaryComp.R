#' @include StringMeasure.R PairwiseMatrix.R

def_attr_bincomp <- list(
  score = 1.0,
  symmetric = TRUE,
  distance = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_bincomp)] <- def_attr_bincomp

elementwise_bincomp_builder <- function(attrs) {
  function(x, y) {
    if (attrs$ignore_case) {
      x <- tolower(x)
      y <- tolower(y)
    }
    if (attrs$distance) {
      ifelse(x == y, 0, attrs$score)
    } else {
      ifelse(x == y, attrs$score, 0)
    }
  }
}

setClass("BinaryComp", contains = "StringMeasure", 
         slots = c(score = "numeric"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_bincomp_builder(attrs)), def_attr_bincomp)),
         validity = function(object) {
           errs <- character()
           if (length(object@score) != 1)
             errs <- c(errs, "`score` must be a numeric vector of length 1")
           if (object@score <= 0)
             errs <- c(errs, "`score` must be positive")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Binary Comparison Measure
#' 
#' @description 
#' Compares a pair of strings based on whether they are in exact agreement or 
#' not.
#' 
#' @details If `similarity = FALSE` (default) the measure behaves like a 
#' distance. When \eqn{x = y} the measure returns distance 0.0, and when 
#' \eqn{x \neq y} the measure returns `score`.
#' 
#' If `similarity = TRUE` the measure behaves like a similarity. When 
#' \eqn{x = y} the measure returns `score`, and when \eqn{x \neq y} the 
#' measure returns 0.0.
#' 
#' @param score a numeric of length 1. Positive distance to return if the 
#'   pair of strings are not identical. Defaults to 1.0.
#' @param similarity a logical. If TRUE, similarities are returned instead of 
#'   distances. Specifically `score` is returned if the strings agree, 
#'   and 0.0 is returned otherwise.
#' @param ignore_case a logical. If TRUE, case is ignored when comparing the 
#'   strings.
#'   
#' @return 
#' A `BinaryComp` instance is returned, which is an S4 class inheriting from 
#' [`StringMeasure-class`].
#' 
#' @export
BinaryComp <- function(score = 1.0, similarity = FALSE, ignore_case = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$distance <- !similarity
  attrs$similarity <- similarity
  arguments <- list("BinaryComp", ".Data" = elementwise_bincomp_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
setMethod(pairwise, signature = c(measure = "BinaryComp", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            if (measure@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
            }
            scores <- matrix(x, nrow=length(x), ncol=length(y))
            scores <- sweep(scores, 2, y, FUN = "==")
            if (measure@distance) {
              scores <- ifelse(scores, 0, measure@score)
            } else {
              scores <- ifelse(scores, measure@score, 0)
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "BinaryComp", x = "vector", y = "NULL"), 
          # TODO: don't return full matrix since dist is symmetric
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, TRUE)
          }
)