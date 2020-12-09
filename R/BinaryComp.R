#' @include StringComparator.R PairwiseMatrix.R CppSeqComparator.R

setClass("BinaryComp", contains = c("CppSeqComparator", "StringComparator"), 
         slots = c(score = "numeric"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           score = 1.0,
           symmetric = TRUE,
           distance = TRUE
         ),
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


#' Binary String/Sequence Comparator
#' 
#' @description 
#' Compares a pair of strings or sequences based on whether they are 
#' identical or not.
#' 
#' @details If `similarity = FALSE` (default) the scores can be interpreted 
#' as distances. When \eqn{x = y} the comparator returns a distance of 0.0, 
#' and when \eqn{x \neq y} the comparator returns `score`.
#' 
#' If `similarity = TRUE` the scores can be interpreted as similarities. 
#' When \eqn{x = y} the comparator returns `score`, and when \eqn{x \neq y} 
#' the comparator returns 0.0.
#' 
#' @param score a numeric of length 1. Positive distance to return if the 
#'   pair of strings/sequences are not identical. Defaults to 1.0.
#' @param similarity a logical. If TRUE, similarities are returned instead of 
#'   distances. Specifically `score` is returned if the strings agree, 
#'   and 0.0 is returned otherwise.
#' @param ignore_case a logical. If TRUE, case is ignored when comparing  
#'   strings.
#'   
#' @return 
#' A `BinaryComp` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @export
BinaryComp <- function(score = 1.0, similarity = FALSE, ignore_case = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$distance <- !similarity
  arguments$similarity <- similarity
  do.call("new", append("BinaryComp", arguments))
}