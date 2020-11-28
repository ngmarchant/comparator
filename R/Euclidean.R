#' @include abstract.R PairwiseMatrix.R Minkowski.R

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
#' TODO
#' 
#' @export
Euclidean <- function(...) {
  attrs <- list(p = 2)
  arguments <- list("Euclidean", ".Data" = elementwise_minkowski_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}