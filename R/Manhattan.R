#' @include abstract.R PairwiseMatrix.R Minkowski.R

attrs <- attributes(getClassDef("Minkowski")@prototype)[-1]

setClass("Manhattan", contains = "Minkowski", 
         prototype = structure(
           .Data = elementwise_minkowski_builder(attrs)
         ),
         validity = function(object) {
           errs <- character()
           if (object@p != 1)
             errs <- c(errs, "`p` must be 1 for Manhattan distance")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Manhattan Distance
#' 
#' @description 
#' TODO
#' 
#' @export
Manhattan <- function(...) {
  attrs <- list(p = 1)
  arguments <- list("Manhattan", ".Data" = elementwise_minkowski_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}