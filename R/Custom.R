#' @include StringMeasure.R PairwiseMatrix.R
NULL

def_attr_cust <- list()

attrs <- attributes(getClassDef("Measure")@prototype)[-1]
attrs[names(def_attr_cust)] <- def_attr_cust

setClass("CustomMeasure", contains = "Measure", 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_const_builder(def_attr_const)), attrs)),
         validity = function(object) {
           errs <- character()
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Custom Measure
#' 
#' @description 
#' TODO
#' 
#' @param measure a vectorized function that takes a pair of vectors x and y 
#'   and returns a vector of elementwise distances/similarities.
#' @param symmetric whether the measure is symmetric if the pair of vectors 
#'   x and y are interchanged.
#' @param distance whether the measure can be interpreted as a 
#'   distance. Defaults to FALSE.
#' @param similarity whether the measure can be interpreted as a similarity. 
#'   Defaults to FALSE.
#' @param tri_inequal whether the measure satisfies the triangle inequality. 
#'   Defaults to FALSE.
#' 
#' @export
CustomMeasure <- function(measure, symmetric = FALSE, distance = FALSE, 
                          similarity = FALSE, tri_inequal = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$measure <- NULL
  if (length(formals(measure)) != 2) stop("`measure` must have exactly two arguments")
  arguments <- list("CustomMeasure", ".Data" = measure)
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
setMethod(pairwise, signature = c(measure = "CustomMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            scores <- matrix(0.0, nrow = length(x), ncol = length(y))
            for (i in seq_along(x)) {
              scores[i,] <- measure(x, y)
            }
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "CustomMeasure", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)