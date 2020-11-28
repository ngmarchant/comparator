#' @include abstract.R PairwiseMatrix.R
NULL

def_attr_const <- list(
  constant = 0.0,
  symmetric = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_const)] <- def_attr_const

elementwise_const_builder <- function(attrs) {
  function(x, y) {
    return(rep_len(attrs$constant, max(length(x), length(y))))
  }
}

setClass("ConstantMeasure", contains = "StringMeasure", 
         slots = c(constant = "numeric"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_const_builder(def_attr_const)), attrs)),
         validity = function(object) {
           errs <- character()
           if (length(object@constant) != 1)
             errs <- c(errs, "`constant` must be a numeric vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (object@distance)
             errs <- c(errs, "`distance` must be FALSE")
           if (object@similarity)
             errs <- c(errs, "`similarity` must be FALSE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Constant Measure
#' 
#' @description 
#' A trivial string measure that returns a constant for any pair of values.
#' 
#' @param constant a non-negative numeric constant
#' 
#' @export
ConstantMeasure <- function(constant = 0.0, ...) {
  attrs <- c(as.list(environment()), list(...))
  arguments <- list("ConstantMeasure", ".Data" = elementwise_const_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
setMethod(pairwise, signature = c(measure = "ConstantMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            scores <- matrix(measure@constant, nrow=length(x), ncol=length(y))
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "ConstantMeasure", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)