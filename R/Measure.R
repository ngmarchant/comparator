#' Virtual measure class from which all other measures are derived
#' 
#' @slot .Data
#' @slot symmetric
#' @slot distance
#' @slot similarity
#' @slot tri_inequal
#' 
#' @export
setClass("Measure", 
         slots = c(
           symmetric = "logical", 
           distance = "logical",
           similarity = "logical",
           tri_inequal = "logical"
         ), 
         prototype = structure(
           .Data = function(x, y) 0,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("VIRTUAL", "function"), 
         validity = function(object) {
           errs <- character()
           if (length(object@symmetric) != 1) 
             errs <- c(errs, "`symmetric` must be a logical vector of length 1")
           if (length(object@distance) != 1) 
             errs <- c(errs, "`distance` must be a logical vector of length 1")
           if (length(object@similarity) != 1) 
             errs <- c(errs, "`similarity` must be a logical vector of length 1")
           if (object@similarity & object@distance)
             errs <- c(errs, "`similarity` and `distance` cannot both be TRUE")
           if (length(object@tri_inequal) != 1) 
             errs <- c(errs, "`tri_inequal` must be a logical vector of length 1")
           if (object@tri_inequal & object@similarity)
             errs <- c(errs, "`tri_inequal` cannot be TRUE when `similarity` is TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' @export
setGeneric("pairwise", function(measure, x, y, return_matrix = FALSE, ...) standardGeneric("pairwise"), 
           signature = c("measure", "x", "y"))

#' @export
setMethod(pairwise, signature = c(measure = "Measure", x = "vector", y = "missing"), 
          function(measure, x, y, return_matrix, ...) {
            pairwise(measure, x, NULL, return_matrix)
          }
)

#' @export
setGeneric("elementwise", function(measure, x, y, ...) standardGeneric("elementwise"), 
           signature = c("measure", "x", "y"))

#' @export
setMethod(elementwise, signature = c(measure = "Measure", x = "vector", y = "vector"), 
          function(measure, x, y, ...) {
            # Using the fact that the elementwise measure is stored in the .Data slot
            measure(x, y)
          }
)