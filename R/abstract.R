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
           
           ifelse(length(errs) == 0, TRUE, errs)
         })

setClass("StringMeasure", 
         slots = c(
           ignore_case = "logical", 
           use_bytes = "logical"
         ), 
         prototype = structure(
           .Data = function(x, y) 0,
           ignore_case = FALSE,
           use_bytes = FALSE,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("VIRTUAL", "Measure"), 
         validity = function(object) {
           errs <- character()
           if (length(object@ignore_case) != 1)
             errs <- c(errs, "`ignore_case` must be a logical vector of length 1")
           if (length(object@use_bytes) != 1) 
             errs <- c(errs, "`use_bytes` must be a logical vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })

setClass("NumericMeasure", 
         prototype = structure(
           .Data = function(x, y) 0,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("VIRTUAL", "Measure"), 
         validity = function(object) {
           errs <- character()
           ifelse(length(errs) == 0, TRUE, errs)
         })

setClass("CppMeasure", 
         contains = "VIRTUAL")

#' @export
setGeneric("pairwise", function(measure, x, y, return_matrix = FALSE, ...) standardGeneric("pairwise"), 
           signature = c("measure", "x", "y"))

#' @export
setGeneric("elementwise", function(measure, x, y, ...) standardGeneric("elementwise"), 
           signature = c("measure", "x", "y"))

#' @export
setMethod(elementwise, signature = c(measure = "Measure", x = "vector", y = "vector"), 
          function(measure, x, y, ...) {
            measure(x, y)
          }
)

elementwise_cpp_builder <- function(measure_name, attrs) {
  attrs[["class"]] <- measure_name
  function(x, y) {
    xy = strings_to_code_vectors(x, y, ignore_case = attrs$ignore_case, use_bytes = attrs$use_bytes)
    codes_x <- xy$x
    codes_y <- xy$y
    elementwisecpp(codes_x, codes_y, attrs)
  }
}

pairwise_cpp_builder <- function(measure_name, attrs, return_matrix, second_arg = TRUE) {
  attrs[["class"]] <- measure_name
  if (second_arg) {
    function(x, y) {
      xy = strings_to_code_vectors(x, y, ignore_case = attrs$ignore_case, use_bytes = attrs$use_bytes)
      codes_x <- xy$x
      codes_y <- xy$y
      scores <- pairwisecpp(codes_x, codes_y, attrs, TRUE)
      if (return_matrix) scores <- as.matrix(scores)
      scores
    }
  } else {
    function(x) {
      xy = strings_to_code_vectors(x, y = NULL, ignore_case = attrs$ignore_case, use_bytes = attrs$use_bytes)
      codes_x <- xy$x
      scores <- pairwisecpp(codes_x, NULL, attrs, return_matrix)
      if (return_matrix) scores <- as.matrix(scores)
      scores
    }
  }
}

#' Pairwise
#' 
#' @export
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, TRUE, second_arg = TRUE)(x, y)
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "Measure", x = "vector", y = "missing"), 
          function(measure, x, y, return_matrix, ...) {
            pairwise(measure, x, NULL, return_matrix)
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, return_matrix, second_arg = FALSE)(x)
          }
)