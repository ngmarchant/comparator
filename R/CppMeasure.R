#' @include Measure.R
NULL

#' Virtual Class for a Measure with a C++ Implementation
#' 
#' @description 
#' This class serves as a trait which is possessed by measures that have a 
#' C++ implementation. Measures without this trait are implemented in R, 
#' and may be slower to execute.
#' 
#' @export
setClass("CppMeasure", 
         contains = "VIRTUAL")

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

#' @describeIn pairwise Specialization for [`CppMeasure-class`] where `x` and 
#' `y` are vectors of strings to compare.
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, TRUE, second_arg = TRUE)(x, y)
          }
)


#' @describeIn pairwise Specialization for [`CppMeasure-class`] where `x` is 
#' a vector of strings to compare among themselves.
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, return_matrix, second_arg = FALSE)(x)
          }
)