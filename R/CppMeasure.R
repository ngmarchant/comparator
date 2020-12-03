#' @include Measure.R
NULL

#' Trait for a measure with an implementation in C++
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

#' @export
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, TRUE, second_arg = TRUE)(x, y)
          }
)


#' @export
setMethod(pairwise, signature = c(measure = "CppMeasure", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            attrs <- attributes(measure)
            pairwise_cpp_builder(class(measure), attrs, return_matrix, second_arg = FALSE)(x)
          }
)