#' @include Comparator.R
NULL

#' Virtual Class for a Sequence Comparator with a C++ Implementation
#' 
#' @description 
#' This class is a trait possessed by SequenceComparators that have a 
#' C++ implementation. SequenceComparators without this trait are implemented 
#' in R, and may be slower to execute.
#' 
#' @export
setClass("CppSeqComparator", 
         contains = "VIRTUAL")

is_list_of_vectors <- function(x) {
  is.list(x) & all(sapply(x, is.vector))
}

#' @describeIn elementwise Specialization for [`CppSeqComparator-class`] where 
#' `x` and `y` are lists of sequences (vectors) to compare.
setMethod(elementwise, signature = c(comparator = "CppSeqComparator", x = "list", y = "list"), 
          function(comparator, x, y, ...) {
            x <- as.list(x)
            y <- as.list(y)
            if (!is_list_of_vectors(x)) stop("`x` must be a list of vectors")
            if (!is_list_of_vectors(y)) stop("`y` must be a list of vectors")
            elementwisecpp(x, y, comparator)
          }
)

#' @describeIn pairwise Specialization for [`CppSeqComparator-class`] where `x` 
#' and `y` are lists of sequences (vectors) to compare.
setMethod(pairwise, signature = c(comparator = "CppSeqComparator", x = "list", y = "list"), 
          function(comparator, x, y, return_matrix, ...) {
            x <- as.list(x)
            y <- as.list(y)
            if (!is_list_of_vectors(x)) stop("`x` must be a list of vectors")
            if (!is_list_of_vectors(y)) stop("`y` must be a list of vectors")
            scores <- pairwisecpp(x, y, comparator, TRUE)
            if (return_matrix) scores <- as.matrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`CppSeqComparator-class`] where `x` is 
#' a list of sequences (vectors) to compare.
setMethod(pairwise, signature = c(comparator = "CppSeqComparator", x = "list", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            x <- as.list(x)
            if (!is_list_of_vectors(x)) stop("`x` must be a list of vectors")
            scores <- pairwisecpp(x, NULL, comparator, return_matrix)
            if (return_matrix) scores <- as.matrix(scores)
            scores
          }
)