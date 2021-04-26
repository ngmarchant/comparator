#' @include StringComparator.R PairwiseMatrix.R BinaryComp.R
NULL

setClass("CustomStringComparator", contains = c("StringComparator"), 
         slots = c(
           custom_fn = "function"
         ),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           custom_fn = BinaryComp(),
           symmetric = TRUE,
           distance = TRUE,
           similarity = FALSE,
           tri_inequal = FALSE
         ), 
         validity = function(object) {
           errs <- character()
           if (object@tri_inequal & (!object@distance | !object@symmetric | object@similarity))
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Custom String Comparator
#' 
#' @description 
#' Compares a pair of strings \eqn{x} and \eqn{y} using a custom function. 
#' The resulting comparator can be used wherever an object of class 
#' [`StringComparator-class`] is required.
#' 
#' @param custom_fn a string comparison function that takes a pair of character 
#'   vectors as arguments and returns a vector of comparison scores.
#' @param symmetric a logical of length 1. If TRUE, the comparator is symmetric 
#'   in its arguments---i.e. `custom_fn(x, y)` is identical to 
#'   `custom_fn(y, x)`.
#' @param distance a logical of length 1. If `TRUE`, the comparator produces  
#'   distances and satisfies `custom_fn(x, x) = 0`. The comparator may not 
#'   satisfy all of the properties of a distance metric.
#' @param similarity a logical of length 1. If `TRUE`, the comparator produces 
#'   similarity scores.
#' @param tri_inequal a logical of length 1. If `TRUE`, the comparator satisfies 
#'   the triangle inequality. This is only possible (but not guaranteed) if 
#'   `distance = TRUE` and `symmetric = TRUE`. If in doubt, it's best to 
#'   pass FALSE.
#' 
#' @examples
#' ## Compare names with possible typos using a reference of known names
#' known_names <- c("Roberto", "Umberto", "Alberto", "Emberto", "Norberto", "Humberto")
#' c1 <- InVocabulary(known_names)
#' c2 <- Levenshtein(similarity = TRUE, normalize = TRUE)
#' 
#' ## Define a custom comparison function as the product of c1 and c2
#' custom_fn <- function(x, y) c1(x, y) * c2(x, y)
#' 
#' ## Instantiate a string comparator
#' comp <- CustomStringComparator(custom_fn, symmetric = TRUE, distance = FALSE, 
#'                                similarity = TRUE, tri_inequal = FALSE)
#' 
#' x <- "Emberto"
#' y <- c("Enberto", "Umberto") 
#' similarities <- comp(x, y)
#' 
#' @export
CustomStringComparator <- function(custom_fn, symmetric, distance, similarity, 
                                   tri_inequal) {
  arguments <- c(as.list(environment()))
  do.call("new", append("CustomStringComparator", arguments))
}

#' @describeIn elementwise Specialization for [`CustomStringComparator`] where `x` and 
#' `y` are vectors of strings to compare.
setMethod(elementwise, signature = c(comparator = "CustomStringComparator", x = "vector", y = "vector"), 
          function(comparator, x, y, ...) {
            comparator@custom_fn(x, y)
          }
)


#' @describeIn pairwise Specialization for [`CustomStringComparator`] where `x` and `y` 
#' are vectors of strings to compare.
setMethod(pairwise, signature = c(comparator = "CustomStringComparator", x = "vector", y = "vector"), 
          function(comparator, x, y, return_matrix, ...) {
            comb <- expand.grid(x = x, y = y, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
            scores <- comparator@custom_fn(comb$x, comb$y)
            dim(scores) <- c(length(x), length(y))
            
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`CustomStringComparator`] where `x` is a 
#' vector of strings to compare among themselves.
setMethod(pairwise, signature = c(comparator = "CustomStringComparator", x = "vector", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(comparator, x, x, return_matrix)
          }
)