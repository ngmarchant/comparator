#' Virtual Comparator Class 
#' 
#' @description This class represents a function for comparing pairs of 
#'   objects. It is the base class from which other types of comparators (e.g. 
#'   [`NumericComparator-class`] and [`StringComparator-class`]) are derived.
#' 
#' @slot .Data a function which takes a pair of arguments `x` and `y`, and 
#'   returns the elementwise scores.
#' @slot symmetric a logical of length 1. If TRUE, the comparator is symmetric 
#'   in its arguments---i.e. `comparator(x, y)` is identical to 
#'   `comparator(y, x)`.
#' @slot distance a logical of length 1. If `TRUE`, the comparator produces  
#'   distances and satisfies `comparator(x, x) = 0`. The comparator may not 
#'   satisfy all of the properties of a distance metric.
#' @slot similarity a logical of length 1. If `TRUE`, the comparator produces 
#'   similarity scores.
#' @slot tri_inequal a logical of length 1. If `TRUE`, the comparator satisfies 
#'   the triangle inequality. This is only possible (but not guaranteed) if 
#'   `distance = TRUE` and `symmetric = TRUE`.
#' 
#' @export
setClass("Comparator", 
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
           # TODO: enable after refactor
           #if (length(formals(object@.Data)) != 3) 
           #  errs <- c(errs, "`.Data` must have exactly three arguments")
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

#' Pairwise Similarity/Distance Matrix
#' 
#' @description 
#' Computes pairwise similarities/distances between two collections of objects 
#' (strings, vectors, etc.) using the provided comparator.
#' 
#' @param comparator a comparator used to compare the objects, which is a 
#'   sub-class of [`Comparator-class`]. 
#' @param x,y a collection of objects to compare, typically stored as entries 
#'   in an atomic vector, rows in a matrix, or entries in a list. The required 
#'   format depends on the type of `comparator`. `y` may be omitted or set to 
#'   `NULL` to compare objects in `x`.
#' @param return_matrix a logical of length 1. If FALSE (default), the pairwise 
#'   similarities/distances will be returned as a [`PairwiseMatrix-class`] 
#'   which is more space-efficient for symmetric comparators. If TRUE, a 
#'   standard [`matrix`] is returned instead.
#' @param ... other parameters passed on to other methods.
#' 
#' @return 
#' If both `x` and `y` are specified, every object in `x` is compared with 
#' every object in `y` using the comparator, and the resulting scores are 
#' returned in a `size(x)` by `size(y)` matrix. 
#' 
#' If only `x` is specified, then the objects in `x` are compared with 
#' themselves using the comparator, and the resulting scores are returned in a 
#' `size(x)` by `size(y)` matrix. 
#' 
#' By default, the matrix is represented as an instance of the 
#' [`PairwiseMatrix-class`] class, which is more space-efficient for symmetric 
#' comparators when `y` is not specified. However, if `return_matrix = TRUE`, 
#' the matrix is returned as an ordinary [`matrix`] instead.
#' 
#' @examples
#' ## Computing the distances between a query point y (a 3D numeric vector) 
#' ## and a set of reference points x
#' x <- rbind(c(1,0,1), c(0,0,0), c(-1,2,-1))
#' y <- c(10, 5, 10)
#' pairwise(Manhattan(), x, y)
#' 
#' ## Computing the pairwise similarities among a set of strings
#' x <- c("Benjamin", "Ben", "Benny", "Bne", "Benedict", "Benson")
#' comparator <- DamerauLevenshtein(similarity = TRUE, normalize = TRUE)
#' pairwise(comparator, x, return_matrix = TRUE)  # return an ordinary matrix
#' 
#' @export
setGeneric("pairwise", function(comparator, x, y, return_matrix = FALSE, ...) standardGeneric("pairwise"), 
           signature = c("comparator", "x", "y"))

#' @describeIn pairwise Compute a pairwise comparator when `y` 
setMethod(pairwise, signature = c(comparator = "Comparator", x = "ANY", y = "missing"), 
          function(comparator, x, y, return_matrix, ...) {
            pairwise(comparator, x, NULL, return_matrix)
          }
)

#' Elementwise Similarity/Distance Vector
#' 
#' @description 
#' Computes elementwise similarities/distances between two collections of 
#' objects (strings, vectors, etc.) using the provided comparator.
#'
#' @param comparator a comparator used to compare the objects, which is a 
#'   sub-class of [`Comparator-class`]. 
#' @param x,y a collection of objects to compare, typically stored as entries 
#'   in an atomic vector, rows in a matrix, or entries in a list. The required 
#'   format depends on the type of `comparator`. If `x` and `y` do not contain 
#'   the same number of objects, the smaller collection is recycled according
#'   to standard `R` behavior.
#' @param ... other parameters passed on to other methods.
#' 
#' @return
#' Every object in `x` is compared to every object in `y` elementwise 
#' (with recycling) using the given comparator, to produce a numeric vector of 
#' scores of length \eqn{max{size(x), size(y)}}.
#' 
#' @note 
#' This function is not strictly necessary, as the `comparator` itself is a 
#' function that returns elementwise vectors of scores. In other words, 
#' `comparator(x, y, ...)` is equivalent to 
#' `elementwise(comparator, x, y, ...)`.
#' 
#' @examples
#' ## Compute the absolute difference between two sets of scalar observations
#' data("iris")
#' x <- as.matrix(iris$Sepal.Width)
#' y <- as.matrix(iris$Sepal.Length)
#' elementwise(Euclidean(), x, y)
#' 
#' ## Compute the edit distance between columns of two linked data.frames
#' col.1 <- c("Hasna Yuhanna", "Korina Zenovia", "Phyllis Haywood", "Nicky Ellen")
#' col.2 <- c("Hasna Yuhanna", "Corinna Zenovia", "Phyllis Dorothy Haywood", "Nicole Ellen")
#' elementwise(Levenshtein(), col.1, col.2)
#' Levenshtein()(col.1, col.2)               # equivalent to above
#' 
#' ## Recycling is used if the two collections don't contain the same number of objects
#' elementwise(Levenshtein(), "Cora Zenovia", col.1)
#' 
#' @export
setGeneric("elementwise", function(comparator, x, y, ...) standardGeneric("elementwise"), 
           signature = c("comparator", "x", "y"))