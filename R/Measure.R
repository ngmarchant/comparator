#' Virtual Measure Class 
#' 
#' @description This class represents a measure for comparing pairs of objects. 
#'   It is the base class from which other types of measures (e.g. 
#'   [`NumericMeasure-class`] and [`StringMeasure-class`]) are derived.
#' 
#' @slot .Data a function which takes a pair of arguments `x` and `y`, and 
#'   returns the elementwise measure scores.
#' @slot symmetric a logical of length 1. If TRUE, the measure is symmetric 
#'   in its arguments---i.e. `measure(x, y)` is identical to `measure(y, x)`.
#' @slot distance a logical of length 1. If `TRUE`, the measure produces  
#'   distances and satisfies `measure(x, x) = 0`. The measure may not satisfy 
#'   all of the properties of a distance metric.
#' @slot similarity a logical of length 1. If `TRUE`, the measure produces 
#'   similarity scores.
#' @slot tri_inequal a logical of length 1. If `TRUE`, the measure satisfies 
#'   the triangle inequality. This is only possible (but not guaranteed) if 
#'   `distance = TRUE` and `symmetric = TRUE`.
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
#' (strings, vectors, etc.) using the provided measure.
#' 
#' @param measure a measure used to compare the objects, which is a sub-class 
#'   of [`Measure-class`]. 
#' @param x,y a collection of objects to compare, typically stored as entries 
#'   in an atomic vector, rows in a matrix, or entries in a list. The required 
#'   format depends on the type of `measure`. `y` may be omitted or set to 
#'   `NULL` to compare objects in `x`.
#' @param return_matrix a logical of length 1. If FALSE (default), the pairwise 
#'   similarities/distances will be returned as a [`PairwiseMatrix-class`] 
#'   which is more space-efficient for symmetric measures. If TRUE, a standard 
#'   [`matrix`] is returned instead.
#' @param ... other parameters passed on to other methods.
#' 
#' @return 
#' If both `x` and `y` are specified, every object in `x` is compared with 
#' every object in `y` using the measure, and the resulting scores are returned 
#' in a `size(x) × size(y)` matrix. 
#' 
#' If only `x` is specified, then the objects in `x` are compared with 
#' themselves using the measure, and the resulting scores are returned in a 
#' `size(x) × size(y)` matrix. 
#' 
#' By default, the matrix is represented as an instance of the 
#' [`PairwiseMatrix-class`] class, which is more space-efficient for symmetric 
#' measures when `y` is not specified. However, if `return_matrix = TRUE`, 
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
#' measure <- DamerauLevenshtein(similarity = TRUE, normalize = TRUE)
#' pairwise(measure, x, return_matrix = TRUE)  # return an ordinary matrix
#' 
#' @export
setGeneric("pairwise", function(measure, x, y, return_matrix = FALSE, ...) standardGeneric("pairwise"), 
           signature = c("measure", "x", "y"))

#' @describeIn pairwise Compute a pairwise measure when `y` 
setMethod(pairwise, signature = c(measure = "Measure", x = "ANY", y = "missing"), 
          function(measure, x, y, return_matrix, ...) {
            pairwise(measure, x, NULL, return_matrix)
          }
)

#' Elementwise Similarity/Distance Vector
#' 
#' @description 
#' Computes elementwise similarities/distances between two collections of 
#' objects (strings, vectors, etc.) using the provided measure.
#'
#' @param measure a measure used to compare the objects, which is a sub-class 
#'   of [`Measure-class`]. 
#' @param x,y a collection of objects to compare, typically stored as entries 
#'   in an atomic vector, rows in a matrix, or entries in a list. The required 
#'   format depends on the type of `measure`. If `x` and `y` do not contain 
#'   the same number of objects, the smaller collection is recycled according
#'   to standard `R` behavior.
#' @param ... other parameters passed on to other methods.
#' 
#' @return
#' Every object in `x` is compared to every object in `y` elementwise 
#' (with recycling) using the given measure , to produce a numeric vector of 
#' scores of length \eqn{max{size(x), size(y)}}.
#' 
#' @note 
#' This function is not strictly necessary, as the `measure` itself is a 
#' function that returns elementwise vectors of scores. In other words, 
#' `measure(x, y, ...)` is equivalent to `elementwise(measure, x, y, ...)`.
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
setGeneric("elementwise", function(measure, x, y, ...) standardGeneric("elementwise"), 
           signature = c("measure", "x", "y"))

# #' @describeIn elementwise Calls `measure(x, y)` for any `x` and `y`
# setMethod(elementwise, signature = c(measure = "Measure", x = "ANY", y = "ANY"), 
#           function(measure, x, y, ...) {
#             # Using the fact that the elementwise measure is stored in the .Data slot
#             measure(x, y)
#           }
# )