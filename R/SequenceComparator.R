#' @include Comparator.R
NULL

#' Virtual Sequence Comparator Class 
#' 
#' @description Represents a comparator for pairs of sequences.
#' 
#' @slot .Data a function that calls the elementwise method for this class, 
#'   with arguments `x`, `y` and `...`.
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
setClass("SequenceComparator", 
         contains = c("VIRTUAL", "Comparator"))