#' @include StringComparator.R CppSeqComparator.R
NULL

setClass("Hamming", contains = c("CppSeqComparator", "StringComparator"), 
         slots = c(constant = "numeric", 
                   normalize = "logical"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           normalize = FALSE,
           symmetric = TRUE,
           distance = TRUE,
           tri_inequal = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           if (object@tri_inequal & (object@normalize | object@similarity))
             errs <- c(errs, "`tri_inequal` must be FALSE when `normalize` or `similarity` is TRUE")
           if (!object@tri_inequal & !object@normalize & object@distance)
             errs <- c(errs, "`tri_inequal` must be TRUE when `normalize` is FALSE and `distance` is TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Hamming String/Sequence Comparator
#' 
#' @description 
#' The Hamming distance between two strings/sequences of equal length is the 
#' number of positions where the corresponding characters/sequence elements 
#' differ. It can be viewed as a type of edit distance where the only 
#' permitted operation is substitution of characters/sequence elements.
#' 
#' @details When the input strings/sequences \eqn{x} and \eqn{y} are of 
#' different lengths (\eqn{|x| \neq |y|}{|x| != |y|}), the Hamming distance 
#' is defined to be \eqn{\infty}{Inf}.
#' 
#' A Hamming similarity is returned if `similarity = TRUE`. When 
#' \eqn{|x| = |y|} the similarity is defined as follows:
#' \deqn{\mathrm{sim}(x, y) = |x| - \mathrm{dist}(x, y),}{sim(x, y) = |x| - dist(x, y),}
#' where \eqn{sim} is the Hamming similarity and \eqn{dist} is the Hamming 
#' distance. When \eqn{|x| \neq |y|}{|x| != |y|} the similarity is defined to 
#' be 0.
#' 
#' Normalization of the Hamming distance/similarity to the unit interval is 
#' also supported by setting `normalize = TRUE`. The raw distance/similarity 
#' is divided by the length of the string/sequence \eqn{|x| = |y|}. If 
#' \eqn{|x| \neq |y|}{|x| != |y|} the normalized distance is defined to be 1, 
#' while the normalized similarity is defined to be 0.
#' 
#' @note While the unnormalized Hamming distance is a metric, the normalized 
#' variant is not as it does not satisfy the triangle inequality.
#' 
#' @param normalize a logical. If TRUE, distances/similarities are normalized 
#'   to the unit interval. Defaults to FALSE.
#' @param similarity a logical. If TRUE, similarity scores are returned 
#'   instead of distances. Defaults to FALSE. 
#' @param ignore_case a logical. If TRUE, case is ignored when comparing  
#'   strings.
#' @param use_bytes a logical. If TRUE, strings are compared byte-by-byte 
#'   rather than character-by-character.
#' 
#' @return 
#' A `Hamming` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @examples
#' ## Compare US ZIP codes
#' x <- "90001"
#' y <- "90209"
#' m1 <- Hamming()                                     # unnormalized distance
#' m2 <- Hamming(similarity = TRUE, normalize = TRUE)  # normalized similarity
#' m1(x, y)
#' m2(x, y)
#' 
#' @seealso Other edit-based comparators include [`LCS`], [`Levenshtein`], 
#' [`OSA`] and [`DamerauLevenshtein`].
#' 
#' @export
Hamming <- function(normalize = FALSE, similarity = FALSE, ignore_case = FALSE, 
                    use_bytes = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$similarity <- similarity
  arguments$distance <- !similarity
  arguments$tri_inequal <- !similarity & !normalize
  do.call("new", append("Hamming", arguments))
}