#' @include StringComparator.R CppSeqComparator.R
NULL

setClass("Levenshtein", contains = c("CppSeqComparator", "StringComparator"), 
         slots = c(
           deletion = "numeric", 
           insertion = "numeric", 
           substitution = "numeric",
           normalize = "logical"
         ),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           deletion = 1.0,
           insertion = 1.0, 
           substitution = 1.0,
           normalize = FALSE,
           symmetric = TRUE,
           distance = TRUE,
           tri_inequal = TRUE
         ), 
         validity = function(object) {
           errs <- character()
           symmetric_weights <- object@deletion == object@insertion
           if (!object@symmetric & symmetric_weights)
             errs <- c(errs, "`symmetric` must be TRUE when operations and their inverses have equal weights")
           if (object@symmetric & !symmetric_weights)
             errs <- c(errs, "`symmetric` must be FALSE when operations and their inverses do not have equal weights")
           if (!object@tri_inequal & symmetric_weights & object@distance)
             errs <- c(errs, "`tri_inequal` must be TRUE when operations and their inverses have equal weights and `distance` is TRUE")
           if (object@tri_inequal & !symmetric_weights)
             errs <- c(errs, "`tri_inequal` must be FALSE when operations and their inverses do not have equal weights")
           if (object@deletion < 0 | length(object@deletion) != 1)
             errs <- c(errs, "`deletion` must be a non-negative numeric vector of length 1")
           if (object@insertion < 0 | length(object@insertion) != 1)
             errs <- c(errs, "`insertion` must be a non-negative numeric vector of length 1")
           if (object@substitution < 0 | length(object@substitution) != 1)
             errs <- c(errs, "`substitution` must be a non-negative numeric vector of length 1")
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Levenshtein String/Sequence Comparator
#' 
#' @description 
#' The Levenshtein (edit) distance between two strings/sequences \eqn{x} and 
#' \eqn{y} is the minimum cost of operations (insertions, deletions or 
#' substitutions) required to transform \eqn{x} into \eqn{y}.
#' 
#' @details 
#' For simplicity we assume `x` and `y` are strings in this section,
#' however the comparator is also implemented for more general sequences.
#' 
#' A Levenshtein similarity is returned if `similarity = TRUE`, which 
#' is defined as 
#' \deqn{\mathrm{sim}(x, y) = \frac{w_d |x| + w_i |y| - \mathrm{dist}(x, y)}{2},}{sim(x, y) = (w_d |x| + w_i |y| - dist(x, y))/2}
#' where \eqn{|x|}, \eqn{|y|} are the number of characters in \eqn{x} and 
#' \eqn{y} respectively, \eqn{\mathrm{dist}}{dist} is the Levenshtein distance, 
#' \eqn{w_d} is the cost of a deletion and \eqn{w_i} is the cost of an 
#' insertion.
#' 
#' Normalization of the Levenshtein distance/similarity to the unit interval 
#' is also supported by setting `normalize = TRUE`. The normalization approach 
#' follows Yujian and Bo (2007), and ensures that the distance remains a metric 
#' when the costs of insertion \eqn{w_i} and deletion \eqn{w_d} are equal. 
#' The normalized distance \eqn{\mathrm{dist}_n}{dist_n} is defined as
#' \deqn{\mathrm{dist}_n(x, y) = \frac{2 \mathrm{dist}(x, y)}{w_d |x| + w_i |y| + \mathrm{dist}(x, y)},}{dist_n(x, y) = 2 * dist(x, y) / (w_d |x| + w_i |y| + dist(x, y)),}
#' and the normalized similarity \eqn{\mathrm{sim}_n}{sim_n} is defined as 
#' \deqn{\mathrm{sim}_n(x, y) = 1 - \mathrm{dist}_n(x, y) = \frac{\mathrm{sim}(x, y)}{w_d |x| + w_i |y| - \mathrm{sim}(x, y)}.}{sim_n(x, y) = 1 - dist_n(x, y) = sim(x, y) / (w_d |x| + w_i |y| - sim(x, y)).}
#' 
#' @note 
#' If the costs of deletion and insertion are equal, this comparator is 
#' symmetric in \eqn{x} and \eqn{y}. In addition, the normalized and 
#' unnormalized distances satisfy the properties of a metric.
#' 
#' @param deletion positive cost associated with deletion of a character 
#'   or sequence element. Defaults to unit cost.
#' @param insertion positive cost associated insertion of a character 
#'   or sequence element. Defaults to unit cost.
#' @param substitution positive cost associated with substitution of a 
#'   character or sequence element. Defaults to unit cost.
#' @param normalize a logical. If TRUE, distances are normalized to the 
#'   unit interval. Defaults to FALSE.
#' @param similarity a logical. If TRUE, similarity scores are returned 
#'   instead of distances. Defaults to FALSE. 
#' @param ignore_case a logical. If TRUE, case is ignored when comparing  
#'   strings.
#' @param use_bytes a logical. If TRUE, strings are compared byte-by-byte 
#'   rather than character-by-character.
#' 
#' @return 
#' A `Levenshtein` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @references 
#' Navarro, G. (2001), "A guided tour to approximate string matching", 
#' \emph{ACM Computing Surveys (CSUR)}, \strong{33}(1), 31-88.
#' 
#' Yujian, L. & Bo, L. (2007), "A Normalized Levenshtein Distance Metric",
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence} 
#' \strong{29}, 1091–1095.
#' 
#' @examples
#' ## Compare names with potential typos
#' x <- c("Brian Cheng", "Bryan Cheng", "Kondo Onyejekwe", "Condo Onyejekve")
#' pairwise(Levenshtein(), x, return_matrix = TRUE)
#' 
#' ## When the substitution cost is high, Levenshtein distance reduces to LCS distance
#' Levenshtein(substitution = 100)("Iran", "Iraq") == LCS()("Iran", "Iraq")
#' 
#' @seealso Other edit-based comparators include [`Hamming`], [`LCS`], 
#' [`OSA`] and [`DamerauLevenshtein`].
#' 
#' @export
Levenshtein <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                        normalize = FALSE, similarity = FALSE, 
                        ignore_case = FALSE, use_bytes = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$similarity <- similarity
  arguments$distance <- !similarity
  arguments$symmetric <- deletion == insertion
  arguments$tri_inequal <- deletion == insertion & !similarity
  do.call("new", append("Levenshtein", arguments))
}