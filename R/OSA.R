#' @include StringComparator.R CppSeqComparator.R
NULL

setClass("OSA", contains = c("CppSeqComparator", "StringComparator"), 
         slots = c(
           deletion = "numeric", 
           insertion = "numeric", 
           substitution = "numeric",
           transposition = "numeric",
           normalize = "logical"
         ),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           deletion = 1.0,
           insertion = 1.0, 
           substitution = 1.0,
           transposition = 1.0,
           normalize = FALSE,
           symmetric = TRUE,
           distance = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (object@deletion < 0 || length(object@deletion) != 1)
             errs <- c(errs, "`deletion` must be a non-negative numeric vector of length 1")
           if (object@insertion < 0 || length(object@insertion) != 1)
             errs <- c(errs, "`insertion` must be a non-negative numeric vector of length 1")
           if (object@substitution < 0 || length(object@substitution) != 1)
             errs <- c(errs, "`substitution` must be a non-negative numeric vector of length 1")
           if (object@transposition < 0 || length(object@transposition) != 1)
             errs <- c(errs, "`transposition` must be a non-negative numeric vector of length 1")
           symmetric_weights <- object@deletion == object@insertion
           if (!object@symmetric && symmetric_weights)
             errs <- c(errs, "`symmetric` must be TRUE when operations and their inverses have equal weights")
           if (object@symmetric && !symmetric_weights)
             errs <- c(errs, "`symmetric` must be FALSE when operations and their inverses do not have equal weights")
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Optimal String Alignment (OSA) String/Sequence Comparator
#' 
#' @description 
#' The Optimal String Alignment (OSA) distance between two strings/sequences 
#' \eqn{x} and \eqn{y} is the minimum cost of operations (insertions, 
#' deletions, substitutions or transpositions) required to transform  \eqn{x} 
#' into \eqn{y}, subject to the constraint that _no substring/subsequence is 
#' edited more than once_. 
#' 
#' @details 
#' For simplicity we assume `x` and `y` are strings in this section,
#' however the comparator is also implemented for more general sequences.
#' 
#' An OSA similarity is returned if `similarity = TRUE`, which 
#' is defined as 
#' \deqn{\mathrm{sim}(x, y) = \frac{w_d |x| + w_i |y| - \mathrm{dist}(x, y)}{2},}{sim(x, y) = (w_d |x| + w_i |y| - dist(x, y))/2}
#' where \eqn{|x|}, \eqn{|y|} are the number of characters in \eqn{x} and 
#' \eqn{y} respectively, \eqn{dist} is the OSA distance, \eqn{w_d} 
#' is the cost of a deletion and \eqn{w_i} is the cost of an insertion.
#' 
#' Normalization of the OSA distance/similarity to the unit interval 
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
#' symmetric in \eqn{x} and \eqn{y}. The OSA distance is not a proper metric 
#' as it does not satisfy the triangle inequality. The Damerau-Levenshtein 
#' distance is closely related---it allows the same edit operations as OSA, 
#' but removes the requirement that no substring can be edited more than once. 
#' 
#' @param deletion positive cost associated with deletion of a character
#'   or sequence element. Defaults to unit cost.
#' @param insertion positive cost associated insertion of a character 
#'   or sequence element. Defaults to unit cost.
#' @param substitution positive cost associated with substitution of a 
#'   character or sequence element. Defaults to unit cost.
#' @param transposition positive cost associated with transposing (swapping) 
#'   a pair of characters or sequence elements. Defaults to unit cost.
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
#' An `OSA` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @references 
#' Boytsov, L. (2011), "Indexing methods for approximate dictionary searching: 
#' Comparative analysis", \emph{ACM J. Exp. Algorithmics} \strong{16}, 
#' Article 1.1.
#' 
#' Navarro, G. (2001), "A guided tour to approximate string matching", 
#' \emph{ACM Computing Surveys (CSUR)}, \strong{33}(1), 31-88.
#' 
#' Yujian, L. & Bo, L. (2007), "A Normalized Levenshtein Distance Metric",
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence} 
#' \strong{29}: 1091–1095.
#' 
#' @examples
#' ## Compare strings with a transposition error
#' x <- "plauge"; y <- "plague"
#' OSA()(x, y) != Levenshtein()(x, y)
#' 
#' ## Unlike Damerau-Levenshtein, OSA does not allow a substring to be 
#' ## edited more than once
#' x <- "ABC"; y <- "CA"
#' OSA()(x, y) != DamerauLevenshtein()(x, y)
#' 
#' ## Compare car names using normalized OSA similarity
#' data(mtcars)
#' cars <- rownames(mtcars)
#' pairwise(OSA(similarity = TRUE, normalize=TRUE), cars)
#' 
#' @seealso Other edit-based comparators include [`Hamming`], [`LCS`], 
#' [`Levenshtein`] and [`DamerauLevenshtein`].
#' 
#' @export
OSA <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                transposition = 1.0, normalize = FALSE, similarity = FALSE, 
                ignore_case = FALSE, use_bytes = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$similarity <- similarity
  arguments$distance <- !similarity
  arguments$symmetric <- insertion == deletion
  do.call("new", append("OSA", arguments))
}