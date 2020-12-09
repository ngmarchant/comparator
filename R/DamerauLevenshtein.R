#' @include Levenshtein.R
NULL

setClass("DamerauLevenshtein", contains = "Levenshtein", 
         slots = c(transposition = "numeric"),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           transposition = 1.0
         ),
         validity = function(object) {
           errs <- character()
           # Other validity checks are taken care of in Levenshtein
           if (object@transposition < 0 | length(object@transposition) != 1)
             errs <- c(errs, "`transposition` must be a non-negative numeric vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Damerau-Levenshtein String/Sequence Comparator
#' 
#' @description 
#' The Damerau-Levenshtein distance between two strings/sequences \eqn{x} 
#' and \eqn{y} is the minimum cost of operations (insertions, deletions, 
#' substitutions or transpositions) required to transform \eqn{x} 
#' into \eqn{y}. It differs from the Levenshtein distance by including 
#' _transpositions_ (swaps) among the allowable operations.
#' 
#' @details 
#' For simplicity we assume `x` and `y` are strings in this section,
#' however the comparator is also implemented for more general sequences.
#' 
#' A Damerau-Levenshtein similarity is returned if `similarity = TRUE`, which 
#' is defined as 
#' \deqn{\mathrm{sim}(x, y) = \frac{w_d |x| + w_i |y| - \mathrm{dist}(x, y)}{2},}{sim(x, y) = (w_d |x| + w_i |y| - dist(x, y))/2}
#' where \eqn{|x|}, \eqn{|y|} are the number of characters in \eqn{x} and 
#' \eqn{y} respectively, \eqn{\mathrm{dist}}{dist} is the Damerau-Levenshtein 
#' distance, \eqn{w_d} is the cost of a deletion and \eqn{w_i} is the cost of 
#' an insertion.
#' 
#' Normalization of the Damerau-Levenshtein distance/similarity to the unit 
#' interval is also supported by setting `normalize = TRUE`. The normalization 
#' approach follows Yujian and Bo (2007), and ensures that the distance 
#' remains a metric when the costs of insertion \eqn{w_i} and deletion 
#' \eqn{w_d} are equal. The normalized distance \eqn{\mathrm{dist}_n}{dist_n} 
#' is defined as
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
#' A `DamerauLevenshtein` instance is returned, which is an S4 class inheriting 
#' from [`Levenshtein`].
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
#' \strong{29}, 1091-1095.
#' 
#' @examples 
#' ## The Damerau-Levenshtein distance reduces to ordinary Levenshtein distance 
#' ## when the cost of transpositions is high
#' x <- "plauge"; y <- "plague"
#' DamerauLevenshtein(transposition = 100)(x, y) == Levenshtein()(x, y)
#' 
#' ## Compare car names using normalized Damerau-Levenshtein similarity
#' data(mtcars)
#' cars <- rownames(mtcars)
#' pairwise(DamerauLevenshtein(similarity = TRUE, normalize=TRUE), cars)
#' 
#' ## Compare sequences using Damerau-Levenshtein distance
#' x <- c("G", "T", "G", "C", "T", "G", "G", "C", "C", "C", "A", "T")
#' y <- c("G", "T", "G", "C", "G", "T", "G", "C", "C", "C", "A", "T")
#' DamerauLevenshtein()(list(x), list(y))
#' 
#' @seealso Other edit-based comparators include [`Hamming`], [`LCS`], 
#' [`Levenshtein`] and [`OSA`].
#' 
#' @rdname DamerauLevenshtein
#' @export
DamerauLevenshtein <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                               transposition = 1.0, normalize = FALSE, similarity = FALSE, 
                               ignore_case = FALSE, use_bytes = FALSE) {
  arguments <- c(as.list(environment()))
  arguments$similarity <- similarity
  arguments$distance <- !similarity
  arguments$symmetric <- deletion == insertion
  arguments$tri_inequal <- deletion == insertion & !similarity
  do.call("new", append("DamerauLevenshtein", arguments))
}