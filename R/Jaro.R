#' @include StringComparator.R CppSeqComparator.R
NULL

setClass("Jaro", contains = c("CppSeqComparator", "StringComparator"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           symmetric = TRUE,
           similarity = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Jaro String/Sequence Comparator
#' 
#' @description
#' Compares a pair of strings/sequences `x` and `y` based on the number of 
#' greedily-aligned characters/sequence elements and the number of 
#' transpositions. It was developed for comparing names at the U.S. Census 
#' Bureau.
#' 
#' @details 
#' For simplicity we assume `x` and `y` are strings in this section,
#' however the comparator is also implemented for more general sequences.
#' 
#' When `similarity = TRUE` (default), the Jaro similarity is computed as
#' \deqn{\mathrm{sim}(x, y) = \frac{1}{3}\left(\frac{m}{|x|} + \frac{m}{|y|} + \frac{m - \lfloor \frac{t}{2} \rfloor}{m}\right)}{sim(x, y) = (1/3)(m/|x| + m/|y| + (m-floor(t/2)/m)}
#' where \eqn{m} is the number of "matching" characters (defined below), 
#' \eqn{t} is the number of "transpositions", and \eqn{|x|,|y|} are the 
#' lengths of the strings \eqn{x} and \eqn{y}. The similarity takes on values 
#' in the range \eqn{[0, 1]}, where 1 corresponds to a perfect match.
#' 
#' The number of "matching" characters \eqn{m} is computed using a greedy 
#' alignment algorithm. The algorithm iterates over the characters in \eqn{x}, 
#' attempting to align the \eqn{i}-th character \eqn{x_i} with the first 
#' matching character in \eqn{y}. When looking for matching characters in 
#' \eqn{y}, the algorithm only considers previously un-matched characters 
#' within a window 
#' \eqn{[\max(0, i - w), \min(|y|, i + w)]}{[max(0, i - w), min(|y|, i + w)]} 
#' where \eqn{w = \left\lfloor \frac{\max(|x|, |y|)}{2} \right\rfloor - 1}{w = floor(max(|x|, |y|)/2) - 1}.
#' The alignment process yields a subsequence of matching characters from 
#' \eqn{x} and \eqn{y}. The number of "transpositions" \eqn{t} is defined to 
#' be the number of positions in the subsequence of \eqn{x} which are 
#' misaligned with the corresponding position in \eqn{y}.
#' 
#' When `similarity = FALSE`, the Jaro distance is computed as 
#' \deqn{\mathrm{dist}(x,y) = 1 - \mathrm{sim}(x,y).}{dist(x,y) = 1 - sim(x,y).}
#' 
#' @note The Jaro distance is not a metric, as it does not satisfy the 
#'   identity axiom \eqn{\mathrm{dist}(x,y) = 0 \Leftrightarrow x = y.}{dist(x, y) = 0 <=> x = y.}
#' 
#' @param similarity a logical. If TRUE, similarity scores are returned 
#'   (default), otherwise distances are returned (see definition under Details).
#' @param ignore_case a logical. If TRUE, case is ignored when comparing  
#'   strings.
#' @param use_bytes a logical. If TRUE, strings are compared byte-by-byte 
#'   rather than character-by-character.
#' 
#' @return 
#' A `Jaro` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @examples
#' ## Compare names
#' Jaro()("Martha", "Mathra")
#' Jaro()("Eileen", "Phyllis")
#' 
#' @seealso 
#' The [`JaroWinkler`] comparator modifies the [`Jaro`] comparator by 
#' boosting the similarity score for strings/sequences that have matching 
#' prefixes.
#' 
#' @references 
#' Jaro, M. A. (1989), "Advances in Record-Linkage Methodology as Applied to 
#' Matching the 1985 Census of Tampa, Florida", \emph{Journal of the American 
#' Statistical Association} \strong{84}(406), 414-420.
#' 
#' @export
Jaro <- function(similarity = TRUE, ignore_case = FALSE, use_bytes = FALSE) {
  distance <- !similarity
  arguments <- c(as.list(environment()))
  do.call("new", append("Jaro", arguments))
}