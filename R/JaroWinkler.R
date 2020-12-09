#' @include Jaro.R
NULL

setClass("JaroWinkler", contains = "Jaro", 
         slots = c(p = "numeric", 
                   threshold = "numeric", 
                   max_prefix = "integer"), 
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           p = 0.1,
           threshold = 0.7,
           max_prefix = 4L
         ),
         validity = function(object) {
           errs <- character()
           mp <- object@max_prefix
           if (length(mp) != 1 | mp < 0)
             errs <- c(errs, "`max_prefix` must be a non-negative numeric vector of length 1")
           if (length(object@p) != 1)
             errs <- c(errs, "`p` must be a numeric vector of length 1")
           if (object@p < 0 | object@p > ifelse(mp == 0, 1, 1/mp))
             errs <- c(errs, "`p` must be in the interval `[0, 1/max_prefix]`")
           if (length(object@threshold) != 1)
             errs <- c(errs, "`threshold` must be a numeric vector of length 1")
           if (object@threshold < 0 | object@threshold >= 1)
             errs <- c(errs, "`threshold` must be in the interval [0, 1]")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Jaro-Winkler String/Sequence Comparator
#' 
#' @description
#' The Jaro-Winkler comparator is a variant of the [`Jaro`] comparator which 
#' boosts the similarity score for strings/sequences with matching prefixes. 
#' It was developed for comparing names at the U.S. Census Bureau.
#' 
#' @details 
#' For simplicity we assume `x` and `y` are strings in this section,
#' however the comparator is also implemented for more general sequences.
#' 
#' The Jaro-Winkler similarity (computed when `similarity = TRUE`) is 
#' defined in terms of the [`Jaro`] similarity. If the Jaro similarity 
#' \eqn{sim_J(x,y)} between strings \eqn{x} and \eqn{y} exceeds a 
#' user-specified threshold \eqn{0 \leq \tau \leq 1}{0 <= threshold <= 1}, 
#' the similarity score is boosted in proportion to the number of matching 
#' characters in the prefixes of \eqn{x} and \eqn{y}. More precisely, the 
#' Jaro-Winkler similarity is defined as:
#' \deqn{\mathrm{sim}_{JW}(x, y) = \mathrm{sim}_J(x, y) + \min(c(x, y), l) p (1 - \mathrm{sim}_J(x, y)),}{sim_{JW}(x, y) = sim_J(x, y) + min(c(x, y), l) p (1 - sim_J(x, y)),}
#' where \eqn{c(x,y)} is the length of the common prefix, \eqn{l \geq 0}{l >= 0}
#' is a user-specified upper bound on the prefix size, and 
#' \eqn{0 \leq p \leq 1/l}{0 <= p <= 1/l} is a scaling factor.
#' 
#' The Jaro-Winkler distance is computed when `similarity = FALSE` and is 
#' defined as 
#' \deqn{\mathrm{dist}_{JW}(x, y) = 1 - \mathrm{sim}_{JW}(x, y).}{dist_{JW}(x, y) = 1 - sim_{JW}(x, y).}
#' 
#' @note Like the Jaro distance, the Jaro-Winkler distance is not a metric as 
#'   it does not satisfy the identity axiom.
#' 
#' @param p a non-negative numeric scalar no larger than 1/max_prefix. 
#'   Similarity scores eligible for boosting are scaled by this factor.
#' @param threshold a numeric scalar on the unit interval. Jaro similarities 
#'   greater than this value are boosted based on matching characters in the 
#'   prefixes of both strings. Jaro similarities below this value are 
#'   returned unadjusted. Defaults to 0.7.
#' @param max_prefix a non-negative integer scalar, specifying the size of 
#'   the prefix to consider for boosting. Defaults to 4 (characters).
#' @param similarity a logical. If TRUE, similarity scores are returned 
#'   (default), otherwise distances are returned (see definition under Details).
#' @param ignore_case a logical. If TRUE, case is ignored when comparing  
#'   strings.
#' @param use_bytes a logical. If TRUE, strings are compared byte-by-byte 
#'   rather than character-by-character.
#' 
#' @return 
#' A `JaroWinkler` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @examples
#' ## Compare names
#' JaroWinkler()("Martha", "Mathra")
#' JaroWinkler()("Eileen", "Phyllis")
#' 
#' ## Reduce the threshold for boosting
#' x <- "Matthew"
#' y <- "Martin"
#' JaroWinkler()(x, y) < JaroWinkler(threshold = 0.5)(x, y)
#'
#' @seealso 
#' This comparator reduces to the [`Jaro`] comparator when `max_prefix = 0L` 
#' or `threshold = 0.0`.
#' 
#' @references 
#' Jaro, M. A. (1989), "Advances in Record-Linkage Methodology as Applied to 
#' Matching the 1985 Census of Tampa, Florida", \emph{Journal of the American 
#' Statistical Association} \strong{84}(406), 414-420.
#' 
#' Winkler, W. E. (2006), "Overview of Record Linkage and Current Research 
#' Directions", Tech. report. Statistics #2006-2. Statistical Research 
#' Division, U.S. Census Bureau. 
#' 
#' Winkler, W., McLaughlin G., Jaro M. and Lynch M. (1994), [strcmp95.c](https://web.archive.org/web/20100227020019/http://www.census.gov/geo/msb/stand/strcmp.c), 
#' Version 2. United States Census Bureau. 
#' 
#' @export
JaroWinkler <- function(p = 0.1, threshold = 0.7, max_prefix = 4L, 
                        similarity = TRUE, ignore_case = FALSE, 
                        use_bytes = FALSE) {
  if (max_prefix != as.integer(max_prefix)) stop("`max_prefix` must be a non-negative integer")
  max_prefix <- as.integer(max_prefix)
  distance <- !similarity
  arguments <- c(as.list(environment()))
  do.call("new", append("JaroWinkler", arguments))
}