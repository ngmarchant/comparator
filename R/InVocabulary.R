#' @include StringComparator.R PairwiseMatrix.R
NULL

setClass("InVocabulary", contains = c("StringComparator"), 
         slots = c(
           vocab = "character",
           both_in_distinct = "numeric", 
           both_in_same = "numeric", 
           one_in = "numeric", 
           none_in = "numeric"
         ),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           vocab = character(),
           both_in_distinct = 0.7, 
           both_in_same = 1.0, 
           one_in = 1.0, 
           none_in = 1.0,
           symmetric = TRUE
         ),
         validity = function(object) {
           errs <- character()
           if (length(object@both_in_distinct) != 1)
             errs <- c(errs, "`both_in_distinct` must be a numeric vector of length 1")
           if (length(object@both_in_same) != 1)
             errs <- c(errs, "`both_in_same` must be a numeric vector of length 1")
           if (length(object@one_in) != 1)
             errs <- c(errs, "`one_in` must be a numeric vector of length 1")
           if (length(object@none_in) != 1)
             errs <- c(errs, "`none_in` must be a numeric vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (object@distance)
             errs <- c(errs, "`distance` must be FALSE")
           if (object@similarity)
             errs <- c(errs, "`similarity` must be FALSE")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' In-Vocabulary Comparator
#' 
#' @description
#' Compares a pair of strings \eqn{x} and \eqn{y} using a reference vocabulary. 
#' Different scores are returned depending on whether both/one/neither of 
#' \eqn{x} and \eqn{y} are in the reference vocabulary.
#' 
#' @details 
#' This comparator is not intended to produce useful scores on its own. 
#' Rather, it is intended to produce multiplicative factors which can be 
#' applied to other similarity/distance scores. It is particularly useful 
#' for comparing names when a reference list (vocabulary) of known names is 
#' available. For example, it can be configured to down-weight the similarity 
#' scores of distinct (known) names like "Roberto" and "Umberto" which are 
#' semantically very different, but deceptively similar in terms of edit 
#' distance. The normalized Levenshtein similarity for these two names is 75%, 
#' but their similarity can be reduced to 53% if multiplied by the score 
#' from this comparator using the default settings.
#' 
#' @param vocab a vector containing in-vocabulary (known) strings. Any strings 
#'   not in this vector are out-of-vocabulary (unknown).
#' @param both_in_distinct score to return if the pair of values being 
#'   compared are both in `vocab` and distinct. Defaults to 0.7, which would 
#'   is appropriate for multiplying by similarity scores. If multiplying 
#'   by distance scores, a value greater than 1 is likely to be more 
#'   appropriate.
#' @param both_in_same score to return if the pair of values being 
#'   compared are both in `vocab` and identical. Defaults to 1.0, which 
#'   would leave another score unchanged when multiplied by this one.
#' @param one_in score to return if only one of the pair of values being 
#'   compared is in `vocab`. Defaults to 1.0, which would leave another 
#'   score unchanged when multiplied by this one.
#' @param none_in score to return if none of the pair of values being 
#'   compared is in `vocab`. Defaults to 1.0, which would leave another 
#'   score unchanged when multiplied by this one.
#' @param ignore_case a logical. If TRUE, case is ignored when comparing the 
#'   strings.
#' 
#' @return
#' An `InVocabulary` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @examples
#' ## Compare names with possible typos using a reference of known names
#' known_names <- c("Roberto", "Umberto", "Alberto", "Emberto", "Norberto", "Humberto")
#' m1 <- InVocabulary(known_names)
#' m2 <- Levenshtein(similarity = TRUE, normalize = TRUE)
#' x <- "Emberto"
#' y <- c("Enberto", "Umberto")
#' # "Emberto" and "Umberto" are likely to refer to distinct people (since 
#' # they are known distinct names) so their Levenshtein similarity is 
#' # downweighted to 0.61. "Emberto" and "Enberto" may refer to the same 
#' # person (likely typo), so their Levenshtein similarity of 0.87 is not 
#' # downweighted.
#' similarities <- m1(x, y) * m2(x, y)
#' 
#' @export
InVocabulary <- function(vocab, both_in_distinct = 0.7, both_in_same = 1.0, 
                         one_in = 1.0, none_in = 1.0, ignore_case = FALSE) {
  arguments <- c(as.list(environment()))
  do.call("new", append("InVocabulary", arguments))
}

#' @describeIn elementwise Specialization for [`InVocabulary`] where `x` and 
#' `y` are vectors of strings to compare.
setMethod(elementwise, signature = c(comparator = "InVocabulary", x = "vector", y = "vector"), 
          function(comparator, x, y, ...) {
            vocab <- comparator@vocab
            
            if (comparator@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
              vocab <- tolower(vocab)
            }
            
            x_known <- x %in% vocab
            y_known <- y %in% vocab
            
            distinct <- x != y
            both_in <- x_known & y_known
            
            out <- rep_len(comparator@none_in, length(x))
            out[x_known | y_known] <- comparator@one_in
            out[both_in] <- comparator@both_in_same
            out[both_in & distinct] <- comparator@both_in_distinct
            
            out
          }
)


#' @describeIn pairwise Specialization for [`InVocabulary`] where `x` and `y` 
#' are vectors of strings to compare.
setMethod(pairwise, signature = c(comparator = "InVocabulary", x = "vector", y = "vector"), 
          function(comparator, x, y, return_matrix, ...) {
            vocab <- comparator@vocab
            
            if (comparator@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
              vocab <- tolower(vocab)
            }
            
            x_known <- x %in% vocab
            y_known <- y %in% vocab
            
            both_in <- matrix(x_known, nrow=length(x), ncol=length(y))
            both_in <- sweep(both_in, 2, y_known, FUN = "&")  
            
            distinct <- matrix(x, nrow=length(x), ncol=length(y))
            distinct <- sweep(distinct, 2, y, FUN = "!=")  
            
            scores <- matrix(comparator@none_in, nrow =length(x), ncol = length(y))
            scores[x_known | y_known] <- comparator@one_in
            scores[both_in] <- comparator@both_in_same
            scores[both_in & distinct] <- comparator@both_in_distinct
            
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @describeIn pairwise Specialization for [`InVocabulary`] where `x` is a 
#' vector of strings to compare among themselves.
setMethod(pairwise, signature = c(comparator = "InVocabulary", x = "vector", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(comparator, x, x, return_matrix)
          }
)