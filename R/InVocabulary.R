#' @include StringMeasure.R PairwiseMatrix.R
NULL

def_attr_invocab <- list(
  vocab = character(),
  both_in_distinct = 0.7, 
  both_in_same = 1.0, 
  one_in = 1.0, 
  none_in = 1.0,
  symmetric = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_invocab)] <- def_attr_invocab

elementwise_invocab_builder <- function(attrs) {
  function(x, y) {
    vocab <- attrs$vocab
    
    if (attrs$ignore_case) {
      x <- tolower(x)
      y <- tolower(y)
      vocab <- tolower(vocab)
    }
    
    x_known <- x %in% vocab
    y_known <- y %in% vocab
    
    distinct <- x != y
    both_in <- x_known & y_known
    
    out <- rep_len(attrs$none_in, length(x))
    out[x_known | y_known] <- attrs$one_in
    out[both_in] <- attrs$both_in_same
    out[both_in & distinct] <- attrs$both_in_distinct
    
    out
  }
}

setClass("InVocabulary", contains = c("StringMeasure"), 
         slots = c(
           vocab = "character",
           both_in_distinct = "numeric", 
           both_in_same = "numeric", 
           one_in = "numeric", 
           none_in = "numeric"
         ),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_invocab_builder(attrs)), 
                                    def_attr_invocab)),
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


#' In-Vocabulary Measure
#' 
#' @description
#' This measure compares strings using a reference vocabulary. Different scores 
#' are returned depending on whether both/one/neither of the strings are 
#' in the reference vocabulary.
#' 
#' @details This measure is not intended to serve as a useful comparison 
#'   measure on its own. Rather, it is intended to produce multiplicative 
#'   factors which can be applied to another similarity/distance measure. 
#'   It is particularly useful for comparing names when a reference list 
#'   (vocabulary) of known names is available. For example, it can be 
#'   configured to down-weight the similarity of distinct (known) names like 
#'   "Roberto" and "Umberto" which are semantically very different, but 
#'   deceptively similar in terms of edit distance. The normalized Levenshtein 
#'   similarity for these two names is 75%, but their similarity can be 
#'   reduced to 53% if multiplied by this measure with the default settings.
#' 
#' @param vocab a vector containing in-vocabulary (known) strings. Any strings 
#'   not in this vector are out-of-vocabulary (unknown).
#' @param both_in_distinct score to return if the pair of values being 
#'   compared are both in `vocab` and distinct. Defaults to 0.7, which would 
#'   is appropriate for multiplying by a similarity measure. If multiplying 
#'   by a distance measure, a value greater than 1 is likely to be more 
#'   appropriate.
#' @param both_in_same score to return if the pair of values being 
#'   compared are both in `vocab` and identical. Defaults to 1.0, which 
#'   would leave another measure unchanged when multiplied by this one.
#' @param one_in score to return if only one of the pair of values being 
#'   compared is in `vocab`. Defaults to 1.0, which would leave another 
#'   measure unchanged when multiplied by this one.
#' @param none_in score to return if none of the pair of values being 
#'   compared is in `vocab`. Defaults to 1.0, which would leave another 
#'   measure unchanged when multiplied by this one.
#' @param ignore_case a logical. If TRUE, case is ignored when computing the 
#'   distance/similarity.
#' 
#' @export
InVocabulary <- function(vocab, both_in_distinct = 0.7, both_in_same = 1.0, 
                         one_in = 1.0, none_in = 1.0, ignore_case = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  arguments <- list("InVocabulary", ".Data" = elementwise_invocab_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
setMethod(pairwise, signature = c(measure = "InVocabulary", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            vocab <- measure@vocab
            
            if (measure@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
              vocab <- tolower(vocab)
            }
            
            x_known <- x %in% vocab
            y_known <- y %in% vocab
            
            both_in <- matrix(x_known, nrow=length(x), ncol=length(y))
            both_in <- sweep(both_in, 2, y_known, FUN = "&")  
            
            distinct <- matrix(v1, nrow=length(v1), ncol=length(v2))
            distinct <- sweep(distinct, 2, v2, FUN = "!=")  
            
            scores <- matrix(measure@none_in, nrow =length(x), ncol = length(y))
            scores[x_known | y_known] <- measure@one_in
            scores[both_in] <- measure@both_in_same
            scores[both_in & distinct] <- measure@both_in_distinct
            
            if (!return_matrix) scores <- as.PairwiseMatrix(scores)
            scores
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "InVocabulary", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)