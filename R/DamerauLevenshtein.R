#' @include Levenshtein.R
NULL

def_attr_dl <- list(
  transposition = 1.0
)

attrs <- attributes(getClassDef("Levenshtein")@prototype)[-1]
attrs[names(def_attr_dl)] <- def_attr_dl

setClass("DamerauLevenshtein", contains = "Levenshtein", 
         slots = c(transposition = "numeric"),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("DamerauLevenshtein", attrs)), 
                                    def_attr_dl)),
         validity = function(object) {
           errs <- character()
           # Other validity checks are taken care of in Levenshtein
           if (object@transposition < 0 | length(object@transposition) != 1)
             errs <- c(errs, "`transposition` must be a non-negative numeric vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Damerau-Levenshtein Distance
#' 
#' @description 
#' The Damerau-Levenshtein distance between two strings is the minimum cost of 
#' single-character operations (insertions, deletions, substitutions or 
#' transpositions) required to transform one string into the other. It differs 
#' from the Levenshtein distance by adding _transpositions_ (swaps) among the 
#' allowable operations.
#' 
#' @note If the costs of all operations are identical, Damerau-Levenshtein 
#'   distance is a proper distance metric (it satisfies the triangle 
#'   inequality).
#' 
#' @param deletion positive cost associated with deletion of a character. 
#'   Defaults to unit cost.
#' @param insertion positive cost associated insertion of a character.
#'   Defaults to unit cost.
#' @param substitution positive cost associated with substitution of a 
#'   character. Defaults to unit cost.
#' @param transposition positive cost associated with transposing (swapping) 
#'   a pair of characters. Defaults to unit cost.
#' @param normalize a logical. If TRUE, distances are normalized to the 
#'   unit interval. Defaults to FALSE.
#' @param similarity a logical. If TRUE, similarity scores on the unit interval 
#'   are returned instead of distances. Defaults to FALSE. 
#' @param ignore_case a logical. If TRUE, case is ignored when computing the 
#'   distance. Defaults to FALSE.
#' @param use_bytes a logical. If TRUE, distances are computed byte-by-byte 
#'   rather than character-by-character.
#' 
#' @export
DamerauLevenshtein <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                               transposition = 1.0, normalize = FALSE, similarity = FALSE, 
                               ignore_case = FALSE, use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  attrs$symmetric <- deletion == insertion
  attrs$tri_inequal <- deletion == insertion & !similarity
  arguments <- list("DamerauLevenshtein", ".Data" = elementwise_cpp_builder("DamerauLevenshtein", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}