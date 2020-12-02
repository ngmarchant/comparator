#' @include abstract.R
NULL

def_attr_osa <- list(
  deletion = 1.0,
  insertion = 1.0, 
  substitution = 1.0,
  transposition = 1.0,
  normalize = FALSE,
  symmetric = TRUE,
  distance = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_osa)] <- def_attr_osa

setClass("OSA", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(
           deletion = "numeric", 
           insertion = "numeric", 
           substitution = "numeric",
           transposition = "numeric",
           normalize = "logical"
         ),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("OSA", attrs)), 
                                    def_attr_osa)),
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


#' Optimal String Alignment (OSA) Distance
#' 
#' @description 
#' The Optimal String Alignment distance between two strings is the minimum 
#' cost of single-character operations (insertions, deletions, substitutions or 
#' transpositions) required to transform one string into the other. TODO - 
#' explain difference from Damerau-Levenshtein.
#' 
#' @note The Optimal String Alignment is not a proper distance metric as it 
#'   does not satisfy the triangle inequality.
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
OSA <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                transposition = 1.0, normalize = FALSE, similarity = FALSE, 
                ignore_case = FALSE, use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  attrs$symmetric <- insertion == deletion
  arguments <- list("OSA", ".Data" = elementwise_cpp_builder("OSA", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}