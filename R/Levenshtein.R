#' @include abstract.R
NULL

def_attr_levenshtein <- list(
  deletion = 1.0,
  insertion = 1.0, 
  substitution = 1.0,
  normalize = FALSE,
  symmetric = TRUE,
  distance = TRUE,
  tri_inequal = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_levenshtein)] <- def_attr_levenshtein

setClass("Levenshtein", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(
           deletion = "numeric", 
           insertion = "numeric", 
           substitution = "numeric",
           normalize = "logical"
         ),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("Levenshtein", attrs)), 
                                    def_attr_levenshtein)),
         validity = function(object) {
           errs <- character()
           equal_weights <- object@deletion == object@insertion && object@deletion == object@substitution
           if (!object@symmetric & equal_weights)
             errs <- c(errs, "`symmetric` must be TRUE when equal weights are used")
           if (object@symmetric & !equal_weights)
             errs <- c(errs, "`symmetric` must be FALSE when unequal weights are used")
           if (!object@tri_inequal & equal_weights & object@distance)
             errs <- c(errs, "`tri_inequal` must be TRUE when equal weights are used and `distance` is TRUE")
           if (object@tri_inequal & !equal_weights)
             errs <- c(errs, "`tri_inequal` must be FALSE when unequal weights are used")
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

#' Levenshtein (Edit) Distance
#' 
#' @description 
#' The Levenshtein distance between two strings is the minimum cost of 
#' single-character operations (insertions, deletions or substitutions) required 
#' to transform one string into the other.
#' 
#' @note If the costs of all operations are identical, Levenshtein distance is 
#'   a proper distance metric (it satisfies the triangle inequality).
#' 
#' @param deletion positive cost associated with deletion of a character. 
#'   Defaults to unit cost.
#' @param insertion positive cost associated insertion of a character.
#'   Defaults to unit cost.
#' @param substitution positive cost associated with substitution of a 
#'   character. Defaults to unit cost.
#' @param normalize a logical. If TRUE, distances are normalized to the 
#'   unit interval. Defaults to FALSE.
#' @param similarity a logical. If TRUE, similarity scores on the unit interval 
#'   are returned instead of distances. Defaults to FALSE. 
#' @param ignore_case a logical. If TRUE, case is ignored when computing the 
#'   distance. Defaults to FALSE.
#' @param use_bytes a logical. If TRUE, distances are computed byte-by-byte 
#'   rather than character-by-character.
#' 
#' @references Yujian, L. & Bo, L. A Normalized Levenshtein Distance Metric. 
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence} 
#' \strong{29}, 1091–1095 (2007).
#' 
#' @export
Levenshtein <- function(deletion = 1.0, insertion = 1.0, substitution = 1.0, 
                        normalize = FALSE, similarity = FALSE, 
                        ignore_case = FALSE, use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  same_weights <- all(deletion == insertion, deletion == substitution)
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  attrs$symmetric <- same_weights
  attrs$tri_inequal <- same_weights & !similarity
  arguments <- list("Levenshtein", ".Data" = elementwise_cpp_builder("Levenshtein", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}