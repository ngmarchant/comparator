#' @include abstract.R
NULL

def_attr_lcs <- list(
  normalize = FALSE,
  symmetric = TRUE,
  distance = TRUE, 
  tri_inequal = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_lcs)] <- def_attr_lcs

setClass("LCS", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(
           deletion = "numeric", 
           insertion = "numeric", 
           normalize = "logical"),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("LCS", attrs)), 
                                    def_attr_lcs)),
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
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Longest Common Subsequence (LCS) Distance
#' 
#' @description 
#' #TODO
#' 
#' @param deletion positive cost associated with deletion of a character. 
#'   Defaults to unit cost.
#' @param insertion positive cost associated insertion of a character.
#'   Defaults to unit cost.
#' @param normalize a logical. If TRUE, distances are normalized to the 
#'   unit interval. Defaults to FALSE.
#' @param similarity a logical. If TRUE, similarity scores on the unit interval 
#'   are returned instead of distances. Defaults to FALSE. 
#' @param ignore_case a logical. If TRUE, case is ignored when computing the 
#'   distance. Defaults to FALSE.
#' @param use_bytes a logical. If TRUE, the measure is computed byte-by-byte 
#'   rather than character-by-character.
#' 
#' @export
LCS <- function(deletion = 1.0, insertion = 1.0, normalize = FALSE, similarity = FALSE, 
                ignore_case = FALSE, use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  attrs$symmetric <- deletion == insertion
  attrs$tri_inequal <- deletion == insertion & !similarity
  arguments <- list("LCS", ".Data" = elementwise_cpp_builder("LCS", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}