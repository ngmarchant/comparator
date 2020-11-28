#' @include abstract.R
NULL

def_attr_lcs <- list(
  normalize = FALSE,
  symmetric = TRUE,
  distance = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_lcs)] <- def_attr_lcs

setClass("LCS", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(normalize = "logical"),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("LCS", attrs)), 
                                    def_attr_lcs)),
         validity = function(object) {
           errs <- character()
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Longest Common Substring (LCS) Distance
#' 
#' @description 
#' #TODO
#' 
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
LCS <- function(normalize = FALSE, similarity = FALSE, ignore_case = FALSE, 
                use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  arguments <- list("LCS", ".Data" = elementwise_cpp_builder("LCS", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}