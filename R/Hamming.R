#' @include abstract.R
NULL

def_attr_hamming <- list(
  normalize = FALSE,
  symmetric = TRUE,
  distance = TRUE,
  tri_inequal = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_hamming)] <- def_attr_hamming

setClass("Hamming", contains = c("StringMeasure", "CppMeasure"), 
         slots = c(constant = "numeric", 
                   normalize = "logical"), 
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_cpp_builder("Hamming", attrs)), def_attr_hamming)),
         validity = function(object) {
           errs <- character()
           if (length(object@normalize) != 1)
             errs <- c(errs, "`normalize` must be a logical vector of length 1")
           if (!object@symmetric)
             errs <- c(errs, "`symmetric` must be TRUE")
           if (!(object@similarity | object@distance))
             errs <- c(errs, "one of `similarity` or `distance` must be TRUE")
           if (object@tri_inequal & (object@normalize | object@similarity))
             errs <- c(errs, "`tri_inequal` must be FALSE when `normalize` or `similarity` is TRUE")
           if (!object@tri_inequal & !object@normalize & object@distance)
             errs <- c(errs, "`tri_inequal` must be TRUE when `normalize` is FALSE and `distance` is TRUE")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' Hamming Distance
#' 
#' @description TODO
#' 
#' @note Unlike other edit distances, this is not a metric if normalize is TRUE
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
Hamming <- function(normalize = FALSE, similarity = FALSE, ignore_case = FALSE, 
                    use_bytes = FALSE, ...) {
  attrs <- c(as.list(environment()), list(...))
  attrs$similarity <- similarity
  attrs$distance <- !similarity
  attrs$tri_inequal <- !similarity & !normalize
  arguments <- list("Hamming", ".Data" = elementwise_cpp_builder("Hamming", attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}