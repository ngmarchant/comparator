#' @include Measure.R
NULL

setClass("NumericMeasure", 
         prototype = structure(
           .Data = function(x, y) 0,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("VIRTUAL", "Measure"), 
         validity = function(object) {
           errs <- character()
           ifelse(length(errs) == 0, TRUE, errs)
         })