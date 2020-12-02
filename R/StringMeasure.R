#' @include Measure.R
NULL

setClass("StringMeasure", 
         slots = c(
           ignore_case = "logical", 
           use_bytes = "logical"
         ), 
         prototype = structure(
           .Data = function(x, y) 0,
           ignore_case = FALSE,
           use_bytes = FALSE,
           symmetric = FALSE,
           distance = FALSE,
           similarity = FALSE,
           tri_inequal = FALSE
         ),
         contains = c("VIRTUAL", "Measure"), 
         validity = function(object) {
           errs <- character()
           if (length(object@ignore_case) != 1)
             errs <- c(errs, "`ignore_case` must be a logical vector of length 1")
           if (length(object@use_bytes) != 1) 
             errs <- c(errs, "`use_bytes` must be a logical vector of length 1")
           ifelse(length(errs) == 0, TRUE, errs)
         })