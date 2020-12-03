#' @include StringMeasure.R
NULL

def_attr_lookup <- list(
  lookup_table = data.frame(key1 = character(), key2 = character(), value = numeric()),
  default_match = 0.0, 
  default_nonmatch = Inf,
  symmetric = TRUE
)

attrs <- attributes(getClassDef("StringMeasure")@prototype)[-1]
attrs[names(def_attr_lookup)] <- def_attr_lookup

elementwise_lookup_builder <- function(attrs) {
  function(x, y) {
    if (attrs$ignore_case) {
      x <- tolower(x)
      y <- tolower(y)
    }
    
    pairs <- data.frame(key1 = x, key2 = y, stringsAsFactors=FALSE)
    # workaround to preserve row-order
    pairs[['order']] <- seq_len(nrow(pairs))
    
    # fill in distances/similarities for combinations using lookup table
    pairs <- merge(pairs, attrs$lookup_table, by=c("key1", "key2"), all.x=TRUE, all.y=FALSE)
    pairs <- pairs[order(pairs[['order']]),]
    pairs[['order']] <- NULL
    
    # fill in distances/similarities for exact matches that were not in the lookup table
    both_observed <- !is.na(pairs$key1) & !is.na(pairs$key2)
    if (!is.na(attrs$default_match)) {
      exact_match <- (pairs$key1 == pairs$key2) & both_observed
      pairs[exact_match & is.na(pairs$value), "value"] <- attrs$default_match
    }
    
    # fill any remaining NAs with default distance/similarity if given
    if (!is.na(attrs$default_nonmatch)) {
      pairs[both_observed & is.na(pairs$value), "value"] <- attrs$default_nonmatch
    }
    
    # return only the distances/similarities
    pairs$value
  }
}

setClass("Lookup", contains = c("StringMeasure"), 
         slots = c(
           lookup_table = "data.frame", 
           default_match = "numeric", 
           default_nonmatch = "numeric"
         ),
         prototype = do.call(structure, 
                             append(c(.Data = elementwise_lookup_builder(attrs)), 
                                    def_attr_lookup)),
         validity = function(object) {
           errs <- character()
           if (!all(colnames(object@lookup_table) == c("key1", "key2", "value")))
             errs <- c(errs, "`lookup_table` has unexpected colnames")
           if (!is.numeric(object@lookup_table$value))
             errs <- c(errs, "value column of `lookup_table` must be numeric")
           if (length(object@default_match) != 1)
             errs <- c(errs, "`default_match` must be a numeric vector of length 1")
           if (length(object@default_nonmatch) != 1)
             errs <- c(errs, "`default_nonmatch` must be a numeric vector of length 1")
           if (object@tri_inequal)
             errs <- c(errs, "`tri_inequal` must be FALSE")
           ifelse(length(errs) == 0, TRUE, errs)
         })


#' Helper function to normalize lookup table
#' 
#' @return Normalized lookup table where the values columns are named "key1" and 
#'   "key2" and the measure column is named "value"
#' @keywords internal
#' @noRd
normalize_lookup_table <- function(lookup_table, values_colnames, measure_colname, symmetric, ignore_case) {
  
  lookup_table <- lookup_table[append(values_colnames, measure_colname)]
  colnames(lookup_table) <- c("key1", "key2", "value")
  
  if (ignore_case) {
    lookup_table[[1]] <- tolower(lookup_table[[1]])
    lookup_table[[2]] <- tolower(lookup_table[[2]])
  }
  
  if (symmetric) {
    # TODO: be more careful about not duplicating rows with v1 == v2
    # copy table with keys reversed
    reversed_table <- lookup_table[,c(2,1,3)]
    names(reversed_table) <- names(reversed_table)[c(2,1,3)]
    # combine table
    lookup_table <- do.call(rbind, list(lookup_table, reversed_table))
  }
  
  # ensure duplicate rows are removed
  duplicated_rows <- duplicated(lookup_table[,c(1, 2)])
  if (any(duplicated_rows)) warning("removing duplicated rows in `lookup_table`")
  lookup_table <- lookup_table[!duplicated_rows,]
  rm(duplicated_rows)
  
  return(lookup_table)
}


#' Lookup Measure
#' 
#' @description
#' This measure compares a pair of strings \eqn{x} and \eqn{y} by retrieving 
#' their distance/similarity from a provided lookup table. 
#' 
#' @details 
#' The lookup table should contain three columns corresponding to \eqn{x}, 
#' and \eqn{y} (`values_colnames` below) and the distance/similarity 
#' (`measure_colname` below). If a pair of values \eqn{x} and \eqn{y} is 
#' not in the lookup table, a default distance/similarity is returned 
#' depending on whether \eqn{x = y} (`default_match` below) or 
#' \eqn{x \neq y}{x ≠ y} (`default_nonmatch` below).
#' 
#' @param lookup_table data frame containing distances/similarities for 
#'   pairs of values
#' @param values_colnames character vector containing the colnames 
#'   corresponding to pairs of values (e.g. strings) in `lookup_table`
#' @param measure_colname name of column that contains distances/similarities 
#'   in `lookup_table`
#' @param default_match distance/similarity to use if the pair of values 
#'   match exactly and do not appear in `lookup_table`. Defaults to 0.0.
#' @param default_nonmatch distance/similarity to use if the pair of values are 
#'   not an exact match and do not appear in `lookup table`. Defaults to `NA`.
#' @param symmetric whether the underlying distance/similarity measure is 
#'   symmetric. If TRUE `lookup_table` need only contain entries for 
#'   one of the two pairs---i.e. an entry for value pair \eqn{(y, x)} is not 
#'   required if an entry for \eqn{(x, y)} is already present. 
#' @param ignore_case a logical. If TRUE, case is ignored when comparing the 
#'   strings.
#' 
#' @return 
#' A `Lookup` instance is returned, which is an S4 class inheriting from 
#' [`StringMeasure-class`].
#' 
#' @examples
#' ## Measure the distance between cities
#' lookup_table <- data.frame(x = c("Melbourne", "Melbourne", "Sydney"), 
#'                            y = c("Sydney", "Brisbane", "Brisbane"), 
#'                            dist = c(713.4, 1374.8, 732.5))
#' 
#' measure <- Lookup(lookup_table, c("x", "y"), "dist")
#' measure("Sydney", "Melbourne")
#' measure("Melbourne", "Perth")
#' 
#' @export
Lookup <- function(lookup_table, values_colnames, measure_colname, 
                   default_match = 0.0, default_nonmatch = NA_real_, 
                   symmetric = TRUE, ignore_case = FALSE, ...) {
  if (!is.data.frame(lookup_table)) 
    stop("`lookup_table` must be a data.frame")
  if (!is.character(measure_colname) || length(measure_colname) != 1)
    stop("`measure_colname` must be a character vector of length 1")
  if (!(measure_colname %in% colnames(lookup_table)))
    stop("`measure_colname` doesn't exist in `lookup_table`")
  if (!is.character(values_colnames) || length(values_colnames) != 2)
    stop("`values_colnames` must be a character vector of length 2")
  if (!all(values_colnames %in% colnames(lookup_table)))
    stop("`values_colnames` refers to columns not in `lookup_table`")
  if (!is.logical(symmetric) || length(symmetric) != 1)
    stop("`symmetric` must be a logical of length 1")
  
  lookup_table <- normalize_lookup_table(lookup_table, values_colnames, measure_colname, symmetric, ignore_case)
  
  attrs <- list(lookup_table = lookup_table, default_match = default_match, 
                default_nonmatch = default_nonmatch, symmetric = symmetric, 
                ignore_case = ignore_case)
  
  arguments <- list("Lookup", ".Data" = elementwise_lookup_builder(attrs))
  arguments <- append(arguments, attrs)
  do.call("new", arguments)
}

#' @export
setMethod(pairwise, signature = c(measure = "Lookup", x = "vector", y = "vector"), 
          function(measure, x, y, return_matrix, ...) {
            if (measure@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
            }
            
            pairs <- list(key1 = x, key2 = y)
            pairs <- expand.grid(pairs, stringsAsFactors=FALSE)
            pairs <- as.data.frame(pairs, stringsAsFactors=FALSE)
            
            # workaround to preserve row-order
            pairs[['order']] <- seq_len(nrow(pairs))
            
            # fill in distances for combinations using lookup table
            pairs <- merge(pairs, measure@lookup_table, by=c("key1", "key2"), all.x=TRUE, all.y=FALSE)
            pairs <- pairs[order(pairs[['order']]),]
            pairs[['order']] <- NULL
            
            # fill in distances/similarities for exact matches that were not in the lookup table
            both_observed <- !is.na(pairs$key1) & !is.na(pairs$key2)
            if (!is.na(measure@default_match)) {
              exact_match <- (pairs$key1 == pairs$key2) & both_observed
              pairs[exact_match & is.na(pairs$value), "value"] <- measure@default_match
            }
            
            # fill any remaining NAs with default distance/similarity if given
            if (!is.na(measure@default_nonmatch)) {
              pairs[both_observed & is.na(pairs$value), "value"] <- measure@default_nonmatch
            }
            
            # return only the distances/similarities
            if (return_matrix) {
              matrix(pairs$value, nrow = length(x), ncol = length(y))
            } else {
              as.PairwiseMatrix(pairs$value, c(length(x), length(y)), TRUE)
            }
          }
)

#' @export
setMethod(pairwise, signature = c(measure = "Lookup", x = "vector", y = "NULL"), 
          function(measure, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(measure, x, x, return_matrix)
          }
)