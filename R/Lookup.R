#' @include StringComparator.R
NULL

setClass("Lookup", contains = c("StringComparator"), 
         slots = c(
           lookup_table = "data.frame", 
           default_match = "numeric", 
           default_nonmatch = "numeric"
         ),
         prototype = structure(
           .Data = function(x, y, ...) elementwise(sys.function(), x, y, ...),
           lookup_table = data.frame(key1 = character(), key2 = character(), score = numeric()),
           default_match = 0.0, 
           default_nonmatch = Inf,
           symmetric = TRUE
         ), 
         validity = function(object) {
           errs <- character()
           if (!all(colnames(object@lookup_table) == c("key1", "key2", "score")))
             errs <- c(errs, "`lookup_table` has unexpected colnames")
           if (!is.numeric(object@lookup_table$score))
             errs <- c(errs, "score column of `lookup_table` must be numeric")
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
#'   "key2" and the score column is named "score"
#' @keywords internal
#' @noRd
normalize_lookup_table <- function(lookup_table, values_colnames, score_colname, symmetric, ignore_case) {
  
  lookup_table <- lookup_table[append(values_colnames, score_colname)]
  colnames(lookup_table) <- c("key1", "key2", "score")
  
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


#' Lookup String Comparator
#' 
#' @description
#' Compares a pair of strings \eqn{x} and \eqn{y} by retrieving 
#' their distance/similarity score from a provided lookup table. 
#' 
#' @details 
#' The lookup table should contain three columns corresponding to \eqn{x}, 
#' and \eqn{y} (`values_colnames` below) and the distance/similarity 
#' (`score_colname` below). If a pair of values \eqn{x} and \eqn{y} is 
#' not in the lookup table, a default distance/similarity is returned 
#' depending on whether \eqn{x = y} (`default_match` below) or 
#' \eqn{x \neq y}{x != y} (`default_nonmatch` below).
#' 
#' @param lookup_table data frame containing distances/similarities for 
#'   pairs of values
#' @param values_colnames character vector containing the colnames 
#'   corresponding to pairs of values (e.g. strings) in `lookup_table`
#' @param score_colname name of column that contains distances/similarities 
#'   in `lookup_table`
#' @param default_match distance/similarity to use if the pair of values 
#'   match exactly and do not appear in `lookup_table`. Defaults to 0.0.
#' @param default_nonmatch distance/similarity to use if the pair of values are 
#'   not an exact match and do not appear in `lookup table`. Defaults to `NA`.
#' @param symmetric whether the underlying distance/similarity scores are 
#'   symmetric. If TRUE `lookup_table` need only contain entries for 
#'   one of the two pairs---i.e. an entry for value pair \eqn{(y, x)} is not 
#'   required if an entry for \eqn{(x, y)} is already present. 
#' @param ignore_case a logical. If TRUE, case is ignored when comparing the 
#'   strings.
#' 
#' @return 
#' A `Lookup` instance is returned, which is an S4 class inheriting from 
#' [`StringComparator-class`].
#' 
#' @examples
#' ## Measure the distance between cities
#' lookup_table <- data.frame(x = c("Melbourne", "Melbourne", "Sydney"), 
#'                            y = c("Sydney", "Brisbane", "Brisbane"), 
#'                            dist = c(713.4, 1374.8, 732.5))
#' 
#' comparator <- Lookup(lookup_table, c("x", "y"), "dist")
#' comparator("Sydney", "Melbourne")
#' comparator("Melbourne", "Perth")
#' 
#' @export
Lookup <- function(lookup_table, values_colnames, score_colname, 
                   default_match = 0.0, default_nonmatch = NA_real_, 
                   symmetric = TRUE, ignore_case = FALSE) {
  if (!is.data.frame(lookup_table)) 
    stop("`lookup_table` must be a data.frame")
  if (!is.character(score_colname) || length(score_colname) != 1)
    stop("`score_colname` must be a character vector of length 1")
  if (!(score_colname %in% colnames(lookup_table)))
    stop("`score_colname` doesn't exist in `lookup_table`")
  if (!is.character(values_colnames) || length(values_colnames) != 2)
    stop("`values_colnames` must be a character vector of length 2")
  if (!all(values_colnames %in% colnames(lookup_table)))
    stop("`values_colnames` refers to columns not in `lookup_table`")
  if (!is.logical(symmetric) || length(symmetric) != 1)
    stop("`symmetric` must be a logical of length 1")
  
  lookup_table <- normalize_lookup_table(lookup_table, values_colnames, score_colname, symmetric, ignore_case)
  
  arguments <- list(lookup_table = lookup_table, default_match = default_match, 
                    default_nonmatch = default_nonmatch, symmetric = symmetric, 
                    ignore_case = ignore_case)
  
  do.call("new", append("Lookup", arguments))
}

#' @describeIn elementwise Specialization for a [`Lookup`] where `x` and `y` 
#' are vectors of strings to compare
setMethod(elementwise, signature = c(comparator = "Lookup", x = "vector", y = "vector"), 
          function(comparator, x, y, ...) {
            if (comparator@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
            }
            
            pairs <- data.frame(key1 = x, key2 = y, stringsAsFactors=FALSE)
            # workaround to preserve row-order
            pairs[['order']] <- seq_len(nrow(pairs))
            
            # fill in scores for combinations using lookup table
            pairs <- merge(pairs, comparator@lookup_table, by=c("key1", "key2"), all.x=TRUE, all.y=FALSE)
            pairs <- pairs[order(pairs[['order']]),]
            pairs[['order']] <- NULL
            
            # fill in scores for exact matches that were not in the lookup table
            both_observed <- !is.na(pairs$key1) & !is.na(pairs$key2)
            if (!is.na(comparator@default_match)) {
              exact_match <- (pairs$key1 == pairs$key2) & both_observed
              pairs[exact_match & is.na(pairs$score), "score"] <- comparator@default_match
            }
            
            # fill any remaining NAs with default distance/similarity if given
            if (!is.na(comparator@default_nonmatch)) {
              pairs[both_observed & is.na(pairs$score), "score"] <- comparator@default_nonmatch
            }
            
            # return only the scores
            pairs$score
          }
)


#' @describeIn pairwise Specialization for a [`Lookup`] where `x` and `y` are 
#' vectors of strings to compare
setMethod(pairwise, signature = c(comparator = "Lookup", x = "vector", y = "vector"), 
          function(comparator, x, y, return_matrix, ...) {
            if (comparator@ignore_case) {
              x <- tolower(x)
              y <- tolower(y)
            }
            
            pairs <- list(key1 = x, key2 = y)
            pairs <- expand.grid(pairs, stringsAsFactors=FALSE)
            pairs <- as.data.frame(pairs, stringsAsFactors=FALSE)
            
            # workaround to preserve row-order
            pairs[['order']] <- seq_len(nrow(pairs))
            
            # fill in scores for combinations using lookup table
            pairs <- merge(pairs, comparator@lookup_table, by=c("key1", "key2"), all.x=TRUE, all.y=FALSE)
            pairs <- pairs[order(pairs[['order']]),]
            pairs[['order']] <- NULL
            
            # fill in scores for exact matches that were not in the lookup table
            both_observed <- !is.na(pairs$key1) & !is.na(pairs$key2)
            if (!is.na(comparator@default_match)) {
              exact_match <- (pairs$key1 == pairs$key2) & both_observed
              pairs[exact_match & is.na(pairs$score), "score"] <- comparator@default_match
            }
            
            # fill any remaining NAs with default score if given
            if (!is.na(comparator@default_nonmatch)) {
              pairs[both_observed & is.na(pairs$score), "score"] <- comparator@default_nonmatch
            }
            
            # return only the scores
            if (return_matrix) {
              matrix(pairs$score, nrow = length(x), ncol = length(y))
            } else {
              PairwiseMatrix(.Data = pairs$score, Dim = c(length(x), length(y)), Diag = TRUE)
            }
          }
)

#' @describeIn pairwise Specialization for [`Lookup`] where `x` is a vector of 
#' strings to compare among themselves
setMethod(pairwise, signature = c(comparator = "Lookup", x = "vector", y = "NULL"), 
          function(comparator, x, y, return_matrix, ...) {
            if (!return_matrix) warning("`return_matrix = FALSE` is not supported")
            pairwise(comparator, x, x, return_matrix)
          }
)