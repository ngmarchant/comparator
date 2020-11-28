
#' Pairwise Similarity/Distance Matrix
#' 
#' @description 
#' Represents a pairwise similarity or distance matrix.
#' 
#' @details If the elements being compared are from the same set, the matrix 
#'   may be symmetric if the corresponding similarity/distance measure is 
#'   symmetric. In this case, entries in the upper triangle and/or along the 
#'   diagonal may not be stored in memory, since they are redundant.
#' 
#' @slot .Data entries of the matrix in column-major order. Entries in the 
#'   upper triangle and/or on the diagonal may be omitted.
#' @slot Dim integer vector of length 2. The dimensions of the matrix.
#' @slot Diag logical indicating whether the diagonal entries are stored in 
#'   `.Data`.
#' 
#' @rdname PairwiseMatrix-class 
#' @export
setClass("PairwiseMatrix", 
         slots = c(Dim = "integer",
                   Diag = "logical"), 
         prototype = structure(
           .Data = numeric(), 
           Dim = c(0L, 0L), 
           Diag = TRUE
         ),
         contains = "numeric", 
         validity = function(object) {
           errs <- character()
           if (length(object@Dim) != 2)
             errs <- c(errs, "`Dim` must be an integer vector of length 2")
           nr <- object@Dim[1]
           nc <- object@Dim[2]
           if (nr < 0 || !is.finite(nr) || nc < 0 || !is.finite(nc))
             errs <- c(errs, "entries of `Dim` must be finite and non-negative")
           n <- length(object@.Data)
           if (n > nr * nc) 
             errs <- c(errs, "mismatch between `Dim` and length of `.Data`")
           if (length(object@Diag) != 1)
             errs <- c(errs, "`Diag` must be a logical vector of length 1")
           if (!object@Diag && prod(object@Dim) == n)
             errs <- c(errs, "`Diag` must be TRUE if `.Data` contains full matrix")
           ifelse(length(errs) == 0, TRUE, errs)
         })

#' @rdname PairwiseMatrix-class 
#' @export
setGeneric("as.PairwiseMatrix", function(.Data, ...) standardGeneric("as.PairwiseMatrix"))


#' @rdname PairwiseMatrix-class 
#' @export
setMethod(as.PairwiseMatrix, signature = c(.Data = "matrix"), 
          function(.Data, ...) {
            new("PairwiseMatrix", .Data = as.vector(.Data), Dim = dim(.Data), Diag = TRUE)
          }
)

#' @rdname PairwiseMatrix-class 
#' @export
setMethod(as.PairwiseMatrix, signature = c(.Data = "numeric"), 
          function(.Data, Dim, Diag, ...) {
            new("PairwiseMatrix", .Data = as.vector(.Data), Dim = dim, Diag = diag)
          }
)

as.matrix.PairwiseMatrix <- function (x, ...) {
  x <- sparse_to_full(x)
  m <- x@.Data
  dim(m) <- x@Dim
  return(m)
}

#' @rdname PairwiseMatrix-class 
#' @export
setMethod("as.matrix", "PairwiseMatrix", as.matrix.PairwiseMatrix)