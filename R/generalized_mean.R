
#' Harmonic Mean
#' 
#' @param x An \R object. Currently there are methods for numeric/logical 
#'   vectors and date, date-time and time interval objects. Complex vectors 
#'   are allowed for `trim = 0`, only.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each 
#'   end of `x` before the mean is computed. Values of trim outside that range 
#'   are taken as the nearest endpoint.
#' @param na.rm a logical value indicating whether `NA` values should be 
#'   stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return If `trim` is zero (the default), the harmonic mean of the values 
#'   in `x` is computed, as a numeric or complex vector of length one. If `x` 
#'   is not logical (coerced to numeric), numeric (including integer) or 
#'   complex, `NA_real_` is returned, with a warning.
#'   
#'   If `trim` is non-zero, a symmetrically trimmed mean is computed with a 
#'   fraction of trim observations deleted from each end before the mean 
#'   is computed.
#'   
#' @seealso [`mean`] for the arithmetic mean and [`gmean`] for the geometric 
#'   mean.
#' 
#' @examples 
#' x <- c(1:10, 50)
#' xm <- hmean(x)
#' 
#' @rdname hmean
#' @export
hmean <- function(x, ...) UseMethod("hmean")

#' @rdname hmean
#' @export
hmean.default <- function(x, trim = 0, na.rm = FALSE, ...) {
  1/mean(1/x, trim = trim, na.rm = na.rm, ...)
}

#' Geometric Mean
#' 
#' @param x An \R object. Currently there are methods for numeric/logical 
#'   vectors and date, date-time and time interval objects. Complex vectors 
#'   are allowed for `trim = 0`, only.
#' @param na.rm a logical value indicating whether `NA` values should be 
#'   stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The geometric mean of the values in `x` is computed, as a numeric 
#'   or complex vector of length one. If `x` is not logical (coerced to 
#'   numeric), numeric (including integer) or complex, `NA_real_` is returned, 
#'   with a warning.
#' 
#' @seealso [`mean`] for the arithmetic mean and [`hmean`] for the harmonic 
#'   mean.
#' 
#' @examples 
#' x <- c(1:10, 50)
#' xm <- gmean(x)
#' 
#' @rdname gmean
#' @export
gmean <- function(x, ...) UseMethod("gmean")

#' @rdname gmean
#' @export
gmean.default <- function(x, na.rm = FALSE, ...) {
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  prod(x, na.rm = na.rm)**(1/length(x))
}