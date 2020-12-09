
# Modified from src/library/utils/R/adist.R in base R
strings_to_code_vectors <- function(x, y=NULL, ignore_case=FALSE, use_bytes=FALSE) {
  bytesToInt <- function(x) {
    if(is.na(x)) return(NA_integer_)
    as.integer(charToRaw(x))
  }
  
  nmx <- names(x)
  x <- as.character(x)
  names(x) <- nmx
  
  ex <- Encoding(x)
  use_bytes <- isTRUE(use_bytes) || any(ex == "bytes")
  
  if(!is.null(y)) {
    nmy <- names(y)
    y <- as.character(y)
    names(y) <- nmy
    ey <- Encoding(y)
    use_bytes <- use_bytes || any(ey == "bytes")
  }
  
  if(use_bytes) {
    x <- lapply(x, bytesToInt)
    y <- if(is.null(y)) {
      x
    } else {
      lapply(y, bytesToInt)
    }
  } else {
    ignore_case <- isTRUE(ignore_case)
    x <- if(ignore_case) {
      lapply(tolower(enc2utf8(x)), utf8ToInt)
    } else {
      lapply(enc2utf8(x), utf8ToInt)
    }
    y <- if(is.null(y)) {
      x
    } else if(ignore_case) {
      lapply(tolower(enc2utf8(y)), utf8ToInt)
    } else {
      lapply(enc2utf8(y), utf8ToInt)
    }
  }
  list("x" = x, "y" = y)
}