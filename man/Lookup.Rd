% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lookup.R
\name{Lookup}
\alias{Lookup}
\title{Lookup String Comparator}
\usage{
Lookup(
  lookup_table,
  values_colnames,
  score_colname,
  default_match = 0,
  default_nonmatch = NA_real_,
  symmetric = TRUE,
  ignore_case = FALSE
)
}
\arguments{
\item{lookup_table}{data frame containing distances/similarities for
pairs of values}

\item{values_colnames}{character vector containing the colnames
corresponding to pairs of values (e.g. strings) in \code{lookup_table}}

\item{score_colname}{name of column that contains distances/similarities
in \code{lookup_table}}

\item{default_match}{distance/similarity to use if the pair of values
match exactly and do not appear in \code{lookup_table}. Defaults to 0.0.}

\item{default_nonmatch}{distance/similarity to use if the pair of values are
not an exact match and do not appear in \verb{lookup table}. Defaults to \code{NA}.}

\item{symmetric}{whether the underlying distance/similarity scores are
symmetric. If TRUE \code{lookup_table} need only contain entries for
one of the two pairs---i.e. an entry for value pair \eqn{(y, x)} is not
required if an entry for \eqn{(x, y)} is already present.}

\item{ignore_case}{a logical. If TRUE, case is ignored when comparing the
strings.}
}
\value{
A \code{Lookup} instance is returned, which is an S4 class inheriting from
\code{\linkS4class{StringComparator}}.
}
\description{
Compares a pair of strings \eqn{x} and \eqn{y} by retrieving
their distance/similarity score from a provided lookup table.
}
\details{
The lookup table should contain three columns corresponding to \eqn{x},
and \eqn{y} (\code{values_colnames} below) and the distance/similarity
(\code{score_colname} below). If a pair of values \eqn{x} and \eqn{y} is
not in the lookup table, a default distance/similarity is returned
depending on whether \eqn{x = y} (\code{default_match} below) or
\eqn{x \neq y}{x != y} (\code{default_nonmatch} below).
}
\examples{
## Measure the distance between cities
lookup_table <- data.frame(x = c("Melbourne", "Melbourne", "Sydney"), 
                           y = c("Sydney", "Brisbane", "Brisbane"), 
                           dist = c(713.4, 1374.8, 732.5))

comparator <- Lookup(lookup_table, c("x", "y"), "dist")
comparator("Sydney", "Melbourne")
comparator("Melbourne", "Perth")

}
