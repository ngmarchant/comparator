
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comparator: Comparison Functions for Clustering and Record Linkage

<!-- badges: start -->

<!-- badges: end -->

comparator implements comparison functions for clustering and record
linkage applications. It includes functions for comparing strings,
sequences and numeric vectors. Where possible, comparators are
implemented in C/C++ to ensure fast performance.

## Supported comparators

### String comparators:

#### Edit-based:

  - `Levenshtein()`: Levenshtein distance/similarity
  - `DamerauLevenshtein()` Damerau-Levenshtein distance/similarity
  - `Hamming()`: Hamming distance/similarity
  - `OSA()`: Optimal String Alignment distance/similarity
  - `LCS()`: Longest Common Subsequence distance/similarity
  - `Jaro()`: Jaro distance/similarity
  - `JaroWinkler()`: Jaro-Winkler distance/similarity

#### Token-based:

Not yet implemented.

#### Hybrid token-character:

  - `MongeElkan()`: Monge-Elkan similarity
  - `FuzzyTokenSet()`: Fuzzy Token Set distance

#### Other:

  - `InVocabulary()`: Compares strings using a reference vocabulary.
    Useful for comparing names.
  - `Lookup()`: Retrieves distances/similarities from a lookup table
  - `BinaryComp()`: Compares strings based on whether they
    agree/disagree exactly.

### Numeric comparators:

  - `Euclidean()`: Euclidean (L-2) distance
  - `Manhattan()`: Manhattan (L-1) distance
  - `Chebyshev()`: Chebyshev (L-∞) distance
  - `Minkowski()`: Minkowski (L-p) distance

## Installation

You can install the latest release from
[CRAN](https://CRAN.R-project.org) by entering:

``` r
install.packages("comparator")
```

The development version can be installed from GitHub using `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("ngmarchant/comparator")
```

## Example

A comparator is instantiated by calling its constructor function. For
example, we can instantiate a Levenshtein similarity comparator that
ignores differences in upper/lowercase characters as follows:

``` r
comparator <- Levenshtein(similarity = TRUE, normalize = TRUE, ignore_case = TRUE)
```

We can apply the comparator to character vectors element-wise as
follows:

``` r
x <- c("John Doe", "Jane Doe")
y <- c("jonathon doe", "jane doe")
elementwise(comparator, x, y)
#> [1] 0.6666667 1.0000000

# shorthand for above
comparator(x, y)
#> [1] 0.6666667 1.0000000
```

This comparator is also defined on sequences:

``` r
x_seq <- list(c(1, 2, 1, 1), c(1, 2, 3, 4))
y_seq <- list(c(4, 3, 2, 1), c(1, 2, 3, 1))
elementwise(comparator, x_seq, y_seq)
#> [1] 0.4545455 0.7777778

# shorthand for above
comparator(x_seq, y_seq)
#> [1] 0.4545455 0.7777778
```

Pairwise comparisons are also supported using the following syntax:

``` r
# compare each string in x with each string in y and return a similarity matrix
pairwise(comparator, x, y, return_matrix = TRUE)
#>           [,1]      [,2]
#> [1,] 0.6666667 0.6842105
#> [2,] 0.5384615 1.0000000

# compare the strings in x pairwise and return a similarity matrix
pairwise(comparator, x, return_matrix = TRUE)
#>           [,1]      [,2]
#> [1,] 1.0000000 0.6842105
#> [2,] 0.6842105 1.0000000
```
