
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comparator: Similarity and Distance Measures in R

<!-- badges: start -->

<!-- badges: end -->

comparator implements similarity and distance measures for clustering
and record linkage applications. It includes measures for comparing
strings as well as numeric vectors. Where possible, measures are
implemented in C/C++ to ensure fast performance.

## Supported measures

### String measures:

#### Edit-based:

  - `Levenshtein()`: Levenshtein distance/similarity
  - `DamerauLevenshtein()` Damearu-Levenshtein distance/similarity
  - `Hamming()`: Hamming distance/similarity
  - `OSA()`: Optimal String Alignment distance/similarity
  - `LCS()`: Longest Common Subsequence distance/similarity
  - `Jaro()`: Jaro distance/similarity
  - `JaroWinkler()`: Jaro-Winkler distance/similarity

#### Token-based:

Not yet implemented.

#### Hybrid token-character:

  - `MongeElkan()`: Monge-Elkan measure
  - `FuzzyTokenSet()`: Fuzzy Token Set distance

#### Other:

  - `InVocabulary()`: Compares strings using a reference vocabulary.
    Useful for comparing names.
  - `Lookup()`: Retrieves distances/similarities from a lookup table
  - `BinaryComp()`: Compares strings based on whether they
    agree/disagree exactly.

### Numeric measures:

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

A measure can be instantiated by calling its constructor function. For
instance, we can define a Levenshtein similarity measure that ignores
differences in upper/lowercase characters as follows:

``` r
measure <- Levenshtein(similarity = TRUE, normalize = TRUE, ignore_case = TRUE)
```

A measure can be used to compare vectors element-wise as follows:

``` r
x <- c("John Doe", "Jane Doe")
y <- c("jonathon doe", "jane doe")
elementwise(measure, x, y)
#> [1] 0.6666667 1.0000000

# shorthand for above
measure(x, y)
#> [1] 0.6666667 1.0000000
```

Pairwise comparisons are also supported using the following syntax:

``` r
# compare each value in x with each value in y and return a similarity matrix
pairwise(measure, x, y, return_matrix = TRUE)
#>           [,1]      [,2]
#> [1,] 0.6666667 0.6842105
#> [2,] 0.5384615 1.0000000

# compare the values in x pairwise and return a similarity matrix
pairwise(measure, x, return_matrix = TRUE)
#>           [,1]      [,2]
#> [1,] 1.0000000 0.6842105
#> [2,] 0.6842105 1.0000000
```
