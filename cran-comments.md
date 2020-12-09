## Resubmission

This is a resubmission. There were two comments to address.

> Please add a few more details about the package functionality and
implemented methods in your Description text.

The Description field has been expanded and now includes examples of the supported comparison functions.

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file

The package implemented methods from a variety of sources---too many to list in the Description field. References are included in the documentation for each comparison function.

In addition, several improvements have been made in the resubmitted version (v0.1.1):

* Add support for comparing sequences, represented as atomic vectors
* Fix bug in `DamerauLevenshtein` that resulted in incorrect computation of transposition costs
* Rename "Measure" to "Comparator" throughout
* Reclassify `MongeElkan` and `FuzzyTokenSet` as token comparators, which operate on lists of token vectors, rather than vectors of (pre-tokenized) strings
* Improve documentation

## Original comments

First release on CRAN.

## Test environments
* Fedora 33, R 4.0.3
* Windows 10, R 4.0.3
* winbuilder (devel)

## R CMD check results

Maintainer: 'Neil Marchant <ngmarchant@gmail.com>'
  
New submission

0 errors | 0 warnings | 1 note
