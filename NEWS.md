# comparator 0.1.2
* Fix compilation issue with GCC12 toolchain.

# comparator 0.1.1
* Add support for comparing sequences, represented as atomic vectors
* Fix bug in `DamerauLevenshtein` that resulted in incorrect computation of transposition costs
* Rename "Measure" to "Comparator" throughout
* Reclassify `MongeElkan` and `FuzzyTokenSet` as token comparators, which operate on lists of token vectors, rather than vectors of (pre-tokenized) strings
* Improve documentation

# comparator 0.1.0
* Initial release
