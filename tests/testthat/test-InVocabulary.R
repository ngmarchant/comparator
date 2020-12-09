context("InVocabulary comparator")

known_names <- c("Paul", "Saul", "Chen", "Cheng")
comparator <- InVocabulary(known_names, both_in_distinct = 0.7, one_in = 0.5, 
                        none_in = 0.2, both_in_same = 1)

test_that("InVocabulary comparator is correct when both string are in-vocab and distinct", {
  expect_equal(comparator("Chen", "Cheng"), 0.7)
})

test_that("InVocabulary comparator is correct when one string is in-vocab and the other is out", {
  expect_equal(comparator("Paul", "Pual"), 0.5)
})

test_that("InVocabulary comparator is correct when neither string is in-vocab", {
  expect_equal(comparator("Mcihael", "Michael"), 0.2)
})

test_that("InVocabulary comparator is correct when the strings are identical and in-vocab", {
  expect_equal(comparator("Paul", "Paul"), 1)
})

test_that("InVocabulary comparator is correct when ignoring case", {
  comparator <- InVocabulary(known_names, both_in_distinct = 0.7, one_in = 0.5, 
                          none_in = 0.2, both_in_same = 1, ignore_case = TRUE)
  expect_equal(comparator("Paul", "paul"), 1)
  expect_equal(comparator("chen", "cheng"), 0.7)
})
