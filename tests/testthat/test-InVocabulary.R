context("InVocabulary measure")

known_names <- c("Paul", "Saul", "Chen", "Cheng")
measure <- InVocabulary(known_names, both_in_distinct = 0.7, one_in = 0.5, 
                        none_in = 0.2, both_in_same = 1)

test_that("InVocabulary measure is correct when both string are in-vocab and distinct", {
  expect_equal(measure("Chen", "Cheng"), 0.7)
})

test_that("InVocabulary measure is correct when one string is in-vocab and the other is out", {
  expect_equal(measure("Paul", "Pual"), 0.5)
})

test_that("InVocabulary measure is correct when neither string is in-vocab", {
  expect_equal(measure("Mcihael", "Michael"), 0.2)
})

test_that("InVocabulary measure is correct when the strings are identical and in-vocab", {
  expect_equal(measure("Paul", "Paul"), 1)
})

test_that("InVocabulary measure is correct when ignoring case", {
  measure <- InVocabulary(known_names, both_in_distinct = 0.7, one_in = 0.5, 
                          none_in = 0.2, both_in_same = 1, ignore_case = TRUE)
  expect_equal(measure("Paul", "paul"), 1)
  expect_equal(measure("chen", "cheng"), 0.7)
})
