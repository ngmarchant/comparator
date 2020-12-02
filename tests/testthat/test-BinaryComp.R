context("Binary Comparison measure")

test_that("Binary Comparison measure is correct when measuring distance", {
  measure <- BinaryComp()
  expect_equal(measure("Chen", "Chen"), 0)
  expect_equal(measure("Chen", "Cheng"), 1)
})

test_that("Binary Comparison measure is correct when measuring similarity", {
  measure <- BinaryComp(similarity = TRUE)
  expect_equal(measure("Chen", "Chen"), 1)
  expect_equal(measure("Chen", "Cheng"), 0)
})

test_that("Binary Comparison measure is correct when measuring disagreement score is non-default", {
  measure <- BinaryComp(disagree = Inf)
  expect_equal(measure("Chen", "Chen"), 0)
  expect_equal(measure("Chen", "Cheng"), Inf)
})

test_that("Binary Comparison measure is correct when ignoring case", {
  measure <- BinaryComp(ignore_case = TRUE)
  expect_equal(measure("apples", "APPLES"), 0)
  expect_equal(measure(":D", ":d"), 0)
})