context("Binary Comparison comparator")

test_that("Binary Comparison comparator is correct when measuring distance", {
  comparator <- BinaryComp()
  expect_equal(comparator("Chen", "Chen"), 0)
  expect_equal(comparator("Chen", "Cheng"), 1)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("A", "B", "C"))), 0)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("B", "B", "C"))), 1)
})

test_that("Binary Comparison comparator is correct when measuring similarity", {
  comparator <- BinaryComp(similarity = TRUE)
  expect_equal(comparator("Chen", "Chen"), 1)
  expect_equal(comparator("Chen", "Cheng"), 0)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("A", "B", "C"))), 1)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("B", "B", "C"))), 0)
})

test_that("Binary Comparison comparator is correct when measuring disagreement score is non-default", {
  comparator <- BinaryComp(score = Inf)
  expect_equal(comparator("Chen", "Chen"), 0)
  expect_equal(comparator("Chen", "Cheng"), Inf)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("A", "B", "C"))), 0)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("B", "B", "C"))), Inf)
})

test_that("Binary Comparison comparator is correct when ignoring case", {
  comparator <- BinaryComp(ignore_case = TRUE)
  expect_equal(comparator("apples", "APPLES"), 0)
  expect_equal(comparator(":D", ":d"), 0)
})