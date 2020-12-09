context("Constant comparator")

test_that("Constant is correct using default parameters", {
  comparator <- Constant()
  expect_equal(comparator("program", "application"), 0.0)
  expect_equal(comparator("", ""), 0.0)
  expect_equal(comparator("?:.", "%$#"), 0.0)
  expect_equal(comparator("ABC", "ABC"), 0.0)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("A", "B", "C"))), 0.0)
})

test_that("Constant is correct when constant score is non-default", {
  comparator <- Constant(constant = 10.0)
  expect_equal(comparator("program", "application"), 10.0)
  expect_equal(comparator("", ""), 10.0)
  expect_equal(comparator("?:.", "%$#"), 10.0)
  expect_equal(comparator("ABC", "ABC"), 10.0)
  expect_equal(comparator(list(c("A", "B", "C")), list(c("A", "B", "C"))), 10.0)
})
