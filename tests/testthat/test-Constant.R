context("Constant measure")

test_that("Constant is correct using default parameters", {
  measure <- Constant()
  expect_equal(measure("program", "application"), 0.0)
  expect_equal(measure("", ""), 0.0)
  expect_equal(measure("?:.", "%$#"), 0.0)
  expect_equal(measure("ABC", "ABC"), 0.0)
})

test_that("Constant is correct when constant score is non-default", {
  measure <- Constant(constant = 10.0)
  expect_equal(measure("program", "application"), 10.0)
  expect_equal(measure("", ""), 10.0)
  expect_equal(measure("?:.", "%$#"), 10.0)
  expect_equal(measure("ABC", "ABC"), 10.0)
})
