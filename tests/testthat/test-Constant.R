context("Constant measure")

test_that("ConstantMeasure is correct using default parameters", {
  measure <- ConstantMeasure()
  expect_equal(measure("program", "application"), 0.0)
  expect_equal(measure("", ""), 0.0)
  expect_equal(measure("?:.", "%$#"), 0.0)
  expect_equal(measure("ABC", "ABC"), 0.0)
})

test_that("ConstantMeasure is correct when constant score is non-default", {
  measure <- ConstantMeasure(constant = 10.0)
  expect_equal(measure("program", "application"), 10.0)
  expect_equal(measure("", ""), 10.0)
  expect_equal(measure("?:.", "%$#"), 10.0)
  expect_equal(measure("ABC", "ABC"), 10.0)
})
