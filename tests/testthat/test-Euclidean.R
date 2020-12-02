context("Euclidean distance")

test_that("Euclidean distance is correct", {
  measure <- Euclidean()
  expect_equal(measure(c(1,2,3), c(2,3,4)), sqrt(3))
  expect_equal(measure(-c(1,1,1), c(1,1,1)), sqrt(12))
  expect_equal(measure(c(1,2), c(1,2)), 0)
  expect_equal(measure(0, 0), 0)
  expect_equal(measure(0, Inf), Inf)
})