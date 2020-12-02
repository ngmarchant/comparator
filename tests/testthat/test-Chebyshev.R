context("Chebyshev distance")

test_that("Chebyshev distance is correct", {
  measure <- Chebyshev()
  expect_equal(measure(c(1,2,3), c(2,3,4)), 1)
  expect_equal(measure(-c(1,1,1), c(1,1,1)), 2)
  expect_equal(measure(c(10,1,-2), c(0,-5,3)), 10)
  expect_equal(measure(c(1,2), c(1,2)), 0)
  expect_equal(measure(0, 0), 0)
  expect_equal(measure(0, Inf), Inf)
})