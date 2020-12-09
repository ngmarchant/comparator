context("Manhattan distance")

test_that("Manhattan distance is correct", {
  comparator <- Manhattan()
  expect_equal(comparator(c(1,2,3), c(2,3,4)), 3)
  expect_equal(comparator(-c(1,1,1), c(1,1,1)), 6)
  expect_equal(comparator(c(1,2), c(1,2)), 0)
  expect_equal(comparator(0, 0), 0)
  expect_equal(comparator(0, Inf), Inf)
})