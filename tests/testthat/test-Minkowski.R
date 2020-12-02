context("Minkowski distance")

test_that("Minkowski distance is correct for default parameter p = 2", {
  minkowksi <- Minkowski()
  euclidean <- Euclidean()
  expect_equal(minkowksi(c(1,2,3), c(2,3,4)), euclidean(c(1,2,3), c(2,3,4)))
  expect_equal(minkowksi(-c(1,1,1), c(1,1,1)), euclidean(-c(1,1,1), c(1,1,1)))
  expect_equal(minkowksi(c(10,1,-2), c(0,-5,3)), euclidean(c(10,1,-2), c(0,-5,3)))
  expect_equal(minkowksi(c(1,2), c(1,2)), euclidean(c(1,2), c(1,2)))
  expect_equal(minkowksi(0, 0), euclidean(0, 0))
  expect_equal(minkowksi(0, Inf), euclidean(0, Inf))
})

test_that("Minkowski distance is correct for non-default parameter p = 3", {
  minkowksi <- Minkowski(p = 3)
  expect_equal(minkowksi(c(1,2,3), c(2,3,4)), 3**(1/3))
  expect_equal(minkowksi(-c(1,1,1), c(1,1,1)), (3*2**3)**(1/3))
  expect_equal(minkowksi(c(10,1,-2), c(0,-5,3)), (10**3 + 6**3 + 5**3)**(1/3))
  expect_equal(minkowksi(c(1,2), c(1,2)), 0)
  expect_equal(minkowksi(0, 0), 0)
  expect_equal(minkowksi(0, Inf), Inf)
})