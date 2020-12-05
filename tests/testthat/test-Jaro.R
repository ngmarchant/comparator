context("Jaro measure")

test_that("Jaro measure is correct when both strings are empty", {
  expect_equal(Jaro()("", ""), 1.0)
  expect_equal(Jaro(similarity = FALSE)("", ""), 0.0)
})

test_that("Jaro measure is correct when one string is empty and the other is not", {
  expect_equal(Jaro()("Paul", ""), 0.0)
  expect_equal(Jaro()("", "Paul"), 0.0)
  expect_equal(Jaro(similarity = FALSE)("Paul", ""), 1.0)
  expect_equal(Jaro(similarity = FALSE)("", "Paul"), 1.0)
})

test_that("Jaro measure is correct for simple examples when using default parameters", {
  measure <- Jaro()
  expect_equal(measure("Mcihael", "Michael"), 0.9523809523809524)
  expect_equal(measure("Phil", "Phillip"), 0.8571428571428571)
  expect_equal(measure("Martha", "Mathra"), 0.944444444444444)
  expect_equal(measure("Crate", "Trace"), 0.7333333333333334)
  expect_equal(measure("Georgina", "George"), 0.8194444444444445)
  expect_equal(measure("Xudong", "Rui"), 0.5)
})