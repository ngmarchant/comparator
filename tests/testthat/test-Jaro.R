context("Jaro comparator")

test_that("Jaro comparator is correct when both strings are empty", {
  expect_equal(Jaro()("", ""), 1.0)
  expect_equal(Jaro(similarity = FALSE)("", ""), 0.0)
})

test_that("Jaro comparator is correct when one string is empty and the other is not", {
  expect_equal(Jaro()("Paul", ""), 0.0)
  expect_equal(Jaro()("", "Paul"), 0.0)
  expect_equal(Jaro(similarity = FALSE)("Paul", ""), 1.0)
  expect_equal(Jaro(similarity = FALSE)("", "Paul"), 1.0)
  expect_equal(Jaro()(list(character()), list(c("A", "B", "A", "B"))), 0.0)
  expect_equal(Jaro()(list(c("A", "B", "A", "B")), list(character())), 0.0)
})

test_that("Jaro comparator is correct for simple examples when using default parameters", {
  comparator <- Jaro()
  expect_equal(comparator("Mcihael", "Michael"), 0.9523809523809524)
  expect_equal(comparator("Phil", "Phillip"), 0.8571428571428571)
  expect_equal(comparator("Martha", "Mathra"), 0.944444444444444)
  expect_equal(comparator("Crate", "Trace"), 0.7333333333333334)
  expect_equal(comparator("Georgina", "George"), 0.8194444444444445)
  expect_equal(comparator("Xudong", "Rui"), 0.5)
  expect_equal(Jaro()(list(c("P", "h", "i", "l")), list(c("P", "h", "i", "l", "l", "i", "p"))), 0.8571428571428571)
})