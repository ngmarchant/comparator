context("Jaro-Winkler comparator")

test_that("Jaro-Winkler comparator is correct when both strings are empty", {
  expect_equal(JaroWinkler()("", ""), 1.0)
  expect_equal(JaroWinkler(similarity = FALSE)("", ""), 0.0)
})

test_that("Jaro-Winkler comparator is correct when one string is empty and the other is not", {
  expect_equal(JaroWinkler()("Paul", ""), 0.0)
  expect_equal(JaroWinkler()("", "Paul"), 0.0)
  expect_equal(JaroWinkler(similarity = FALSE)("Paul", ""), 1.0)
  expect_equal(JaroWinkler(similarity = FALSE)("", "Paul"), 1.0)
  expect_equal(JaroWinkler()(list(character()), list(c("A", "B", "A", "B"))), 0.0)
  expect_equal(JaroWinkler()(list(c("A", "B", "A", "B")), list(character())), 0.0)
})

test_that("Jaro-Winkler comparator is correct for simple examples when using default parameters", {
  comparator <- JaroWinkler()
  expect_equal(comparator("Mcihael", "Michael"), 0.9571428571428572)
  expect_equal(comparator("Phil", "Phillip"), 0.9142857142857143)
  expect_equal(comparator("Martha", "Mathra"), 0.9555555555555556)
  expect_equal(comparator("Crate", "Trace"), 0.7333333333333334)
  expect_equal(comparator("Georgina", "George"), 0.8916666666666667)
  expect_equal(comparator("Xudong", "Rui"), 0.5)
  expect_equal(comparator(list(c("P", "h", "i", "l")), list(c("P", "h", "i", "l", "l", "i", "p"))), 0.9142857142857143)
})

test_that("Jaro-Winkler comparator is correct for simple examples when using a custom weight", {
  comparator <- JaroWinkler(p = 0.3, max_prefix = 3)
  expect_equal(comparator("Mcihael", "Michael"), 0.9666666666666667)
  expect_equal(comparator("Phil", "Phillip"), 0.9857142857142857)
  expect_equal(comparator("Martha", "Mathra"), 0.9777777777777778)
  expect_equal(comparator("Crate", "Trace"), 0.7333333333333334)
  expect_equal(comparator("Georgina", "George"), 0.9819444444444444)
  expect_equal(comparator("Xudong", "Rui"), 0.5)
})