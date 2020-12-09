context("Lookup comparator")

lookup_table <- data.frame(y = c("Aqua", "Maroon", "Gold"), 
                           x = c("Blue", "Red", "Yellow"), 
                           s = c(0.2, 0.3, 0.1))

test_that("Lookup comparator is correct when using default settings", {
  comparator <- Lookup(lookup_table, c("x", "y"), "s")
  expect_equal(comparator("Green", "Red"), NA_real_)
  expect_equal(comparator("Blue", "Aqua"), 0.2)
  expect_equal(comparator("Aqua", "Blue"), 0.2)
  expect_equal(comparator("Yellow", "Gold"), 0.1)
  expect_equal(comparator("Amber", "Amber"), 0)
})

test_that("Lookup comparator is correct when symmetric = FALSE", {
  comparator <- Lookup(lookup_table, c("x", "y"), "s", symmetric = FALSE)
  expect_equal(comparator("Green", "Red"), NA_real_)
  expect_equal(comparator("Blue", "Aqua"), 0.2)
  expect_equal(comparator("Aqua", "Blue"), NA_real_)
  expect_equal(comparator("Yellow", "Gold"), 0.1)
  expect_equal(comparator("Amber", "Amber"), 0)
})

test_that("Lookup comparator is correct when non-default scores are provided", {
  comparator <- Lookup(lookup_table, c("x", "y"), "s", default_match = -10, default_nonmatch = 10)
  expect_equal(comparator("Green", "Red"), 10)
  expect_equal(comparator("Blue", "Aqua"), 0.2)
  expect_equal(comparator("Aqua", "Blue"), 0.2)
  expect_equal(comparator("Yellow", "Gold"), 0.1)
  expect_equal(comparator("Amber", "Amber"), -10)
})

test_that("Lookup comparator is correct when ignoring case", {
  comparator <- Lookup(lookup_table, c("x", "y"), "s", ignore_case = TRUE)
  expect_equal(comparator("green", "Red"), NA_real_)
  expect_equal(comparator("blue", "aqua"), 0.2)
  expect_equal(comparator("Aqua", "BLUE"), 0.2)
  expect_equal(comparator("Yellow", "Gold"), 0.1)
  expect_equal(comparator("Amber", "AMBER"), 0)
})