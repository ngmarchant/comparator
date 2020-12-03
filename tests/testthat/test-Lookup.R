context("Lookup measure")

lookup_table <- data.frame(y = c("Aqua", "Maroon", "Gold"), 
                           x = c("Blue", "Red", "Yellow"), 
                           s = c(0.2, 0.3, 0.1))

test_that("Lookup measure is correct when using default settings", {
  measure <- Lookup(lookup_table, c("x", "y"), "s")
  expect_equal(measure("Green", "Red"), NA_real_)
  expect_equal(measure("Blue", "Aqua"), 0.2)
  expect_equal(measure("Aqua", "Blue"), 0.2)
  expect_equal(measure("Yellow", "Gold"), 0.1)
  expect_equal(measure("Amber", "Amber"), 0)
})

test_that("Lookup measure is correct when symmetric = FALSE", {
  measure <- Lookup(lookup_table, c("x", "y"), "s", symmetric = FALSE)
  expect_equal(measure("Green", "Red"), NA_real_)
  expect_equal(measure("Blue", "Aqua"), 0.2)
  expect_equal(measure("Aqua", "Blue"), NA_real_)
  expect_equal(measure("Yellow", "Gold"), 0.1)
  expect_equal(measure("Amber", "Amber"), 0)
})

test_that("Lookup measure is correct when non-default scores are provided", {
  measure <- Lookup(lookup_table, c("x", "y"), "s", default_match = -10, default_nonmatch = 10)
  expect_equal(measure("Green", "Red"), 10)
  expect_equal(measure("Blue", "Aqua"), 0.2)
  expect_equal(measure("Aqua", "Blue"), 0.2)
  expect_equal(measure("Yellow", "Gold"), 0.1)
  expect_equal(measure("Amber", "Amber"), -10)
})

test_that("Lookup measure is correct when ignoring case", {
  measure <- Lookup(lookup_table, c("x", "y"), "s", ignore_case = TRUE)
  expect_equal(measure("green", "Red"), NA_real_)
  expect_equal(measure("blue", "aqua"), 0.2)
  expect_equal(measure("Aqua", "BLUE"), 0.2)
  expect_equal(measure("Yellow", "Gold"), 0.1)
  expect_equal(measure("Amber", "AMBER"), 0)
})