context("Fuzzy Token Set distance")

test_that("Fuzzy Token Set distance is correct when using default settings", {
  comparator <- FuzzyTokenSet()
  x <- strsplit("The quick brown fox", "\\s+")
  y <- strsplit("The quicker browner fox", "\\s+")
  z <- strsplit("The quicker fox", "\\s+")
  expect_equal(comparator(x, y), (0 + 2/7 + 2/7 + 0)/4)
  expect_equal(comparator(x, z), (0 + 2/7 + 1 + 0)/4)
  expect_equal(comparator(z, x), (0 + 2/7 + 1 + 0)/4)
  x <- strsplit("University of Melbourne", "\\s+")
  y <- strsplit("Melbourne University", "\\s+")
  expect_equal(comparator(x, y), (0 + 1 + 0)/3)
})

test_that("Fuzzy Token Set distance is correct when using a non-default inner comparator", {
  comparator <- FuzzyTokenSet(inner_comparator = BinaryComp())
  x <- strsplit("The quick brown fox", "\\s+")
  y <- strsplit("The quicker browner fox", "\\s+")
  z <- strsplit("The quicker fox", "\\s+")
  expect_equal(comparator(x, y), 2/4)
  expect_equal(comparator(x, z), 2/4)
  expect_equal(comparator(z, x), 2/4)
  x <- strsplit("University of Melbourne", "\\s+")
  y <- strsplit("Melbourne University", "\\s+")
  expect_equal(comparator(x, y), 1/3)
})

test_that("Fuzzy Token Set distance is correct when using asymmetric weights", {
  comparator <- FuzzyTokenSet(deletion = 0.5)
  x <- strsplit("The quick brown fox", "\\s+")
  y <- strsplit("The quicker browner fox", "\\s+")
  z <- strsplit("The quicker fox", "\\s+")
  expect_equal(comparator(x, y), (0 + 2/7 + 2/7 + 0)/4)
  expect_equal(comparator(x, z), (0 + 2/7 + 1/2 + 0)/4)
  expect_equal(comparator(z, x), (0 + 2/7 + 1 + 0)/4)
  x <- strsplit("University of Melbourne", "\\s+")
  y <- strsplit("Melbourne University", "\\s+")
  expect_equal(comparator(x, y), (0 + 1/2 + 0)/3)
})

test_that("Fuzzy Token Set distance is correct when using a non-default aggregation function", {
  comparator <- FuzzyTokenSet(agg_function = base::sum)
  x <- strsplit("The quick brown fox", "\\s+")
  y <- strsplit("The quicker browner fox", "\\s+")
  z <- strsplit("The quicker fox", "\\s+")
  expect_equal(comparator(x, y), 0 + 2/7 + 2/7 + 0)
  expect_equal(comparator(x, z), 0 + 2/7 + 1 + 0)
  expect_equal(comparator(z, x), 0 + 2/7 + 1 + 0)
  x <- strsplit("University of Melbourne", "\\s+")
  y <- strsplit("Melbourne University", "\\s+")
  expect_equal(comparator(x, y), 0 + 1 + 0)
})