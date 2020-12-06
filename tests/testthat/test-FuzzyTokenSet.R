context("Fuzzy Token Set distance")

test_that("Fuzzy Token Set distance is correct when using default settings", {
  measure <- FuzzyTokenSet()
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), (0 + 2/7 + 2/7 + 0)/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), (0 + 2/7 + 1 + 0)/4)
  expect_equal(measure("The quicker fox", "The quick brown fox"), (0 + 2/7 + 1 + 0)/4)
  expect_equal(measure("University of Melbourne", "Melbourne University"), (0 + 1 + 0)/3)
})

test_that("Fuzzy Token Set distance is correct when using a non-default inner measure", {
  measure <- FuzzyTokenSet(inner_measure = BinaryComp())
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), 2/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), 2/4)
  expect_equal(measure("The quicker fox", "The quick brown fox"), 2/4)
  expect_equal(measure("University of Melbourne", "Melbourne University"), 1/3)
})

test_that("Fuzzy Token Set distance is correct when using asymmetric weights", {
  measure <- FuzzyTokenSet(deletion = 0.5)
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), (0 + 2/7 + 2/7 + 0)/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), (0 + 2/7 + 1/2 + 0)/4)
  expect_equal(measure("The quicker fox", "The quick brown fox"), (0 + 2/7 + 1 + 0)/4)
  expect_equal(measure("University of Melbourne", "Melbourne University"), (0 + 1/2 + 0)/3)
})

test_that("Fuzzy Token Set distance is correct when using a non-default aggregation function", {
  measure <- FuzzyTokenSet(agg_function = base::sum)
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), 0 + 2/7 + 2/7 + 0)
  expect_equal(measure("The quick brown fox", "The quicker fox"), 0 + 2/7 + 1 + 0)
  expect_equal(measure("The quicker fox", "The quick brown fox"), 0 + 2/7 + 1 + 0)
  expect_equal(measure("University of Melbourne", "Melbourne University"), 0 + 1 + 0)
})