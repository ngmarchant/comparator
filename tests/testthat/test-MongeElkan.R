context("Monge-Elkan measure")

test_that("Monge-Elkan measure is correct when using default settings", {
  measure <- MongeElkan()
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), (2+2*10/14)/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), (2+10/14+1/3)/4)
  expect_equal(measure("The quicker fox", "The quick brown fox"), (2+10/14)/3)
  expect_equal(measure("University of Melbourne", "Melbourne University"), (3/19 + 2)/3)
})

test_that("Monge-Elkan measure is correct when using a non-default inner measure", {
  measure <- MongeElkan(inner_measure = BinaryComp(similarity = TRUE))
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), 2/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), 2/4)
  expect_equal(measure("The quicker fox", "The quick brown fox"), 2/3)
  expect_equal(measure("University of Melbourne", "Melbourne University"), 2/3)
})

test_that("Monge-Elkan measure is correct when using a non-default aggregation function", {
  measure <- MongeElkan(agg_function = hmean)
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), 4/(2 + 2*14/10))
  expect_equal(measure("The quick brown fox", "The quicker fox"), 4/(2 + 14/10 + 3))
  expect_equal(measure("The quicker fox", "The quick brown fox"), 3/(2 + 14/10))
  expect_equal(measure("University of Melbourne", "Melbourne University"), 3/(2 + 19/3))
})

test_that("Monge-Elkan measure is correct when symmetrized", {
  measure <- MongeElkan(symmetrize = TRUE)
  expect_equal(measure("The quick brown fox", "The quicker browner fox"), (2+2*10/14)/4)
  expect_equal(measure("The quick brown fox", "The quicker fox"), (2+10/14)/3)
  expect_equal(measure("The quicker fox", "The quick brown fox"), (2+10/14)/3)
})