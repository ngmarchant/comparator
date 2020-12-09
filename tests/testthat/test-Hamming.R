context("Hamming distance")

examples_equal_weights <- list(
  list(x = "abc", y = "abc", true_dist = 0),              # identical
  list(x = "plane", y = "plant", true_dist = 1),          # substitution
  list(x = "color", y = "colour", true_dist = Inf),       # unequal length
  list(x = "favourite", y = "favorite", true_dist = Inf), # unequal length
  list(x = "1234", y = "", true_dist = Inf),              # unequal length
  list(x = "", y = "1234", true_dist = Inf),              # unequal length
  list(x = "", y = "", true_dist = 0),                    # empty
  list(x = "positive", y = "evitisop", true_dist = 8),    # reverse
  list(x = "café", y = "cafe", true_dist = 1),            # character with diacritic
  list(x = list(c("A", "B", "B", "A")), y = list(c("A", "B", "A", "B")), true_dist = 2)
)

test_that("Hamming distance is correct when weights are equal", {
  for (example in examples_equal_weights) {
    with(example, 
         expect_equal(Hamming()(x, y), true_dist))
    
  }
})

examples_normalized <- list(
  list(x = "abc", y = "abc", true_dist = 0),             # identical
  list(x = "plane", y = "plant", true_dist = 1/5),       # substitution
  list(x = "color", y = "colour", true_dist = 1),        # unequal length
  list(x = "favourite", y = "favorite", true_dist = 1),  # unequal length
  list(x = "1234", y = "", true_dist = 1),               # unequal length
  list(x = "", y = "1234", true_dist = 1),               # unequal length
  list(x = "", y = "", true_dist = 0),                   # empty
  list(x = "positive", y = "evitisop", true_dist = 1),   # reverse
  list(x = "café", y = "cafe", true_dist = 1/4),         # character with diacritic
  list(x = list(c("A", "B", "B", "A")), y = list(c("A", "B", "A", "B")), true_dist = 2/4)
)

test_that("Hamming distance is correct when normalized", {
  for (example in examples_normalized) {
    with(example, 
         expect_equal(Hamming(normalize = TRUE)(x, y), true_dist))
    
  }
})