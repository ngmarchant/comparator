context("Longest Common Subsequence distance")

examples_equal_weights <- list(
  list(x = "abc", y = "abc", true_dist = 0),            # identical
  list(x = "plane", y = "plant", true_dist = 2),        # substitution
  list(x = "color", y = "colour", true_dist = 1),       # insertion
  list(x = "favourite", y = "favorite", true_dist = 1), # deletion
  list(x = "abc", y = "ca", true_dist = 3),             # transposition + insertion
  list(x = "abc", y = "acb", true_dist = 2),            # transposition
  list(x = "1234", y = "", true_dist = 4),              # complete deletion
  list(x = "", y = "1234", true_dist = 4),              # complete insertion
  list(x = "", y = "", true_dist = 0),                  # empty
  list(x = "positive", y = "evitisop", true_dist = 10), # reverse
  list(x = "café", y = "cafe", true_dist = 2),          # character with diacritic
  list(x = "Saturday", y = "Sunday", true_dist = 4)
  
)

test_that("LCS distance is correct when weights are equal", {
  for (example in examples_equal_weights) {
    with(example, 
         expect_equal(LCS()(x, y), true_dist))
    
  }
})

examples_nonequal_weights <- list(
  list(measure = LCS(insertion = 10, deletion = 10), 
       x = "abc", y = "abc", true_dist = 0),               # identical
  list(measure = LCS(insertion = 0.9), 
       x = "color", y = "colour", true_dist = 0.9),        # insertion
  list(measure = LCS(deletion = 0.9), 
       x = "favourite", y = "favorite", true_dist = 0.9),  # deletion
  list(measure = LCS(insertion = 100), 
       x = "abc", y = "ca", true_dist = 102),              # transposition + insertion
  list(measure = LCS(insertion = 10, deletion = 20), 
       x = "", y = "", true_dist = 0),                     # empty
  list(measure = LCS(insertion=100), 
       x = "positive", y = "evitisop", true_dist = 505)    # reverse
)

test_that("LCS distance is correct when weights are not equal", {
  for (example in examples_nonequal_weights) {
    with(example, 
         expect_equal(measure(x, y), true_dist))
    
  }
})

examples_normalized <- list(
  list(x = "abc", y = "abc", true_dist = 0),                       # identical
  list(x = "plane", y = "plant", true_dist = 2*2/(2*5+2)),         # substitution
  list(x = "color", y = "colour", true_dist = 2/(5+6+1)),          # insertion
  list(x = "favourite", y = "favorite", true_dist = 2/(9+8+1)),    # deletion
  list(x = "abc", y = "ca", true_dist = 2*3/(3+2+3)),              # transposition + insertion
  list(x = "abc", y = "acb", true_dist = 2*2/(2*3+2)),             # transposition
  list(x = "1234", y = "", true_dist = 2*4/(4+4)),                 # complete deletion
  list(x = "", y = "1234", true_dist = 2*4/(4+4)),                 # complete insertion
  list(x = "", y = "", true_dist = 0),                             # empty
  list(x = "positive", y = "evitisop", true_dist = 2*10/(2*8+10)), # reverse
  list(x = "café", y = "cafe", true_dist = 2*2/(2*4+2)),           # character with diacritic
  list(x = "Saturday", y = "Sunday", true_dist = 2*4/(8+6+4))
)

test_that("LCS distance is correct when normalized", {
  for (example in examples_normalized) {
    with(example, 
         expect_equal(LCS(normalize = TRUE)(x, y), true_dist))
    
  }
})