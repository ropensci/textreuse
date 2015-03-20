context("Ratio of matches")

test_that("calculates the value correctly", {
  expect_equal(ratio_of_matches(1:4, 3:5), 2/3)
  expect_equal(ratio_of_matches(letters[1:3], letters[3:4]), 0.5)
  a <- sample(letters, 10, replace = TRUE)
  b <- sample(letters, 10, replace = TRUE)
  res <- ratio_of_matches(a, b)
  expect_true(0 <= res & res <= 1)
})

test_that("works with TextReuseTextDocument", {
  ny <- system.file("extdata/ny1850-match.txt", package = "textreuse")
  ca <- system.file("extdata/ca1851-match.txt", package = "textreuse")
  expect_is(jaccard_coef(ny, ca), "numeric")
})
