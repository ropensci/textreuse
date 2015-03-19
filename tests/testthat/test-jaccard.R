context("Jaccard coefficient")

test_that("calculates the value correctly", {
  expect_equal(jaccard_coef(1:3, 2:4), 0.5)
  expect_equal(jaccard_coef(letters[1:3], letters[2:3]), 2/3)
  a <- sample(letters, 10, replace = TRUE)
  b <- sample(letters, 10, replace = TRUE)
  res <- jaccard_coef(a, b)
  expect_true(0 <= res & res <= 1)
  })

test_that("works with TextReuseTextDocument", {
  ny <- system.file("extdata/ny1850-match.txt", package = "textreuse")
  ca <- system.file("extdata/ca1851-match.txt", package = "textreuse")
  expect_is(jaccard_coef(ny, ca), "numeric")
})
