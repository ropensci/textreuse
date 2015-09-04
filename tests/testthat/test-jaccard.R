context("Jaccard coefficients")

test_that("calculates the similarity coefficient correctly", {
  expect_equal(jaccard_similarity(1:3, 2:4), 0.5)
  expect_equal(jaccard_similarity(letters[1:3], letters[2:3]), 2/3)
  a <- sample(letters, 10, replace = TRUE)
  b <- sample(letters, 10, replace = TRUE)
  res <- jaccard_similarity(a, b)
  expect_true(0 <= res & res <= 1)
  })

test_that("calculates the dissimilarity coefficient correctly", {
  expect_equal(1 - jaccard_similarity(1:10, 8:11),
               jaccard_dissimilarity(1:10, 8:11))
  expect_equal(1 - jaccard_similarity(letters[1:5], letters[3:10]),
               jaccard_dissimilarity(letters[1:5], letters[3:10]))
})

test_that("works with TextReuseTextDocument", {
  ny <- system.file("extdata/ny1850-match.txt", package = "textreuse")
  ca <- system.file("extdata/ca1851-match.txt", package = "textreuse")
  expect_is(jaccard_similarity(ny, ca), "numeric")
})
