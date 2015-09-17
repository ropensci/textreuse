context("Pairwise comparison")

dir <- system.file("extdata/legal", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir)
cf1 <- pairwise_compare(corpus, jaccard_similarity)
cf2 <- pairwise_compare(corpus, ratio_of_matches, directional = TRUE)

test_that("returns matrix with correct properties", {
  expect_is(cf1, "matrix")
  expect_equal(rownames(cf1), names(corpus))
  expect_is(cf1[1, 3], "numeric")
  expect_is(cf2[3, 1], "numeric")
  expect_equal(cf1[lower.tri(cf1, diag = TRUE)], rep(NA_real_, 6))
  expect_equal(cf2[diag(cf2)], rep(NA_real_, 3))
})

test_that("peforms calculations as expected", {
  expect_equal(cf1[1,2], jaccard_similarity(corpus[[1]], corpus[[2]]))
  expect_equal(cf2[3,2], ratio_of_matches(corpus[[3]], corpus[[2]]))
})
