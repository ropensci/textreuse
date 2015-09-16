context("Candidate pairs")
dir <- system.file("extdata", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir)
names(corpus) <- filenames(names(corpus))
m <- pairwise_cf(corpus, ratio_of_matches, directional = TRUE)
pairs <- pairwise_candidates(m)

test_that("returns a data frame with correct properties", {
  expect_equal(nrow(pairs), 6)
  expect_is(pairs, "data.frame")
  expect_is(pairs, "tbl_df")
  expect_named(pairs, c("a", "b", "score"))
})

test_that("a and b are in correct order, so results are correct", {
  r <- sample(1:nrow(pairs), 1)
  expect_equal(pairs$score[r],
               ratio_of_matches(corpus[[pairs$a[r]]], corpus[[pairs$b[r]]]))
})

