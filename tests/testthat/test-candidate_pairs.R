context("Candidate pairs")
dir <- system.file("extdata/legal", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir)
m <- pairwise_compare(corpus, ratio_of_matches, directional = TRUE)
pairs <- pairwise_candidates(m)

test_that("returns a data frame with correct properties", {
  expect_equal(nrow(pairs), 6)
  expect_is(pairs, "data.frame")
  expect_is(pairs, "tbl_df")
  expect_is(pairs, "textreuse_candidates")
  expect_named(pairs, c("a", "b", "score"))
})

test_that("can be converted to matrix", {
  m <- as.matrix(pairs)
  expect_is(m, "matrix")
  expect_equal(names(corpus), colnames(m))
})

test_that("matrix conversion can keep all document ids", {
  pairs_with_ids <- pairs[1, ]
  attr(pairs_with_ids, "all-doc-ids") <- names(corpus)
  m <- as.matrix(pairs_with_ids)
  expect_equal(names(corpus), colnames(m))
  expect_equal(names(corpus), rownames(m))
})

test_that("can be converted to sparse matrix", {
  m <- as_sparse_matrix(pairs)
  expect_is(m, "dgCMatrix")
  expect_equal(names(corpus), colnames(m))
  expect_equal(as.matrix(m), as.matrix(pairs))
})
