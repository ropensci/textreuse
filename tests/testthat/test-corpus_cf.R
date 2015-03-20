context("Corpus comparison")

test_that("works on an abstract problem", {
  x <- list(1, 2, 3)
  y <- list(10, 20, 30)
  res <- corpus_cf(x, y, `*`, id = NULL)
  expect_equal(res, matrix(c(10, 20, 30, 20, 40, 60, 30, 60, 90), nrow = 3))
})

test_that("works on TextReuseTextDocument corpus", {
  ny         <- system.file("extdata/ny1850-match.txt", package = "textreuse")
  ca_match   <- system.file("extdata/ca1851-match.txt", package = "textreuse")
  ca_nomatch <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
  ny         <- TextReuseTextDocument(ny, meta = list(id = "ny"))
  ca_match   <- TextReuseTextDocument(ca_match, meta = list(id = "ca_match"))
  ca_nomatch <- TextReuseTextDocument(ca_nomatch, meta = list(id = "ca_no"))
  corpus <- list(ny, ca_match, ca_nomatch)

  res1 <- corpus_cf(corpus, corpus, jaccard_coef, id = "id")
  res2 <- corpus_cf(corpus, corpus, ratio_of_matches, id = "id")

  expect_equal(rownames(res1), c("ny", "ca_match", "ca_no"))
  expect_equal(colnames(res1), c("ny", "ca_match", "ca_no"))
  expect_equal(rownames(res2), c("ny", "ca_match", "ca_no"))
  expect_equal(colnames(res2), c("ny", "ca_match", "ca_no"))

  jac_res <- jaccard_coef(ny, ca_match)
  rat_res1 <- ratio_of_matches(ny, ca_match)
  rat_res2 <- ratio_of_matches(ny, ca_nomatch)

  expect_equal(res1[1, 2], jac_res)
  # Test directionality of comparison
  expect_equal(res2[1, 2], rat_res1)
  expect_equal(res2[1, 3], rat_res2)
})
