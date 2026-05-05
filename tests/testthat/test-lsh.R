context("LSH")

dir <- system.file("extdata/legal", package = "textreuse")
minhash <- minhash_generator(200, seed = 9228)
corpus <- TextReuseCorpus(dir = dir,
                          tokenizer = tokenize_ngrams, n = 5,
                          keep_tokens = TRUE,
                          minhash_func = minhash)
buckets <- lsh(corpus, bands = 50)
candidates <- lsh_candidates(buckets)
scores <- lsh_compare(candidates, corpus, jaccard_similarity)

test_that("returns a data frame", {
  expect_is(buckets, "tbl_df")
})

test_that("returns error if improper number of bands are chosen", {
  expect_error(lsh(corpus, bands = 33), "The number of hashes")
})

test_that("creates buckets without warnings", {
  expect_warning(lsh(corpus, bands = 50), NA)
})

test_that("returns pairs of candidates without duplicates", {
  expect_is(candidates, "data.frame")
  expect_named(candidates, c("a", "b", "score"))
  expect_equal(candidates[[1, 1]], "ca1851-match")
  expect_equal(candidates[[1, 2]], "ny1850-match")
  expect_equal(candidates[[1, 3]], NA_real_)
  expect_equal(nrow(candidates), 1)
})

test_that("additional documents can be added", {
  corpus2 <- rehash(corpus, minhash, type = "minhashes")
  buckets1and2 <- lsh(corpus2[1:2], bands = 50)
  buckets3 <- lsh(corpus2[[3]], bands = 50)
  buckets_combined <- dplyr::bind_rows(buckets1and2, buckets3)
  expect_equal(buckets_combined$buckets, buckets$buckets)
  expect_equal(buckets_combined$doc, buckets$doc)
})

test_that("lsh buckets can be extended with new documents", {
  corpus2 <- rehash(corpus, minhash, type = "minhashes")
  buckets1and2 <- lsh(corpus2[1:2], bands = 50)
  buckets_added <- lsh_add(buckets1and2, corpus2[[3]], bands = 50)
  expect_is(buckets_added, "lsh_buckets")
  expect_equal(buckets_added$buckets, buckets$buckets)
  expect_equal(buckets_added$doc, buckets$doc)
})

test_that("lsh buckets are replaced when adding an existing document", {
  corpus2 <- rehash(corpus, minhash, type = "minhashes")
  buckets_added <- lsh_add(buckets[1:100, ], corpus2[[1]], bands = 50)
  expect_equal(sum(buckets_added$doc == names(corpus2)[1]), 50)
})

test_that("candidates can be scored", {
  correct <- jaccard_similarity(corpus[["ca1851-match"]],
                                corpus[["ny1850-match"]])
  expect_equal(scores[[1,3]], correct)
})

test_that("pre-scored candidates are not recalculated", {
  candidates_scored <- candidates
  candidates_scored$score[1] <- 1
  rescored <- lsh_compare(candidates_scored, corpus, function(a, b) 0)
  expect_equal(rescored$score[1], 1)
})

test_that("scores can be converted to a matrix", {
  m <- as.matrix(scores)
  expect_is(m, "matrix")
  expect_equal(colnames(m), names(corpus))
})

test_that("can be queried for a single document", {
  match <- lsh_query(buckets, "ca1851-match")
  expect_equal(match$b, "ny1850-match")
  expect_is(match, "textreuse_candidates")
})
