context("LSH")

dir <- system.file("extdata/legal", package = "textreuse")
set.seed(4235)
minhash <- minhash_generator(200)
corpus <- TextReuseCorpus(dir = dir,
                          tokenizer = tokenize_ngrams, n = 5,
                          hash_func = minhash)
buckets <- lsh(corpus, bands = 50)
candidates <- lsh_candidates(buckets)
corpus2 <- tokenize(corpus, tokenizer = tokenize_ngrams, n = 5)
scores <- lsh_compare(candidates, corpus2, jaccard_similarity)

test_that("returns a bucket", {
  expect_is(buckets, "hash")
})

test_that("returns error if improper number of bands are chosen", {
  expect_error(lsh(corpus, bands = 33), "Bands times rows")
})

test_that("returns pairs of candidates", {
  expect_is(candidates, "data.frame")
  expect_named(candidates, c("a", "b", "score"))
  expect_equal(candidates[1, 1], "ca1851-match")
  expect_equal(candidates[1, 2], "ny1850-match")
  expect_equal(candidates[1, 3], NA_real_)
})

test_that("pairs of candidates not clusters",{
  fake_cache <- hash::hash("qwe" = c("a", "b"),
                           "asd" = c("c"),
                           "zxc" = c("b", "c", "e"))
  pairs <- lsh_candidates(fake_cache)
  expect_equal(nrow(pairs), 4)
})

test_that("additional documents can be added", {
  start <- length(buckets)
  newman <- TextReuseTextDocument(file = "newman.txt", hash_func = minhash)
  lsh(newman, bands = 50, buckets = buckets)
  end <- length(buckets)
  expect_equal(start + 50, end)
})

test_that("candidates can be scored", {
  correct <- jaccard_similarity(corpus2[["ca1851-match"]],
                                corpus2[["ny1850-match"]])
  expect_equal(scores[1,3], correct)
})
