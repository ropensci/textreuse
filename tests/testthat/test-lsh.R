context("LSH")

dir <- system.file("extdata", package = "textreuse")
set.seed(4235)
minhash <- minhash_generator(200)
corpus <- TextReuseCorpus(dir = dir,
                          tokenizer = tokenize_ngrams, n = 5,
                          hash_func = minhash)
buckets <- lsh(corpus, bands = 50)
candidates <- lsh_candidates(buckets)

test_that("returns a bucket", {
  expect_is(buckets, "hash")
})

test_that("returns error if improper number of bands are chosen", {
  expect_error(lsh(corpus, bands = 33), "Bands times rows")
})

test_that("returns pairs of candidates", {
  expect_is(candidates, "list")
  expect_equal(candidates[[1]], c("ca1851-match", "ny1850-match"))
})

test_that("pairs of candidates or clusters of candidates",{
  fake_cache <- hash::hash("qwe" = c("a", "b"),
                           "asd" = c("c"),
                           "zxc" = c("b", "c", "e"))
  pairs <- lsh_candidates(fake_cache)
  clusters <- lsh_candidates(fake_cache, pairs = FALSE)
  expect_equal(length(pairs), 4)
  expect_equal(length(clusters), 2)
})

test_that("additional documents can be added", {
  start <- length(buckets)
  newman <- TextReuseTextDocument(file = "newman.txt", hash_func = minhash)
  lsh(newman, bands = 50, buckets = buckets)
  end <- length(buckets)
  expect_equal(start + 50, end)
})
