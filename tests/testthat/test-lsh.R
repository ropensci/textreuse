context("LSH")

dir <- system.file("extdata", package = "textreuse")
minhash <- minhash_generator(200)
corpus <- TextReuseCorpus(dir = dir,
                          tokenizer = tokenize_ngrams, n = 5,
                          hash_func = minhash)
names(corpus) <- filenames(names(corpus))
buckets <- lsh(corpus, bands = 50)

test_that("returns a bucket", {
  expect_is(buckets, "hash")
})

test_that("returns error if improper number of bands are chosen", {
  expect_error(lsh(corpus, bands = 33), "Bands times rows")
})
