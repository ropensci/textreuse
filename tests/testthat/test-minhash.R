context("Minhash")

mhash <- minhash_generator()
file <- system.file("extdata/ny1850-match.txt", package = "textreuse")
doc <- TextReuseTextDocument(file = file, hash_func = mhash, keep_tokens = TRUE)

test_that("random integers can be generated", {
  expect_is(random_ints(3), "integer")
  expect_equal(length(random_ints(5)), 5)
})

test_that("minhash generator returns a proper function", {
  expect_is(mhash, "function")
  e <- environment(mhash)
  expect_is(e$r, "integer")
  expect_equal(length(e$r), 200)
})

test_that("minhash works on a TextReuseTextDocument", {
  expect_equal(length(hashes(doc)), 200)
  expect_is(hashes(doc), "integer")
  expect_equal(mhash(tokens(doc)), hashes(doc))
})
