context("Hashing")

lines  <- system.file("extdata/legal/ny1850-match.txt", package = "textreuse") %>%
  readLines()
ngrams <- lines %>%
  str_c(collapse = " ") %>%
  tokenize_ngrams(n = 5)

lines_hashed  <- hash_string(lines)
ngrams_hashed <- hash_string(ngrams)

test_that("returns correct values", {
  expect_is(lines_hashed, "integer")
  expect_is(ngrams_hashed, "integer")
})

test_that("there are no collisions", {
  expect_equal(length(unique(lines)), length(unique(lines_hashed)))
  expect_equal(length(unique(ngrams)), length(unique(ngrams_hashed)))
})
