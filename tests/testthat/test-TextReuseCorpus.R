context("TextReuseCorpus")

ny <- system.file("extdata/legal/ny1850-match.txt", package = "textreuse")
ca1 <- system.file("extdata/legal/ca1851-nomatch.txt", package = "textreuse")
ca2 <- system.file("extdata/legal/ca1851-match.txt", package = "textreuse")
paths <- c(ca2, ca1, ny)
dir <- system.file("extdata/legal", package = "textreuse")
meta <- list("Corpus name" = "B")
corpus_a <- TextReuseCorpus(paths = paths, keep_tokens = TRUE)
corpus_b <- TextReuseCorpus(dir = dir, keep_tokens = TRUE)

test_that("loads from paths or directory identically", {
  expect_identical(corpus_a, corpus_b)
})

test_that("has metadata", {
  expect_is(meta(corpus_a), "list")
  meta(corpus_b) <- meta
  expect_identical(meta(corpus_b), meta)
})

test_that("has accessor functions", {
  expect_equal(names(corpus_a), filenames(paths))
  names(corpus_b) <- letters[1:3]
  expect_equal(names(corpus_b), letters[1:3])
  expect_equal(length(corpus_a), 3)
})

test_that("has the right classes", {
  expect_is(corpus_a, "TextReuseCorpus")
  expect_is(corpus_a, "Corpus")
})

test_that("has subset methods", {
  expect_identical(corpus_a[[1]],
                   TextReuseTextDocument(file = paths[1], keep_tokens = TRUE))
  # by file path
  expect_identical(corpus_a[[filenames(paths[3])]],
                   TextReuseTextDocument(file = paths[3], keep_tokens = TRUE))
  expect_equal(length(corpus_a[2:3]), 2)
  expect_is(corpus_a[2:3], "TextReuseCorpus")
})

test_that("prints sensibly", {
  expect_output(corpus_a, "TextReuseCorpus")
  expect_output(corpus_a, "Number of documents: 3")
})

test_that("can be retokenized", {
  skip_on_appveyor()
  expect_equal(tokens(corpus_a[[1]])[1:2], c("4 every action", "every action shall"))
  corpus_a <- tokenize(corpus_a, tokenize_words)
  expect_equal(tokens(corpus_a[[1]])[1:2], c("4", "every"))
})

test_that("has methods for tokens and hashes", {
  t <- tokens(corpus_a)
  h <- hashes(corpus_b)
  expect_is(h, "list")
  expect_is(t, "list")
  expect_is(t[[2]], "character")
  expect_is(h[[1]], "integer")
  expect_named(t, names(corpus_a))
  expect_named(h, names(corpus_b))
})
