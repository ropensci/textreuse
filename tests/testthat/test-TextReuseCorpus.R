context("TextReuseCorpus")

ny <- system.file("extdata/ny1850-match.txt", package = "textreuse")
ca1 <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
ca2 <- system.file("extdata/ca1851-match.txt", package = "textreuse")
paths <- c(ca2, ca1, ny)
dir <- system.file("extdata", package = "textreuse")
meta <- list("Corpus name" = "B")
corpus_a <- TextReuseCorpus(paths = paths)
corpus_b <- TextReuseCorpus(dir = dir)

test_that("loads from paths or directory identically", {
  expect_identical(corpus_a, corpus_b)
})

test_that("has metadata", {
  expect_is(meta(corpus_a), "list")
  meta(corpus_b) <- meta
  expect_identical(meta(corpus_b), meta)
})

test_that("has accessor functions", {
  expect_equal(names(corpus_a), paths)
  names(corpus_b) <- letters[1:3]
  expect_equal(names(corpus_b), letters[1:3])
  expect_equal(length(corpus_a), 3)
})

test_that("has the right classes", {
  expect_is(corpus_a, "TextReuseCorpus")
  expect_is(corpus_a, "Corpus")
})

test_that("has subset methods", {
  expect_identical(corpus_a[[1]], TextReuseTextDocument(file = paths[1]))
  # by file path
  expect_identical(corpus_a[[paths[3]]], TextReuseTextDocument(file = paths[3]))
  expect_equal(length(corpus_a[2:3]), 2)
})

test_that("prints sensibly", {
  expect_output(corpus_a, "TextReuseCorpus")
  expect_output(corpus_a, "Number of documents: 3")
})
