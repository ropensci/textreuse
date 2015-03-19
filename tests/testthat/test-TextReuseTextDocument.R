context("TextReuseTextDocument")

doc <- TextReuseTextDocument("newman.txt")

test_that("inherits from the correct classes", {
  expect_is(doc, c("TextReuseTextDocument", "TextDocument"))
})

test_that("has the correct structure", {
  expect_named(doc, c("content", "ngrams", "meta"))
})

test_that("can set the metadata", {
  expect_named(meta(doc), c("file"))
  doc2 <- TextReuseTextDocument("newman.txt",
                                meta = list(author = "Newman, John Henry"))
  expect_named(meta(doc2), c("author", "file"))
  expect_equal(meta(doc2, "author"), "Newman, John Henry")
})

test_that("provides the necessary methods", {
  expect_is(as.character(doc), c("String", "character"))
  expect_is(content(doc), "String", "character")
  expect_output(print(doc), "And now that I am about to trace")
  expect_is(meta(doc), c("list"))
})

test_that("has correct n-grams", {
  expect_equal(head(doc$ngrams, 3),
               c("and now that i am", "now that i am about", "that i am about to"))
})
