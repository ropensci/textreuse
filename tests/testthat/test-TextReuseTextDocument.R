context("TextReuseTextDocument")

doc <- TextReuseTextDocument(file = "newman.txt")
test_meta <- list(id = "test")

test_that("inherits from the correct classes", {
  expect_is(doc, c("TextReuseTextDocument", "TextDocument"))
})

test_that("has the correct structure", {
  expect_named(doc, c("content", "tokens", "hashes", "meta"))
})

test_that("can set the metadata", {
  expect_named(meta(doc), c("file", "id"))
  doc2 <- TextReuseTextDocument(file = "newman.txt",
                                meta = list(author = "Newman, John Henry"))
  expect_named(meta(doc2), c("author", "file", "id"))
  expect_equal(meta(doc2, "author"), "Newman, John Henry")
})

test_that("provides the necessary methods", {
  expect_is(as.character(doc), c("String", "character"))
  expect_is(content(doc), "String", "character")
  expect_output(print(doc), "And now that I am about to trace")
  expect_is(meta(doc), c("list"))

  doc3 <- TextReuseTextDocument(file = "newman.txt")
  meta(doc3)    <- list("author" = "Newman, John Henry")
  expect_equal(meta(doc3), list("author" = "Newman, John Henry"))
  content(doc3) <- "Replacing content"
  expect_equal(content(doc3), "Replacing content")
  meta(doc3, "author") <- "Cardinal Newman"
  expect_equal(meta(doc3, "author"), "Cardinal Newman")
})

test_that("has correct tokens", {
  expect_equal(head(doc$tokens, 3),
               c("and now that", "now that i", "that i am"))
})

test_that("can be created from a character vector not just a file", {
  text <- "This is the text of the document."
  doc <- TextReuseTextDocument(text, meta = test_meta)
  expect_equal(text, as.character(doc))
})

test_that("can be retokenized", {
  text <- "This is the text. But also this."
  a <- TextReuseTextDocument(text, meta = test_meta, tokenizer = tokenize_words)
  b <- tokenize(a, tokenizer = tokenize_sentences)
  expect_false(identical(tokens(a), tokens(b)))
  expect_false(identical(hashes(a), hashes(b)))
  expect_equal(tokens(a), c("this", "is", "the", "text", "but", "also", "this"))
  expect_equal(tokens(b), c("this is the text", "but also this"))
})
