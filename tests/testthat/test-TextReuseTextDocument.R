context("TextReuseTextDocument")

doc <- TextReuseTextDocument(file = "newman.txt", keep_tokens = TRUE)
test_meta <- list(id = "test")

test_that("inherits from the correct classes", {
  expect_is(doc, c("TextReuseTextDocument", "TextDocument"))
})

test_that("has the correct structure", {
  expect_named(doc, c("content", "tokens", "hashes", "minhashes", "meta"))
})

test_that("can set the metadata", {
  expect_named(meta(doc), c("file", "hash_func", "id", "minhash_func",
                            "tokenizer"))
  doc2 <- TextReuseTextDocument(file = "newman.txt",
                                meta = list(author = "Newman, John Henry"))
  expect_named(meta(doc2), c("author", "file", "hash_func", "id", "tokenizer"))
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
  a <- TextReuseTextDocument(text, meta = test_meta, tokenizer = tokenize_words,
                             keep_tokens = TRUE)
  b <- tokenize(a, tokenizer = tokenize_sentences, keep_tokens = TRUE)
  expect_false(identical(tokens(a), tokens(b)))
  expect_false(identical(hashes(a), hashes(b)))
  expect_equal(tokens(a), c("this", "is", "the", "text", "but", "also", "this"))
  expect_equal(tokens(b), c("this is the text", "but also this"))
})

test_that("can rehash if it has tokens", {
  tokenless <- TextReuseTextDocument(file = "newman.txt")
  expect_true(has_tokens(doc))
  expect_true(!has_tokens(tokenless))
  expect_is(hashes(rehash(doc, hash_string)), "integer")
  expect_error(rehash(tokenless), "tokens")
})

test_that("can be created with no tokens", {
  doc <- TextReuseTextDocument(file = "newman.txt", tokenizer = NULL)
  expect_false(has_tokens(doc))
  expect_false(has_hashes(doc))
})

test_that("skips documents that are too short", {
  expect_warning(short_doc <-
                   TextReuseTextDocument(text = "Too short",
                                         meta = list(id = "short"),
                                         skip_short = TRUE),
                 "Skipping document with ID")
  expect_null(short_doc)
})

test_that("gives warning when skipping short documents from file", {
  skip_on_cran()
  too_short <- tempfile("tooshort", fileext = ".txt")
  writeLines("Two words", too_short)
  expect_warning(short_doc <- TextReuseTextDocument(file = too_short, n = 5),
                 "Skipping document with ID 'tooshort")
  expect_null(short_doc)
  file.remove(too_short)
})
