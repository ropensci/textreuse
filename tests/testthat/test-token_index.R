context("Token index")

texts <- c(a = "one two three four",
           b = "one two three five",
           c = "six seven eight nine")
corpus <- TextReuseCorpus(text = texts, tokenizer = tokenize_ngrams, n = 2,
                          keep_tokens = TRUE)

test_that("token index maps tokens to documents", {
  index <- token_index(corpus, min_doc_count = 2)
  expect_is(index, "textreuse_token_index")
  expect_equal(index$token, c("one two", "two three"))
  expect_equal(index$n_docs, c(2L, 2L))
  expect_equal(index$docs[[1]], c("a", "b"))
})

test_that("token index can remove common tokens", {
  index <- token_index(corpus, min_doc_count = 2, max_doc_count = 1)
  expect_equal(nrow(index), 0)
})

test_that("token index can return candidate pairs", {
  index <- token_index(corpus, min_doc_count = 2)
  candidates <- token_index_candidates(index)
  expect_is(candidates, "textreuse_candidates")
  expect_equal(candidates$a, "a")
  expect_equal(candidates$b, "b")
  expect_equal(candidates$score, NA_real_)
})
