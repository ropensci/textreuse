context("Ratio of matches")

test_that("calculates the value correctly", {
  expect_equal(ratio_of_matches(1:4, 3:5), 2/3)
  expect_equal(ratio_of_matches(letters[1:3], letters[3:4]), 0.5)
  a <- sample(letters, 10, replace = TRUE)
  b <- sample(letters, 10, replace = TRUE)
  res <- ratio_of_matches(a, b)
  expect_true(0 <= res & res <= 1)
})

test_that("counts matches directly", {
  expect_equal(count_matches(1:4, 3:5), 2L)
  expect_equal(count_matches(letters[1:3], letters[3:4]), 1L)
  expect_equal(count_matches(c("a", "b"), c("a", "a", "c")), 2L)
})

test_that("returns matching tokens", {
  expect_equal(matching_tokens(c("a", "b"), c("a", "a", "c")), c("a", "a"))
  expect_equal(matching_tokens(1:4, 3:5), 3:4)
})

test_that("works with TextReuseTextDocument", {
  ny <- system.file("extdata/legal/ny1850-match.txt", package = "textreuse")
  ca <- system.file("extdata/legal/ca1851-match.txt", package = "textreuse")
  ny <- TextReuseTextDocument(ny, meta = list(id = "ny"))
  ca <- TextReuseTextDocument(ca, meta = list(id = "ca"))
  expect_is(jaccard_similarity(ny, ca), "numeric")
})

test_that("counts and returns matching TextReuseTextDocument tokens", {
  a <- TextReuseTextDocument("one two three four",
                             meta = list(id = "a"),
                             tokenizer = tokenize_ngrams,
                             n = 2,
                             keep_tokens = TRUE)
  b <- TextReuseTextDocument("zero one two three",
                             meta = list(id = "b"),
                             tokenizer = tokenize_ngrams,
                             n = 2,
                             keep_tokens = TRUE)
  expect_equal(count_matches(a, b), 2L)
  expect_equal(matching_tokens(a, b), c("one two", "two three"))
})
