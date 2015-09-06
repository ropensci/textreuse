context("Tokenizers")

sentence <- "This is a sentence which has a number of words in it; also some
             tricky puncuation and spacing. Does it work?"

test_that("n-grams can be generated", {
  results <- c("this is a sentence which",
               "is a sentence which has",
               "a sentence which has a",
               "sentence which has a number",
               "which has a number of",
               "has a number of words",
               "a number of words in",
               "number of words in it",
               "of words in it also",
               "words in it also some",
               "in it also some tricky",
               "it also some tricky puncuation",
               "also some tricky puncuation and",
               "some tricky puncuation and spacing",
               "tricky puncuation and spacing does",
               "puncuation and spacing does it",
               "and spacing does it work")
  expect_equal(tokenize_ngrams(sentence, n = 5), results)
})

test_that("different values of n work", {
  n3 <- tokenize_ngrams(sentence, n = 3)
  n5 <- tokenize_ngrams(sentence, n = 5)

  expect_equal(str_split(n3[1], boundary("word"))[[1]] %>% length(), 3)
  expect_equal(str_split(n5[1], boundary("word"))[[1]] %>% length(), 5)
})

test_that("ngrams can be uppercase", {
  sentence <- tokenize_ngrams("This is a Capital Word.", n = 4, lowercase = FALSE)
  results  <- c("This is a Capital", "is a Capital Word")
  expect_equal(sentence, results)
})

test_that("word tokenizer works", {
  expect_equal(tokenize_words("There are several; WORDS here"),
               c("there", "are", "several", "words", "here"))
  expect_equal(tokenize_words("There are several; WORDS here", lowercase = FALSE),
               c("There", "are", "several", "WORDS", "here"))
})


test_that("sentence tokenizers works", {
  expect_equal(tokenize_sentences("This is a---sentence. This too."),
               c("this is a sentence", "this too"))
  expect_equal(tokenize_sentences("This is a sentence. This too.", lowercase = FALSE),
               c("This is a sentence", "This too"))
})

test_that("tokenizers fail on non-strings", {
  text <- c("This is not a string", "because it is length two.")
  expect_error(tokenize_words(text), "string is not a string")
  expect_error(tokenize_sentences(text), "string is not a string")
  expect_error(tokenize_ngrams(text), "string is not a string")
  expect_error(tokenize_skip_ngrams(text), "string is not a string")
})

test_that("skip n-grams behave as expected", {
  dylan <- "How many roads must a man walk down"
  skips <- tokenize_skip_ngrams(dylan, n = 3, k = 2)
  correct <- c("how many roads", "many roads must", "roads must a", "must a man",
               "a man walk", "man walk down", "how must walk", "many a down",
               "how roads a", "many must man", "roads a walk", "must man down")
  expect_equal(sort(skips), sort(correct))
  expect_equal(tokenize_skip_ngrams(dylan, n = 5, k = 0),
               tokenize_ngrams(dylan, n = 5))
})
