context("N-grams")

sentence <- "This is a sentence which has a number of words in it; also some
             tricky puncuation and spacing. Does it work?"

test_that("n-grams can be generated", {
  results <- c("This is a sentence which",
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
               "tricky puncuation and spacing Does",
               "puncuation and spacing Does it",
               "and spacing Does it work")
  expect_equal(ngrams(sentence, n = 5), results)
})

test_that("different values of n work", {
  n3 <- ngrams(sentence, n = 3)
  n5 <- ngrams(sentence, n = 5)

  expect_equal(str_split(n3[1], boundary("word"))[[1]] %>% length(), 3)
  expect_equal(str_split(n5[1], boundary("word"))[[1]] %>% length(), 5)
})
