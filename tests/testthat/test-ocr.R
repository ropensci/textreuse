context("OCR quality")

dickens <- "Itt was thle best of times, it was thle worst of times, it was the age of wisdom, it was the age of foolishness."

words_used <- c("itt", "was", "thle", "best", "of", "times", "it", "was", "thle",
                "worst", "of", "times", "it", "was", "the", "age", "of",
                "wisdom", "it", "was", "the", "age", "of", "foolishness")

dictionary_results <- ocr_quality(dickens, method = "dictionary")

test_that("word_list returns list of words used", {
  expect_equal(word_list(dickens), words_used)
})

test_that("OCR quality returns a value between 0 and 1", {
  expect_less_than(dictionary_results, 1)
  expect_more_than(dictionary_results, 0)
})

test_that("OCR quality is weighted according to number of mismatches", {
  expect_equal(dictionary_results, 0.875)
})

test_that("OCR quality expects a string", {
  expect_error(ocr_quality(c("One", "Two")))
})

