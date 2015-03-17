context("OCR quality")

dickens <- "Itt was thle best of times, it was thle worst of times, it was the age of wisdom, it was the age of foolishness."
shakespeare <- "Tooo be, or not tooo be---that is the question: Whether 'tis nobler in the mind to suffer; The slings and arrows of outrageous fortune..."

dictionary_results <- ocr_quality(c(dickens, shakespeare), method = "dictionary")

test_that("OCR quality returns a value between 0 and 1", {
  expect_less_than(max(dictionary_results), 1)
  expect_more_than(min(dictionary_results), 0)
})

test_that("OCR quality is weighted according to number of mismatches", {
  expect_equal(dictionary_results, c(0.875, 0.92))
})
