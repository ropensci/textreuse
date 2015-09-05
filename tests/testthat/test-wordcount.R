context("Word counts")

test_that("counts words correctly for different classes", {
  w <- c("One two three four five six seven; all good children go to heaven.")
  expect_equal(wordcount(w), 13)
  w_doc <- TextReuseTextDocument(w)
  expect_equal(wordcount(w), 13)
})
