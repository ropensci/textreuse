context("Filenames")

paths <- c("corpus/one.txt", "deep/corpus/two.R", "~/home/three.markdown",
           "/corpus/four.md", "../corpus/five.text")
test_that("properly returns filenames", {
  expect_equal(filenames(paths), c("one", "two", "three", "four", "five"))
  expect_equal(filenames(paths, extension = TRUE), basename(paths))
})
