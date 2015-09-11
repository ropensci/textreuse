context("Utils")

test_that("as_string returns the correct type", {
  s <- as_string(c("First", "Second"))
  expect_is(s, "String")
  expect_equal(as.character(s), "First Second")
})
