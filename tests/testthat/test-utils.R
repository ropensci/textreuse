context("Utils")

test_that("as_string returns the correct type", {
  s <- as_string(c("First", "Second"))
  expect_is(s, "String")
  expect_equal(as.character(s), "First\nSecond")
})

test_that("data frames can be sorted by rows", {
  df1 <- data.frame(a = letters[1:3], b = letters[24:26], stringsAsFactors = FALSE)
  df2 <- data.frame(a = letters[24:26], b = letters[1:3], stringsAsFactors = FALSE)
  expect_identical(sort_df_by_rows(df2), df1)
})

test_that("data frames can be sorted by columns", {
  df1 <- data.frame(a = letters[c(2, 3, 1)], b = letters[c(25, 26, 24)],
                    stringsAsFactors = FALSE)
  df2 <- data.frame(a = letters[1:3], b = letters[24:26],
                    stringsAsFactors = FALSE)
  df1 <- textreuse:::sort_df_by_columns(df1)
  rownames(df1) <- NULL
  rownames(df2) <- NULL
  expect_identical(df1, df2)
})

test_that("edit marks can be created correctly", {
  expect_equal(mark_chars("word", "#"), "####")
})
