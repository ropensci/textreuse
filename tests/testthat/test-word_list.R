context("Word list")

dickens <- "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair."

words_used <- c("age", "belief", "best", "darkness", "despair", "epoch",
                "foolishness", "hope", "incredulity", "it", "light", "of",
                "season", "spring", "the", "times", "was", "winter", "wisdom",
                "worst")

test_that("returns a list of lowercase, alphabetized words", {
  expect_equal(word_list(dickens), words_used)
})

