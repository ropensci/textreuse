context("Minhash")

test_that("random integers can be generated", {
  expect_is(random_ints(3), "integer")
  expect_equal(length(random_ints(5)), 5)
})

test_that("minhash works on a character vector", {
  random <- random_ints(5)
  a <- minhash(c("To", "be", "or", "not", "to", "be"), random)
  b <- minhash(c("To", "be", "whether", "or", "not"), random)
  c <- minhash(c("This", "sentence", "is", "unlike", "the", "other"), random)
  expect_is(a, "integer")
  expect_equal(length(a), length(b))
  expect_true(jaccard_coef(a, b) > jaccard_coef(a, c))
})
