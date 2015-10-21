#' @export
align_local <- function(a, b, tokenizer = tokenize_words, ...,
                        match = 2L, mismatch = -1L, gap = -1L) {

  assert_that(is.character(a),
              is.character(b),
              is_integer_like(match),
              is_integer_like(mismatch),
              is_integer_like(gap))

  if (match <= 0 || mismatch > 0 || gap > 0) {
    stop("The scoring parameters should have the following characteristics:\n",
         "    - `match` should be a positive integer\n",
         "    - `mismatch` should be a negative integer or zero\n",
         "    - `gap` should be a negative integer or zero\n")
  }

  match    <- as.integer(match)
  mismatch <- as.integer(mismatch)
  gap      <- as.integer(gap)

  # Prepare the character vectors
  if (length(a) == 1 & length(b) == 1) {
    assert_that(is.function(tokenizer))
    a <- tokenizer(a, ...)
    b <- tokenizer(b, ...)
  } else {
    if (!missing(tokenizer))
      stop("Do not specify a tokenizer if providing character vectors.")
  }

  # Create the integer matrix
  m <- matrix(0L, length(b) + 1L, length(a) + 1L)
  rownames(m) <- c(NA, b)
  colnames(m) <- c(NA, a)

  m <- sw_matrix(m, a, b, match, mismatch, gap)

  m

}
