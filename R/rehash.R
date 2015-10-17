#' Recompute the hashes for a document or corpus
#'
#' Given a \code{\link{TextReuseTextDocument}} or a
#' \code{\link{TextReuseCorpus}}, this function recomputes the hashes with the
#' function specified. This implies that you have retained the tokens with the
#' \code{keep_tokens = TRUE} parameter. This function is intended to be used
#' when changing from keeping only minhashes of the tokens to keeping hashes of
#' all the tokens. See the vignette on minhash/LSH.
#'
#' @param x A \code{\link{TextReuseTextDocument}} or
#'   \code{\link{TextReuseCorpus}}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}, \code{\link{minhash_generator}}.
#'
#' @return The modified \code{\link{TextReuseTextDocument}} or
#'   \code{\link{TextReuseCorpus}}.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash1 <- minhash_generator(seed = 1)
#' corpus <- TextReuseCorpus(dir = dir, hash_func = minhash1, keep_tokens = TRUE)
#' head(hashes(corpus[[1]]))
#' minhash2 <- minhash_generator(seed = 2)
#' corpus <- rehash(corpus, minhash2)
#' head(hashes(corpus[[2]]))
#'
#' @export
rehash <- function(x, hash_func) {
  UseMethod("rehash", x)
}

#' @export
rehash.TextReuseTextDocument <- function(x, hash_func) {
  assert_that(has_tokens(x),
              is.function(hash_func))
  x$hashes <- hash_func(x$tokens)
  x$meta$hash_func <- as.character(substitute(hash_func))
  x
}

#' @export
rehash.TextReuseCorpus <- function(x, hash_func) {
  x$documents <- lapply(x$documents, rehash, hash_func)
  x$meta$hash_func <- as.character(substitute(hash_func))
  x
}
