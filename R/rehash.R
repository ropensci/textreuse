#' Recompute the hashes for a document or corpus
#'
#' Given a \code{\link{TextReuseTextDocument}} or a
#' \code{\link{TextReuseCorpus}}, this function recomputes either the hashes or
#' the minhashes with the function specified. This implies that you have
#' retained the tokens with the \code{keep_tokens = TRUE} parameter.
#'
#' @param x A \code{\link{TextReuseTextDocument}} or
#'   \code{\link{TextReuseCorpus}}.
#' @param func A function to either hash the tokens or to generate the minhash
#'   signature. See \code{\link{hash_string}}, \code{\link{minhash_generator}}.
#' @param type Recompute the \code{hashes} or \code{minhashes}?
#'
#' @return The modified \code{\link{TextReuseTextDocument}} or
#'   \code{\link{TextReuseCorpus}}.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash1 <- minhash_generator(seed = 1)
#' corpus <- TextReuseCorpus(dir = dir, minhash_func = minhash1, keep_tokens = TRUE)
#' head(minhashes(corpus[[1]]))
#' minhash2 <- minhash_generator(seed = 2)
#' corpus <- rehash(corpus, minhash2, type = "minhashes")
#' head(minhashes(corpus[[2]]))
#'
#' @export
rehash <- function(x, func, type = c("hashes", "minhashes")) {
  UseMethod("rehash", x)
}

#' @export
rehash.TextReuseTextDocument <- function(x, func,
                                         type = c("hashes", "minhashes")) {
  assert_that(has_tokens(x),
              is.function(func))
  type <- match.arg(type)

  if (type == "hashes") {
    x$hashes <- func(x$tokens)
    x$meta$hash_func <- as.character(substitute(func))
  } else if (type == "minhashes") {
    x$minhashes <- func(x$tokens)
    x$meta$minhash_func <- as.character(substitute(func))
  }
  x
}

#' @export
rehash.TextReuseCorpus <- function(x, func,  type = c("hashes", "minhashes")) {
  assert_that(is.function(func))
  type <- match.arg(type)
  apply_func <- get_apply_function()
  x$documents <- apply_func(x$documents, rehash, func, type)
  if (type == "hashes")
    x$meta$hash_func <- as.character(substitute(func))
  else if (type == "minhashes")
    x$meta$minhash_func <- as.character(substitute(func))
  x
}
