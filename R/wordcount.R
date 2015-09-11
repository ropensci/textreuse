#' Count words
#'
#' This function counts words in a text, for example, a character vector, a
#' \code{\link{TextReuseTextDocument}}, some other object that inherits from
#' \code{\link[NLP]{TextDocument}}, or a all the documents in a
#' \code{\link{TextReuseCorpus}}.
#'
#' @param x The object containing a text.
#' @export
#' @return An integer vector for the word count.
wordcount <- function(x) UseMethod("wordcount", x)

#' @export
wordcount.default <- function(x) {
  assert_that(is.string(x))
  str_count(x, boundary("word"))
}

#' @export
wordcount.TextDocument <- function(x) wordcount(x$content)

#' @export
wordcount.TextReuseCorpus <- function(x) {
  vapply(x$documents, wordcount, integer(1))
}
