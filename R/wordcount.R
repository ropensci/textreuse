#' Count words
#'
#' This function counts words in a text, for example, a character vector, a
#' \code{\link{TextReuseTextDocument}}, or some other object that inherits from
#' \code{\link[NLP]{TextDocument}}.
#'
#' @param x The object containing a text.
#' @export
#' @return An integer value for the word count.
wordcount <- function(x) UseMethod("wordcount", x)

#' @export
wordcount.default <- function(x) {
  assert_that(is.string(x))
  str_count(x, boundary("word"))
}

#' @export
wordcount.TextDocument <- function(x) wordcount(x$content)
