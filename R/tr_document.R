#' Create an document object
#'
#' This function either reads in a character vector or a file and creates an
#' object of class \code{tr_document}. This class is used for comparing
#' documents.
#'
#' @param string A character vector of length 1.
#' @param file The path to a file, if \code{string} is not provided.
#' @param n The value of \code{n} in the n-grams that are created by
#'   \link{ngrams}.
#' @param ... Arguments are passed on to \link{readLines} if \code{file} is
#'   provided.
#'
#' @return An object of class \code{tr_document}. This object inherits from
#'   \code{list}, and contains the following: \code{text} (the document text),
#'   \code{file} (the path to the file), \code{ngrams} (the n-grams created from
#'   the document).
#' @export
tr_document <- function(string, file = NULL, n = 5, ...) {

  assert_that(is.string(string))

  if (!is.null(file) && missing(string)) {
    assert_that(is.readable(file))
    string <- readLines(file, ...) %>%
      str_c(collapse = " ")
  }

  structure(list(
    text   = string,
    file   = file,
    ngrams = ngrams(string, n = n)
  ), class = "tr_document")
}

#' @export
tr_document.print <- function(x) {
  print(x$text)
}
