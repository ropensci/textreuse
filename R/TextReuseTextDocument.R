#' Create an TextReuseTextDocument object
#'
#' This function reads in a file and creates an object of class
#' \code{TextReuseTextDocument}. This class is used for comparing documents.
#'
#' @param file The path to a file, if \code{string} is not provided.
#' @param n The value of \code{n} in the n-grams that are created by
#'   \link{ngrams}.
#' @param meta A list with named elements for the metadata associated with this
#'   document.
#' @param ... Arguments are passed on to \link{readLines} if \code{file} is
#'   provided.
#'
#' @return An object of class \code{TextReuseTextDocument}. This object inherits
#'   from the virtual S3 class \code{\link[NLP]{TextDocument}} in the NLP
#'   package. It contains the following elements: \describe{ \item{content}{The
#'   text of the document.} \item{ngrams}{The document in shingled n-grams. See
#'   \code{\link{ngrams}}.} \item{metadata}{The document metadata, including the
#'   filename (if any) in \code{file}.} }
#' @export
TextReuseTextDocument <- function(file, n = 5, meta = NULL, ...) {

  assert_that(is.readable(file))
  text <- readLines(file, ...) %>%
      str_c(collapse = " ") %>%
      NLP::as.String()

  assert_that(is.count(n))
  ngrams <- ngrams(text, n = n)

  if (missing(meta)) {
    meta <- list(file = file)
  } else {
    assert_that(is.list(meta))
    meta$file <- file
  }

  doc <- list(
    content = text,
    ngrams  = ngrams,
    meta    = meta
    )

  class(doc) <- c("TextReuseTextDocument", "TextDocument")

  doc

}

#' @export
print.TextReuseTextDocument <- function(x, ...) {
  x$content %>%
    str_sub(end = 100) %>%
    print()
  invisible(x)
}

#' @export
as.character.TextReuseTextDocument <- function(x, ...) {
  x$content
}

#' @export
#' @method content TextReuseTextDocument
content.TextReuseTextDocument <- function(x) {
  x$content
}

#' @export
#' @method content<- TextReuseTextDocument
`content<-.TextReuseTextDocument` <- function(x, value) {
  assert_that(is.character(value))
  x$content <- value
  x
}

#' @export
#' @method meta TextReuseTextDocument
meta.TextReuseTextDocument <- function(doc, tag = NULL, ...) {
  if (is.null(tag))
    doc$meta
  else
    doc$meta[[tag]]
}

#' @export
#' @method meta<- TextReuseTextDocument
`meta<-.TextReuseTextDocument` <- function(x, value) {
  assert_that(is.list(value))
  x$meta <- value
  x
}