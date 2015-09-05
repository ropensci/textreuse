#' TextReuseTextDocument
#'
#' This is the contructor function for \code{TextReuseTextDocument} objects.
#' This class is used for comparing documents.
#'
#' @param text A character vector containing the text of the document. This
#'   argument can be skipped if supplying \code{file}.
#' @param file The path to a text file, if \code{text} is not provided.
#' @param meta A list with named elements for the metadata associated with this
#'   document.
#' @param tokenizer A function to split the text into tokens. See
#'   \code{\link{tokenizers}}.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param keep_tokens Should the tokens be saved in the document that is
#'   returned or discarded?
#' @param keep_text Should the text be saved in the document that is returned or
#'   discarded?
#'
#' @details This constructor function follows a three-step process. It reads in
#'   the text, either from a file or from memory. It then tokenizes that text.
#'   Then it hashes the tokens. Most of the comparison functions in this package
#'   rely only on the hashes to make the comparison. By passing \code{FALSE} to
#'   \code{keep_tokens} and \code{keep_text}, you can avoid saving those
#'   objects, which can result in significant memory savings for large corpora.
#'
#' @return An object of class \code{TextReuseTextDocument}. This object inherits
#'   from the virtual S3 class \code{\link[NLP]{TextDocument}} in the NLP
#'   package. It contains the following elements: \describe{ \item{content}{The
#'   text of the document.} \item{tokens}{The tokens created from the text.}
#'   \item{hashes}{Hashes created from the tokens.} \item{metadata}{The document
#'   metadata, including the filename (if any) in \code{file}.} }
#'
#' @examples
#' file <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' doc  <- TextReuseTextDocument(file = file, meta = list(title = "NY 1850"))
#' print(doc)
#' meta(doc)
#' head(tokens(doc))
#' head(hashes(doc))
#' \dontrun{
#' content(doc)
#' }
#' @export
TextReuseTextDocument <- function(text, file = NULL, meta = NULL,
                                  tokenizer = ngrams, ...,
                                  hash_func = hash_string,
                                  keep_tokens = TRUE, keep_text = TRUE) {

  if (!is.null(file)) {
    assert_that(missing(text))
    assert_that(is.readable(file))
    text <- as_string(readLines(file), "\n")
  }

  assert_that(is.character(text))
  text <- as_string(text)

  assert_that(is.function(tokenizer))
  tokens <- tokenizer(text, ...)

  hashes <- hash_func(tokens)

  if (!keep_tokens) tokens <- NULL
  if (!keep_text) text <- NULL

  if (missing(meta)) {
    meta <- list(file = file)
  } else {
    assert_that(is.list(meta))
    meta$file <- file
  }

  doc <- list(
    content = text,
    tokens  = tokens,
    hashes  = hashes,
    meta    = meta
    )

  class(doc) <- c("TextReuseTextDocument", "TextDocument")

  doc

}

#' Accessors for TextReuseTextDocument objects
#'
#' The \code{meta} and \code{content} generics are defined by the NLP package. See the \code{\link[NLP]{generics}} documentation in that package.
#'
#' @importFrom NLP meta
#' @name meta
#' @export
#' @rdname NLP-imports
NULL

#' @importFrom NLP meta<-
#' @name meta<-
#' @export
#' @rdname NLP-imports
NULL

#' @importFrom NLP content
#' @name content
#' @export
#' @rdname NLP-imports
NULL

#' @importFrom NLP content<-
#' @name content<-
#' @export
#' @rdname NLP-imports
NULL

#' @export
print.TextReuseTextDocument <- function(x, ...) {
  x$content %>%
    str_sub(end = 100) %>%
    print()
  invisible(x)
}

#' @export
as.character.TextReuseTextDocument <- function(x, ...) {
  as.character(x$content)
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
meta.TextReuseTextDocument <- function(x, tag = NULL, ...) {
  if (is.null(tag))
    x$meta
  else
    x$meta[[tag]]
}

#' @export
#' @method meta<- TextReuseTextDocument
`meta<-.TextReuseTextDocument` <- function(x, tag = NULL, ..., value) {
  if (is.null(tag)) {
    assert_that(is.list(value))
    x$meta <- value
  } else {
    x$meta[[tag]] <- value
  }
  x
}

#' Accessors for TextReuseTextDocument objects
#'
#' Accessor functions to read and write components of
#' \code{\link{TextReuseTextDocument}} objects.
#' @name TextReuseTextDocument-accessors
#' @param x The object to acess.
#' @param value The value to assign.
NULL

#' @export
#' @rdname TextReuseTextDocument-accessors
tokens <- function(x) UseMethod("tokens", x)

#' @export
tokens.TextReuseTextDocument <- function(x) x$tokens

#' @export
#' @rdname TextReuseTextDocument-accessors
`tokens<-` <- function(x, value) UseMethod("tokens<-", x)

#' @export
`tokens<-.TextReuseTextDocument` <- function(x, value) {
  x$tokens <- value
  x
}

#' @export
#' @rdname TextReuseTextDocument-accessors
hashes <- function(x) UseMethod("hashes", x)

#' @export
hashes.TextReuseTextDocument <- function(x) x$hashes

#' @export
#' @rdname TextReuseTextDocument-accessors
`hashes<-` <- function(x, value) UseMethod("hashes<-", x)

#' @export
`hashes<-.TextReuseTextDocument` <- function(x, value) {
  x$hashes <- value
  x
}
