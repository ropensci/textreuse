#' TextReuseTextDocument
#'
#' This is the constructor function for \code{TextReuseTextDocument} objects.
#' This class is used for comparing documents.
#'
#' @param text A character vector containing the text of the document. This
#'   argument can be skipped if supplying \code{file}.
#' @param file The path to a text file, if \code{text} is not provided.
#' @param meta A list with named elements for the metadata associated with this
#'   document. If a document is created using the \code{text} parameter, then
#'   you must provide an \code{id} field, e.g., \code{meta = list(id =
#'   "my_id")}. If the document is created using \code{file}, then the ID will
#'   be created from the file name.
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
#' doc  <- TextReuseTextDocument(file = file, meta = list(id = "ny1850"))
#' print(doc)
#' meta(doc)
#' head(tokens(doc))
#' head(hashes(doc))
#' \dontrun{
#' content(doc)
#' }
#' @export
TextReuseTextDocument <- function(text, file = NULL, meta = list(),
                                  tokenizer = tokenize_ngrams, ...,
                                  hash_func = hash_string,
                                  keep_tokens = FALSE, keep_text = TRUE) {

  if (!missing(text)) assert_that(has_id(meta))

  if (!is.null(file)) {
    assert_that(missing(text),
                is.readable(file))
    text <- as_string(readLines(file), "\n")
  }

  assert_that(is.character(text))
  text <- as_string(text)

  assert_that(is.function(tokenizer))
  tokenizer_name <- as.character(substitute(tokenizer))
  tokens <- tokenizer(text, ...)

  assert_that(is.function(hash_func))
  hash_func_name <- as.character(substitute(hash_func))
  hashes <- hash_func(tokens)

  if (!keep_tokens) tokens <- NULL
  if (!keep_text) text <- NULL

  if (missing(meta)) {
    meta <- list(file = file,
                 id = filenames(file),
                 tokenizer = tokenizer_name,
                 hash_func = hash_func_name)
  }
  assert_that(is.list(meta))
  if (!is.null(file)) {
    meta$file <- file
    meta$id <- filenames(file)
  }
  # Don't overwrite these when called from TextReuseCorpus
  if (is.null(meta$tokenizer) & is.null(meta$hash_func)) {
    meta$tokenizer <- tokenizer_name
    meta$hash_func <- hash_func_name
  }

  meta <- sort_meta(meta)

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
  cat("TextReuseTextDocument\n")
  pretty_print_metadata(x)
  cat("content", ":", str_sub(x$content, end = 200))
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
#' @return Either a vector or a named list of vectors.
NULL

#' @export
#' @rdname TextReuseTextDocument-accessors
tokens <- function(x) UseMethod("tokens", x)

#' @export
tokens.TextReuseTextDocument <- function(x) x$tokens

#' @export
tokens.TextReuseCorpus <- function(x) {
  corpus_names <- names(x)
  l <- lapply(x$documents, function(i) tokens(i))
  names(l) <- corpus_names
  l
}

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
hashes.TextReuseCorpus <- function(x) {
  corpus_names <- names(x)
  l <- lapply(x$documents, function(i) hashes(i))
  names(l) <- corpus_names
  l
}

#' @export
#' @rdname TextReuseTextDocument-accessors
`hashes<-` <- function(x, value) UseMethod("hashes<-", x)

#' @export
`hashes<-.TextReuseTextDocument` <- function(x, value) {
  x$hashes <- value
  x
}
