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
#'   \code{\link{tokenizers}}. If value is \code{NULL}, then tokenizing and
#'   hashing will be skipped.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param minhash_func A function to create minhash signatures of the document.
#'   See \code{\link{minhash_generator}}.
#' @param keep_tokens Should the tokens be saved in the document that is
#'   returned or discarded?
#' @param keep_text Should the text be saved in the document that is returned or
#'   discarded?
#' @param skip_short Should short documents be skipped? (See details.)
#'
#' @details This constructor function follows a three-step process. It reads in
#'   the text, either from a file or from memory. It then tokenizes that text.
#'   Then it hashes the tokens. Most of the comparison functions in this package
#'   rely only on the hashes to make the comparison. By passing \code{FALSE} to
#'   \code{keep_tokens} and \code{keep_text}, you can avoid saving those
#'   objects, which can result in significant memory savings for large corpora.
#'
#'   If \code{skip_short = TRUE}, this function will return \code{NULL} for very
#'   short or empty documents. A very short document is one where there are two
#'   few words to create at least two n-grams. For example, if five-grams are
#'   desired, then a document must be at least six words long. If no value of
#'   \code{n} is provided, then the function assumes a value of \code{n = 3}. A
#'   warning will be printed with the document ID of a skipped document.
#'
#' @return An object of class \code{TextReuseTextDocument}. This object inherits
#'   from the virtual S3 class \code{\link[NLP]{TextDocument}} in the NLP
#'   package. It contains the following elements: \describe{ \item{content}{The
#'   text of the document.} \item{tokens}{The tokens created from the text.}
#'   \item{hashes}{Hashes created from the tokens.} \item{minhashes}{The minhash
#'   signature of the document.} \item{metadata}{The document metadata,
#'   including the filename (if any) in \code{file}.} }
#'
#' @seealso \link[=TextReuseTextDocument-accessors]{Accessors for TextReuse
#'   objects}.
#'
#' @examples
#' file <- system.file("extdata/legal/ny1850-match.txt", package = "textreuse")
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
                                  minhash_func = NULL,
                                  keep_tokens = FALSE,
                                  keep_text = TRUE,
                                  skip_short = TRUE) {

  if (!missing(text)) assert_that(has_id(meta))

  if (!is.null(file)) {
    assert_that(missing(text),
                is.readable(file))
    text <- as_string(readLines(file))
  }

  assert_that(is.character(text))
  text <- as_string(text)

  # Define document ID early
  document_id <- ifelse(is.null(meta$id), filenames(file), meta$id)

  # Check length of document
  if (skip_short) {
    n_call <- match.call(expand.dots = TRUE)[["n"]]
    if (is.null(n_call))
      n_call <- 3
    if (wordcount(text) < n_call + 1) {
      warning("Skipping document with ID '", document_id,
              "' because it has too few words ",
              "to create at least two n-grams with n = ", n_call, ".",
              call. = FALSE, noBreaks. = TRUE)
      return(NULL)
    }
  }

  # Tokenize and hash
  if (!is.null(tokenizer)) {

    assert_that(is.function(tokenizer))
    tokenizer_name <- as.character(substitute(tokenizer))
    tokens <- tokenizer(text, ...)

    assert_that(is.function(hash_func))
    hash_func_name <- as.character(substitute(hash_func))
    hashes <- hash_func(tokens)

    # Also minhash if requested
    if (!is.null(minhash_func)) {
      assert_that(is.function(minhash_func))
      minhash_func_name <- as.character(substitute(minhash_func))
      minhashes <- minhash_func(tokens)
    } else {
      minhashes <- NULL
      minhash_func_name <- NULL
    }

  } else {
    tokens <- NULL
    hashes <- NULL
    minhashes <- NULL
    tokenizer_name <- NULL
    hash_func_name <- NULL
    minhash_func_name <- NULL

  }

  if (!keep_tokens) tokens <- NULL
  if (!keep_text) text <- NULL

  if (missing(meta)) {
    meta <- list(file = file,
                 id = document_id,
                 tokenizer = tokenizer_name,
                 hash_func = hash_func_name,
                 minhash_func = minhash_func_name)
  }
  assert_that(is.list(meta))
  if (!is.null(file)) {
    meta$file <- file
    meta$id <- document_id
  }
  # Don't overwrite these when called from TextReuseCorpus
  if (is.null(meta$tokenizer) & is.null(meta$hash_func) &
      is.null(meta$minhash_func)) {
    meta$tokenizer <- tokenizer_name
    meta$hash_func <- hash_func_name
    meta$minhash_func <- minhash_func_name
  }

  meta <- sort_meta(meta)

  doc <- list(
    content   = text,
    tokens    = tokens,
    hashes    = hashes,
    minhashes = minhashes,
    meta      = meta
    )

  class(doc) <- c("TextReuseTextDocument", "TextDocument")

  doc

}

#' @importFrom NLP meta
#' @export
NLP::meta

#' @importFrom NLP meta<-
#' @export
NLP::`meta<-`

#' @importFrom NLP content
#' @export
NLP::content

#' @importFrom NLP content<-
#' @export
NLP::`content<-`

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

#' Accessors for TextReuse objects
#'
#' Accessor functions to read and write components of
#' \code{\link{TextReuseTextDocument}} and \code{\link{TextReuseCorpus}}
#' objects.
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

#' @export
#' @rdname TextReuseTextDocument-accessors
minhashes <- function(x) UseMethod("minhashes", x)

#' @export
minhashes.TextReuseTextDocument <- function(x) x$minhashes

#' @export
minhashes.TextReuseCorpus <- function(x) {
  corpus_names <- names(x)
  l <- lapply(x$documents, function(i) minhashes(i))
  names(l) <- corpus_names
  l
}

#' @export
#' @rdname TextReuseTextDocument-accessors
`minhashes<-` <- function(x, value) UseMethod("minhashes<-", x)

#' @export
`minhashes<-.TextReuseTextDocument` <- function(x, value) {
  x$minhashes <- value
  x
}

#' @param x An R object to check.
#' @export
#' @rdname TextReuseTextDocument
is.TextReuseTextDocument <- function(x) {
  inherits(x, "TextReuseTextDocument")
}

#' @export
#' @rdname TextReuseTextDocument
has_content <- function(x) {
  assert_that(is.TextReuseTextDocument(x))
  !is.null(x$content)
}

assertthat::on_failure(has_content) <- function(call, env) {
  paste0("Document does not have text in its content field.")
}

#' @export
#' @rdname TextReuseTextDocument
has_tokens <- function(x) {
  assert_that(is.TextReuseTextDocument(x))
  !is.null(x$tokens)
}

assertthat::on_failure(has_tokens) <- function(call, env) {
  "Document does not have tokens."
}

#' @export
#' @rdname TextReuseTextDocument
has_hashes <- function(x) {
  assert_that(is.TextReuseTextDocument(x))
  !is.null(x$hashes)
}

assertthat::on_failure(has_hashes) <- function(call, env) {
  "Document does not have hashes."
}

#' @export
#' @rdname TextReuseTextDocument
has_minhashes <- function(x) {
  assert_that(is.TextReuseTextDocument(x))
  !is.null(x$minhashes)
}

assertthat::on_failure(has_minhashes) <- function(call, env) {
  "Document does not have a minhash signature."
}

has_minhashes_corpus <- function(x) {
  assert_that(is.TextReuseCorpus(x))
  all(vapply(minhashes(x), Negate(is.null), logical(1)))
}

assertthat::on_failure(has_minhashes_corpus) <- function(call, env) {
  "Some documents in the corpus do not have a minhash signature."
}

