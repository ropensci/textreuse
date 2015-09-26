#' TextReuseCorpus
#'
#' This is the contructor function for a \code{TextReuseCorpus}, modeled on the
#' virtual S3 class \code{\link[tm]{Corpus}} from the \code{tm} package. The
#' object is a \code{TextReuseCorpus}, which is basically a list containing
#' objects of class \code{\link{TextReuseTextDocument}}. Arguments are passed
#' along to that constructor function. You can pass either a character vector of
#' paths to text files using the \code{paths =} parameter, or a directory
#' containing text files using the \code{dir =} parameter.
#'
#' @param paths A character vector of paths to files to be opened.
#' @param dir A The path to a directory of text files.
#' @param meta A list with named elements for the metadata associated with this
#'   corpus.
#' @param progress Display a progress bar while loading files.
#' @param tokenizer A function to split the text into tokens. See
#'   \code{\link{tokenizers}}. If value is \code{NULL}, then tokenizing and
#'   hashing will be skipped.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param keep_tokens Should the tokens be saved in the documents that are
#'   returned or discarded?
#' @param keep_text Should the text be saved in the documents that are returned
#'   or discarded?
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' corpus <- TextReuseCorpus(dir = dir, meta = list("description" = "Field Codes"))
#' # Subset by position or file name
#' corpus[[1]]
#' names(corpus)
#' corpus[["ca1851-match"]]
#' @export
TextReuseCorpus <- function(paths, dir = NULL, meta = list(),
                            progress = interactive(),
                            tokenizer = tokenize_ngrams, ...,
                            hash_func = hash_string,
                            keep_tokens = FALSE, keep_text = TRUE) {

  if (missing(paths) & !is.null(dir)) {
    assert_that(is.dir(dir))
    paths <- Sys.glob(str_c(dir, "/*"))
  }

  vapply(paths, is.readable, logical(1), USE.NAMES = FALSE)

  if (!is.null(tokenizer)) {
    assert_that(is.function(tokenizer),
                is.function(hash_func))
    tokenizer_name <- as.character(substitute(tokenizer))
    hash_func_name <- as.character(substitute(hash_func))
    loading_msg <- "Loading, tokenizing, and hashing "
  } else {
    tokenizer_name <- NULL
    hash_func_name <- NULL
    loading_msg <- "Loading "
  }

  if (progress) {
    len <- length(paths)
    message(loading_msg, prettyNum(len, big.mark = ","), " documents.")
    pb <- txtProgressBar(min = 0, max = len, style = 3)
  }
  docs <- lapply(seq_along(paths), function(i) {
    d <- TextReuseTextDocument(file = paths[i], tokenizer = tokenizer, ...,
                               hash_func = hash_func, keep_tokens = keep_tokens,
                               keep_text = keep_text,
                               meta = list(tokenizer = tokenizer_name,
                                           hash_func = hash_func_name))
    if (progress) setTxtProgressBar(pb, i)
    d
  })

  if (progress) close(pb)

  names(docs) <- filenames(paths)

  assert_that(is.list(meta))
  meta$tokenizer <- tokenizer_name
  meta$hash_func <- hash_func_name

  if (!is.null(names(meta))) meta <- sort_meta(meta)

  corpus <- list(documents = docs, meta = meta)
  class(corpus) <- c("TextReuseCorpus", "Corpus")

  corpus

}

#' @export
meta.TextReuseCorpus <- function(x, tag = NULL, ...) {
  if (is.null(tag))
    x$meta
  else
    x$meta[[tag]]
}

#' @export
`meta<-.TextReuseCorpus` <- function(x, tag = NULL, ..., value) {
  if (is.null(tag)) {
    assert_that(is.list(value))
    x$meta <- value
  } else {
    x$meta[[tag]] <- value
  }
  x
}

#' @export
print.TextReuseCorpus <- function(x, ...) {
  cat("TextReuseCorpus\n")
  cat("Number of documents:", length(x), "\n")
  pretty_print_metadata(x)
}

#' @export
length.TextReuseCorpus <- function(x) {
  length(x$documents)
}

#' @export
`[.TextReuseCorpus` <- function(x, i) {
  x$documents <- x$documents[i]
  x
}

#' @export
`[[.TextReuseCorpus` <- function(x, i) {
  x$documents[[i]]
}

#' @export
names.TextReuseCorpus <- function(x) {
  names(x$documents)
}

#' @export
`names<-.TextReuseCorpus` <- function(x, value) {
  names(x$documents) <- value
  x
}

#' @param x An R object to check.
#' @export
#' @rdname TextReuseCorpus
is.TextReuseCorpus <- function(x) {
  inherits(x, "TextReuseCorpus")
}
