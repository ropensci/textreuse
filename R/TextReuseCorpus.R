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
#'   \code{\link{tokenizers}}.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param keep_tokens Should the tokens be saved in the documents that are
#'   returned or discarded?
#' @param keep_text Should the text be saved in the documents that are returned or
#'   discarded?
#' @examples
#' ny <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' ca1 <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
#' ca2 <- system.file("extdata/ca1851-match.txt", package = "textreuse")
#' paths <- c(ny, ca1, ca2)
#' corpus <- TextReuseCorpus(paths, meta = list("description" = "Field Codes"))
#' meta(corpus)
#' # Subset by position or file name
#' corpus[[1]]
#' corpus[[paths[3]]]
#' @export
TextReuseCorpus <- function(paths, dir = NULL, meta = list(),
                            progress = interactive(),
                            tokenizer = tokenize_ngrams, ...,
                            hash_func = hash_string,
                            keep_tokens = TRUE, keep_text = TRUE) {

  if (missing(paths) & !is.null(dir)) {
    assert_that(is.dir(dir))
    paths <- Sys.glob(str_c(dir, "/*"))
  }

  vapply(paths, is.readable, logical(1), USE.NAMES = FALSE)

  if (progress) pb <- txtProgressBar(min = 0, max = length(paths), style = 3)
  docs <- lapply(seq_along(paths), function(i) {
    d <- TextReuseTextDocument(file = paths[i], tokenizer = tokenizer, ...,
                               hash_func = hash_func, keep_tokens = keep_tokens,
                               keep_text = keep_text)
    if (progress) setTxtProgressBar(pb, i)
    d
  })

  if (progress) close(pb)

  names(docs) <- paths

  assert_that(is.list(meta))

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
  x$documents[i]
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
