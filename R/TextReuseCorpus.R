#' TextReuseCorpus
#'
#' This is the constructor function for a \code{TextReuseCorpus}, modeled on the
#' virtual S3 class \code{\link[tm]{Corpus}} from the \code{tm} package. The
#' object is a \code{TextReuseCorpus}, which is basically a list containing
#' objects of class \code{\link{TextReuseTextDocument}}. Arguments are passed
#' along to that constructor function. To create the corpus, you can pass either
#' a character vector of paths to text files using the \code{paths =} parameter,
#' a directory containing text files (with any extension) using the \code{dir =}
#' parameter, or a character vector of documents using the \code{text = }
#' parameter, where each element in the characer vector is a document. If the
#' character vector passed to \code{text = } has names, then those names will be
#' used as the document IDs. Otherwise, IDs will be assigned to the documents.
#' Only one of the \code{paths}, \code{dir}, or \code{text} parameters should be
#' specified.
#'
#' @details If \code{skip_short = TRUE}, this function will skip very short or
#'   empty documents. A very short document is one where there are two few words
#'   to create at least two n-grams. For example, if five-grams are desired,
#'   then a document must be at least six words long. If no value of \code{n} is
#'   provided, then the function assumes a value of \code{n = 3}. A warning will
#'   be printed with the document ID of each skipped document. Use
#'   \code{skipped()} to get the IDs of skipped documents.
#'
#'   This function will use multiple cores on non-Windows machines if the
#'   \code{"mc.cores"} option is set. For example, to use four cores:
#'   \code{options("mc.cores" = 4L)}.
#'
#' @param paths A character vector of paths to files to be opened.
#' @param dir The path to a directory of text files.
#' @param text A character vector (possibly named) of documents.
#' @param meta A list with named elements for the metadata associated with this
#'   corpus.
#' @param progress Display a progress bar while loading files.
#' @param tokenizer A function to split the text into tokens. See
#'   \code{\link{tokenizers}}. If value is \code{NULL}, then tokenizing and
#'   hashing will be skipped.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param minhash_func A function to create minhash signatures of the document.
#'   See \code{\link{minhash_generator}}.
#' @param keep_tokens Should the tokens be saved in the documents that are
#'   returned or discarded?
#' @param keep_text Should the text be saved in the documents that are returned
#'   or discarded?
#' @param skip_short Should short documents be skipped? (See details.)
#'
#' @seealso \link[=TextReuseTextDocument-accessors]{Accessors for TextReuse
#'   objects}.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' corpus <- TextReuseCorpus(dir = dir, meta = list("description" = "Field Codes"))
#' # Subset by position or file name
#' corpus[[1]]
#' names(corpus)
#' corpus[["ca1851-match"]]
#'
#' @export
TextReuseCorpus <- function(paths, dir = NULL, text = NULL, meta = list(),
                            progress = interactive(),
                            tokenizer = tokenize_ngrams, ...,
                            hash_func = hash_string,
                            minhash_func = NULL,
                            keep_tokens = FALSE,
                            keep_text = TRUE,
                            skip_short = TRUE) {

  if (!is.null(tokenizer)) {
    assert_that(is.function(tokenizer),
                is.function(hash_func))
    tokenizer_name <- as.character(substitute(tokenizer))
    hash_func_name <- as.character(substitute(hash_func))
    if (!is.null(minhash_func)) {
      minhash_func_name <- as.character(substitute(minhash_func))
    } else {
      minhash_func_name <- NULL
    }
    loading_msg <- "Loading, tokenizing, and hashing "
  } else {
    tokenizer_name <- NULL
    hash_func_name <- NULL
    minhash_func_name <- NULL
    loading_msg <- "Loading "
  }

  apply_func <- get_apply_function()

  # If we get a character vector of documents, use that; otherwise load
  # the files from disk.
  if (!missing(text)) {

    assert_that(missing(paths),
                is.null(dir),
                is.character(text))

    if (progress) {
      len <- length(text)
      message(loading_msg, prettyNum(len, big.mark = ","), " documents.")
      if (using_parallel())
        progress <- FALSE
      else
        pb <- txtProgressBar(min = 0, max = len, style = 3)
    }

    if (is.null(names(text)))
      names(text) <- str_c("doc-", 1:length(text))

    docs <- apply_func(seq_along(text), function(i) {
      d <- TextReuseTextDocument(text = text[i],
                                 tokenizer = tokenizer, ...,
                                 hash_func = hash_func,
                                 minhash_func = minhash_func,
                                 keep_tokens = keep_tokens,
                                 keep_text = keep_text,
                                 skip_short = skip_short,
                                 meta = list(id = names(text)[i],
                                             tokenizer = tokenizer_name,
                                             hash_func = hash_func_name,
                                             minhash_func = minhash_func_name))
      if (progress) setTxtProgressBar(pb, i)
      d
    })

    if (progress) close(pb)

    names(docs) <- names(text)

  } else {

    if (missing(paths) & !is.null(dir)) {
      assert_that(is.dir(dir))
      paths <- Sys.glob(str_c(dir, "/*"))
    }

    vapply(paths, is.readable, logical(1), USE.NAMES = FALSE)

    if (progress) {
      len <- length(paths)
      message(loading_msg, prettyNum(len, big.mark = ","), " documents.")
      if (using_parallel())
        progress <- FALSE
      else
        pb <- txtProgressBar(min = 0, max = len, style = 3)
    }
    docs <- apply_func(seq_along(paths), function(i) {
      d <- TextReuseTextDocument(file = paths[i], tokenizer = tokenizer, ...,
                                 hash_func = hash_func,
                                 minhash_func = minhash_func,
                                 keep_tokens = keep_tokens,
                                 keep_text = keep_text,
                                 skip_short = skip_short,
                                 meta = list(tokenizer = tokenizer_name,
                                             hash_func = hash_func_name,
                                             minhash_func = minhash_func_name))
      if (progress) setTxtProgressBar(pb, i)
      d
    })

    if (progress) close(pb)

    names(docs) <- filenames(paths)
  }

  # Filter documents that were skipped because they were too short
  if (skip_short) {
    skipped <- names(Filter(is.null, docs))
    docs <- Filter(Negate(is.null), docs)
    if (length(skipped) > 0)
      warning("Skipped ", length(skipped), " documents that were too short. ",
              "Use `skipped()` to get their IDs.")
  }

  assert_that(is.list(meta))
  meta$tokenizer <- tokenizer_name
  meta$hash_func <- hash_func_name
  meta$minhash_func <- minhash_func_name

  if (!is.null(names(meta))) meta <- sort_meta(meta)

  corpus <- list(documents = docs, meta = meta)
  class(corpus) <- c("TextReuseCorpus", "Corpus")
  attr(corpus, "skipped") <- skipped

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

#' @export
#' @rdname TextReuseCorpus
skipped <- function(x) {
  assert_that(is.TextReuseCorpus(x))
  attr(x, "skipped", exact = TRUE)
}
