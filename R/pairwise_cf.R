#' Compare documents in a corpus to one another pairwise
#'
#' Given two lists containing documents of class
#' \code{\link{TextReuseTextDocument}}, this function applies a comparison
#' function to every pairing of documents, and returns a matrix with the
#' comparison scores.
#'
#' @param x A list of documents to be compared.
#' @param y A list of documents to be compared. Often \code{y} will be the same
#'   as \code{x}.
#' @param f The function to apply to \code{x} and \code{y}.
#' @param ... Additional arguments passed to \code{f}.
#' @param id The name of the value in the \code{meta} list of the
#'   \code{\link{TextReuseTextDocument}} to be used in row and column names of
#'   the matrix. Set to \code{NULL} to remove row and column names.
#'
#' @seealso See these document comparison functions, \code{\link{jaccard_similarity}},
#'   \code{\link{ratio_of_matches}}.
#'
#' @examples
#' ny         <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' ca_match   <- system.file("extdata/ca1851-match.txt", package = "textreuse")
#' ca_nomatch <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
#'
#' ny         <- TextReuseTextDocument(ny, meta = list(id = "ny"))
#' ca_match   <- TextReuseTextDocument(ca_match, meta = list(id = "ca_match"))
#' ca_nomatch <- TextReuseTextDocument(ca_nomatch, meta = list(id = "ca_no"))
#'
#' corpus <- list(ny, ca_match, ca_nomatch)
#'
#' pairwise_cf(corpus, corpus, jaccard_similarity, id = "id")
#' pairwise_cf(corpus, corpus, ratio_of_matches, id = "id")
#'
#' @export
pairwise_cf <- function(x, y, f, ..., id = "file") {
  assert_that(is.list(x))
  assert_that(is.list(y))

  # http://stackoverflow.com/questions/1719447/
  m <- outer(x, y, function(x, y) {
   vapply(seq_along(x), function(i) { f(x[[i]], y[[i]], ...) }, numeric(1))
  })

  if(!is.null(id)) {
    rows <- vapply(x, meta, character(1), id, USE.NAMES = FALSE)
    cols <- vapply(y, meta, character(1), id, USE.NAMES = FALSE)
    rownames(m) <- rows
    colnames(m) <- cols
  }

  m
}
