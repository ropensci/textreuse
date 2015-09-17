#' Pairwise comparisons among documents in a corpus
#'
#' Given a \code{\link{TextReuseCorpus}} containing documents of class
#' \code{\link{TextReuseTextDocument}}, this function applies a comparison
#' function to every pairing of documents, and returns a matrix with the
#' comparison scores.
#'
#' @param corpus A \code{\link{TextReuseCorpus}}.
#' @param f The function to apply to \code{x} and \code{y}.
#' @param ... Additional arguments passed to \code{f}.
#' @param directional Some comparison functions are commutative, so that
#'   \code{f(a, b) == f(b, a)} (e.g., \code{\link{jaccard_similarity}}). Other
#'   functions are directional, so that \code{f(a, b)} measures \code{a}'s
#'   borrowing from \code{b}, which may not be the same as \code{f(b, a)} (e.g.,
#'   \code{\link{ratio_of_matches}}). If \code{directional} is \code{FALSE},
#'   then only the minimum number of comparisons will be made, i.e., the upper
#'   triangle of the matrix. If \code{directional} is \code{TRUE}, then both
#'   directional comparisons will be measured. In no case, however, will
#'   documents be compared to themselves, i.e., the diagonal of the matrix.
#' @param progress Display a progress bar while comparing documents.
#'
#' @return A square matrix with dimensions equal to the length of the corpus,
#'   and row and column names set by the names of the documents in the corpus. A
#'   value of \code{NA} in the matrix indicates that a comparison was not made.
#'   In cases of directional comparisons, then the comparison reported is
#'   \code{f(row, column)}.
#'
#' @seealso See these document comparison functions,
#'   \code{\link{jaccard_similarity}}, \code{\link{ratio_of_matches}}.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' corpus <- TextReuseCorpus(dir = dir)
#' names(corpus) <- filenames(names(corpus))
#'
#' # A non-directional comparison
#' pairwise_compare(corpus, jaccard_similarity)
#'
#' # A directional comparison
#' pairwise_compare(corpus, ratio_of_matches, directional = TRUE)
#' @export
pairwise_compare <- function(corpus, f, ..., directional = FALSE,
                        progress = interactive()) {
  assert_that(is.TextReuseCorpus(corpus),
              is.function(f))

  len <- length(corpus)
  ids <- names(corpus)

  m <- matrix(0, len, len, dimnames = list(ids, ids))

  if (!directional)
    m[lower.tri(m, diag = TRUE)] <- NA
  else
    diag(m) <- NA


  if (progress) {
    num_pairs <- sum(!is.na(m))
    message("Making ", prettyNum(num_pairs, big.mark = ","), " comparisons.")
    pb <- txtProgressBar(min = 0, max = num_pairs, style = 3)
  }

  for (i in seq_along(m)) {
    if (is.na(m[i])) next
    indexes <- arrayInd(i, dim(m))
    m[indexes] <- f(corpus[[indexes[1]]], corpus[[indexes[2]]])
    if (progress) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  }

  if (progress) close(pb)

  m

}
