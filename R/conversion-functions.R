#' Convert candidates data frames to other formats
#'
#' These functions convert a \code{textreuse_candidates} object to dense or
#' sparse matrices.
#'
#' @param x An object of class \code{\link[=lsh_compare]{textreuse_candidates}}.
#' @param ... Additional arguments.
#'
#' @return A similarity matrix with row and column names containing document IDs.
#'
#' @export
#' @method as.matrix textreuse_candidates
as.matrix.textreuse_candidates <- function(x, ...) {

  docs <- candidate_doc_ids(x)
  n <- length(docs)
  m <- matrix(0, n, n)
  rownames(m) <- docs
  colnames(m) <- docs
  diag(m) <- 1.0

  for (r in seq_len(nrow(x))) {
    a <- x$a[r]
    b <- x$b[r]
    score <- x$score[r]
    m[a, b] <- score
    m[b, a] <- score
  }

  m

}

#' @rdname as.matrix.textreuse_candidates
#' @export
as_sparse_matrix <- function(x) {
  assert_that(is_candidates_df(x))

  docs <- candidate_doc_ids(x)
  n <- length(docs)
  doc_ids <- stats::setNames(seq_along(docs), docs)

  rows <- seq_len(n)
  cols <- seq_len(n)
  values <- rep(1.0, n)

  if (nrow(x) > 0) {
    a <- unname(doc_ids[x$a])
    b <- unname(doc_ids[x$b])
    rows <- c(rows, a, b)
    cols <- c(cols, b, a)
    values <- c(values, x$score, x$score)
  }

  Matrix::sparseMatrix(i = rows, j = cols, x = values, dims = c(n, n),
                       use.last.ij = TRUE,
                       dimnames = list(docs, docs))
}

candidate_doc_ids <- function(x) {
  all_doc_ids <- attr(x, "all-doc-ids")
  if (is.null(all_doc_ids)) {
    all_doc_ids <- c(x$a, x$b)
  }
  sort(unique(all_doc_ids))
}
