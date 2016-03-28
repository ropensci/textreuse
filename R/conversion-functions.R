#' Convert candidates data frames to other formats
#'
#' These S3 methods convert a \code{textreuse_candidates} object to a matrix.
#'
#' @param x An object of class \code{\link[=lsh_compare]{textreuse_candidates}}.
#' @param ... Additional arguments.
#'
#' @return A similarity matrix with row and column names containing document IDs.
#'
#' @export
#' @method as.matrix textreuse_candidates
as.matrix.textreuse_candidates <- function(x, ...) {

  docs <- sort(unique(c(x$a, x$b)))
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
