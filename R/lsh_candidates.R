#' Candidate pairs from LSH comparisons
#'
#' Given a cache object from an LSH comparison, this function returns the
#' potential candidates.
#' @param buckets A cache object returned from \code{\link{lsh}}.
#' @return A list containing character vectors with the IDs of potential matches.
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 234)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           hash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' lsh_candidates(buckets)
#' @export
lsh_candidates <- function(buckets) {
  assert_that(hash::is.hash(buckets))
  v <- hash::values(buckets)
  matches <- v[which(vapply(v, length, integer(1), USE.NAMES = FALSE) > 1)]
  matches <- unique(matches)

  # break clusters into pairs
  new_pairs <- list()
  deletable <- vector(mode = "integer")
  for (i in seq_along(matches)) {
    cluster <- matches[[i]]
    if (length(cluster) > 2) {
      combinations <- combn(cluster, m = 2, simplify = FALSE)
      new_pairs <- c(new_pairs, combinations)
      deletable <- c(deletable, i)
    }
  }
  if (length(new_pairs) > 0) {
    matches <- c(matches, new_pairs)
    matches[deletable] <- NULL
  }

  l <- length(matches)
  a <- vector("character", l)
  b <- vector("character", l)
  for (i in seq_along(matches)) {
    a[i] <- matches[[i]][1]
    b[i] <- matches[[i]][2]
  }
  df <- data.frame(a = a, b = b, score = NA_real_, stringsAsFactors = FALSE)
  df <- unique(sort_df_by_columns(sort_df_by_rows(df)))
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}
