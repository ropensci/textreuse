#' Candidate pairs from LSH comparisons
#'
#' Given a cache object from an LSH comparison, this function returns the
#' potential candidates.
#' @param buckets A cache object returned from \code{\link{lsh}}.
#' @param pairs If \code{TRUE}, pairs of candidates will be returned. Otherwise,
#'   clusters of candidates will be returned.
#' @return A list containing character vectors with the IDs of potential matches.
#' @examples
#' dir <- system.file("extdata", package = "textreuse")
#' set.seed(234)
#' minhash2 <- minhash_generator(200)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           hash_func = minhash2)
#' names(corpus) <- filenames(names(corpus))
#' buckets <- lsh(corpus, bands = 50)
#' lsh_candidates(buckets)
#' @export
lsh_candidates <- function(buckets, pairs = TRUE) {
  assert_that(hash::is.hash(buckets))
  v <- hash::values(buckets)
  matches <- v[which(vapply(v, length, integer(1), USE.NAMES = FALSE) > 1)]
  matches <- unique(matches)

  if (!pairs) return(matches) # return clusters

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
  unique(matches) # return pairs
}
