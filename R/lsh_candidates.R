#' Candidate pairs from LSH comparisons
#'
#' Given a data frame of LSH buckets returned from \code{\link{lsh}}, this
#' function returns the potential candidates.
#'
#' @param buckets A data frame returned from \code{\link{lsh}}.
#'
#' @return A data frame of candidate pairs.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 234)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           minhash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' lsh_candidates(buckets)
#'
#' @export
lsh_candidates <- function(buckets) {
  assert_that(is_lsh_buckets(buckets))
  x <- y <- NULL

  # The inequality join creates each unordered pair only once, avoiding the
  # duplicate orientations and self-pairs produced by a full bucket self-join.
  candidates <- buckets %>%
    dplyr::inner_join(
      buckets,
      by = dplyr::join_by(buckets, x$doc < y$doc)
    ) %>%
    dplyr::distinct(.data$doc.x, .data$doc.y) %>%
    dplyr::transmute(a = .data$doc.x, b = .data$doc.y) %>%
    dplyr::arrange(.data$a, .data$b) %>%
    dplyr::mutate(score = NA_real_)

  class(candidates) <- c("textreuse_candidates", class(candidates))

  candidates

}
