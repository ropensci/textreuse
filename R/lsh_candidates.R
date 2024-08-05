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

  candidates <- buckets %>%
    dplyr::left_join(buckets, by = "buckets") %>%
    dplyr::filter(.data$doc.x != .data$doc.y) %>%
    dplyr::distinct(doc.x, doc.y) %>%
    dplyr::arrange(.data$doc.x, .data$doc.y) %>%
    dplyr::mutate(dn = pmin(.data$doc.x, .data$doc.y),
                  up = pmax(.data$doc.x, .data$doc.y)) %>%
    dplyr::distinct(.data$up, .data$dn) %>%
    dplyr::select(a = dn, b = up) %>%
    dplyr::arrange(.data$a, .data$b) %>%
    dplyr::mutate(score = NA_real_)

  class(candidates) <- c("textreuse_candidates", class(candidates))

  candidates

}
