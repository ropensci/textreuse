#' Query a LSH cache for matches to a single document
#'
#' This function retrieves the matches for a single document from an \code{lsh_buckets} object created by \code{\link{lsh}}. See \code{\link{lsh_candidates}} to rerieve all pairs of matches.
#'
#' @param buckets An \code{lsh_buckets} object created by \code{\link{lsh}}.
#' @param id The document ID to find matches for.
#'
#' @return An \code{lsh_candidates} data frame with matches to the document specified.
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 235)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           minhash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' lsh_query(buckets, "ny1850-match")
#'
#' @seealso \code{\link{lsh}}, \code{\link{lsh_candidates}}
#' @export
lsh_query <- function(buckets, id) {
  assert_that(is_lsh_buckets(buckets),
              is.string(id))

  signatures <- buckets %>%
    dplyr::filter_(~doc == id) %>%
    `$`("buckets")

  docs <- buckets %>%
    dplyr::filter_(~buckets %in% signatures) %>%
    `$`("doc")

  res <- dplyr::data_frame(a = id, b = docs, score = NA_real_) %>%
    dplyr::filter_(~a != b) %>%
    dplyr::distinct_(~a, ~b)

  class(res) <- c("textreuse_candidates", class(res))

  res
}
