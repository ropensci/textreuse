#' List of all candidates in a corpus
#'
#' @param candidates A data frame of candidate pairs from
#'   \code{\link{lsh_candidates}}.
#' @return A character vector of document IDs from the candidate pairs, to be
#'   used to subset the \code{\link{TextReuseCorpus}}.
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 234)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           minhash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' candidates <- lsh_candidates(buckets)
#' lsh_subset(candidates)
#' corpus[lsh_subset(candidates)]
#' @export
lsh_subset <- function(candidates) {
  assert_that(is_candidates_df(candidates))
  sort(unique(c(candidates$a, candidates$b)))
}
