#' Compare candidates identified by LSH
#'
#' The \code{\link{lsh_candidates}} only identifies potential matches, but
#' cannot estimate the actual similarity of the documents. This function takes a
#' data frame returned by \code{\link{lsh_candidates}} and applies a comparison
#' function to each of the documents in a corpus, thereby calculating the
#' document similarity score. Note that since your corpus will have minhash
#' signatures rather than hashes for the tokens itself, you will probably wish
#' to use \code{\link{tokenize}} to calculate new hashes. This can be done for
#' just the potentially similar documents. See the package vignettes for
#' details.
#'
#' @param candidates A data frame returned by \code{\link{lsh_candidates}}.
#' @param corpus The same \code{\link{TextReuseCorpus}} corpus which was used to generate the candidates.
#' @param f A comparison function such as \code{\link{jaccard_similarity}}.
#' @param progress Display a progress bar while comparing documents. Progress
#'   bars are disabled when using parallel processing.
#' @return A data frame with values calculated for \code{score}.
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 234)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           minhash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' candidates <- lsh_candidates(buckets)
#' lsh_compare(candidates, corpus, jaccard_similarity)
#' @export
lsh_compare <- function(candidates, corpus, f, progress = interactive()) {
  assert_that(is_candidates_df(candidates),
              is.function(f),
              is.TextReuseCorpus(corpus))

  rows_to_score <- which(is.na(candidates$score))
  num_rows <- length(rows_to_score)
  use_parallel <- using_parallel()

  if (num_rows == 0) {
    attr(candidates, "all-doc-ids") <- names(corpus)
    return(candidates)
  }

  if (progress) {
    message("Making ", prettyNum(num_rows, big.mark = ","),
            " comparisons.")
    if (!use_parallel) {
      pb <- txtProgressBar(min = 0, max = num_rows, style = 3)
    }
  }

  apply_fun <- get_apply_function()
  scores <- apply_fun(seq_along(rows_to_score), function(j) {
    i <- rows_to_score[j]
    a <- candidates$a[i]
    b <- candidates$b[i]
    score <- f(corpus[[a]], corpus[[b]])
    if (progress && !use_parallel) setTxtProgressBar(pb, j)
    score
  })

  candidates$score[rows_to_score] <- unlist(scores, use.names = FALSE)

  if (progress && !use_parallel) close(pb)

  attr(candidates, "all-doc-ids") <- names(corpus)

  candidates
}
