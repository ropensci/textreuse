#' Probability that a candidate pair will be detected with LSH
#'
#' Functions to help choose the correct parameters for the \code{\link{lsh}} and
#' \code{\link{minhash_generator}} functions. Use \code{lsh_threshold} to
#' determine the minimum Jaccard similarity for two documents for them to likely
#' be considered a match. Use \code{lsh_probability} to determine the
#' probability that a pair of documents with a known Jaccard similarity will be
#' detected.
#'
#' @param h The number of minhash signatures.
#' @param b The number of LSH bands.
#' @param s The Jaccard similarity.
#' @details Locality sensitive hashing returns a list of possible matches for
#' similar documents. How likely is it that a pair of documents will be detected
#' as a possible match? If \code{h} is the number of minhash signatures,
#' \code{b} is the number of bands in the LSH function (implying then that the
#' number of rows \code{r = h / b}), and \code{s} is the actual Jaccard
#' similarity of the two documents, then the probability \code{p} that the two
#' documents will be marked as a candidate pair is given by this equation.
#'
#' \deqn{p = 1 - (1 - s^{r})^{b}}
#'
#' According to \href{http://infolab.stanford.edu/~ullman/mmds/book.pdf}{MMDS},
#' that equation approxmiates an S-curve. This implies that there is a threshold
#' (\code{t}) for \code{s} approximated by this equation.
#'
#' \deqn{t = \frac{1}{b}^{\frac{1}{r}}}
#'
#' @references Jure Leskovec, Anand Rajaraman, and Jeff Ullman,
#'  \href{http://www.mmds.org/#book}{\emph{Mining of Massive Datasets}}
#'  (Cambridge University Press, 2011), ch. 3.
#' @examples
#' # Threshold for default values
#' lsh_threshold(h = 200, b = 40)
#'
#' # Probability for varying values of s
#' lsh_probability(h = 200, b = 40, s = .25)
#' lsh_probability(h = 200, b = 40, s = .50)
#' lsh_probability(h = 200, b = 40, s = .75)
#' @export
lsh_probability <- function(h, b, s) {
  assert_that(is.count(h),
              is.count(b),
              check_banding(h, b),
              is.number(s))
  1 - (1 - s ^ (h / b)) ^ b
}

#' @rdname lsh_probability
#' @export
lsh_threshold <- function(h, b) {
  assert_that(is.count(h),
              is.count(b),
              check_banding(h, b))
  (1 / b ) ^ (1 / (h / b))
}
