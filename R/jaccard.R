#' Calculate Jaccard similarity/dissimilarity coefficients
#'
#' These functions compute the Jaccard measures of similarity or dissimilarity
#' for two sets. The coefficients will be numbers between \code{0} and \code{1}.
#' For the similarity coefficient, the higher the number the more similar the
#' two sets are. When applied to two documents of class
#' \code{\link{TextReuseTextDocument}}, the n-grams in those documents are
#' compared. But this function can be passed objects of any class accepted by
#' the set functions in base R. So it is possible, for instance, to pass this
#' function two character vectors comprised of word, line, sentence, or
#' paragraph tokens, or those character vectors hashed as integers.
#'
#' The Jaccard similarity coeffecient is defined as follows:
#'
#' \deqn{J(A, B) = \frac{ | A \cup B | }{ | A \cap B | }}{ length(intersect(a,
#' b)) / length(union(a, b))}
#'
#' The dissimilarity is simply
#'
#' \deqn{1 - J(A, B)}
#'
#' The Jaccard bag similarity treats \code{a} and \code{b} as bags rather than
#' sets, so that the result is a fraction where the numerator is the sum of each
#' matching element counted the minimum number of times it appears in each bag,
#' and the denominator is the sum of the lengths of both bags. The maximum value
#' for the Jaccard bag similarity is \code{0.5}.
#'
#' @param a The first set (or bag) to be compared.
#' @param b The second set (or bag) to be compared.
#'
#' @examples
#' jaccard_similarity(1:3, 2:4)
#' jaccard_dissimilarity(1:3, 2:4)
#'
#' ny         <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' ca_match   <- system.file("extdata/ca1851-match.txt", package = "textreuse")
#' ca_nomatch <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
#'
#' ny         <- TextReuseTextDocument(ny, meta = list(id = "ny"))
#' ca_match   <- TextReuseTextDocument(ca_match, meta = list(id = "ca_match"))
#' ca_nomatch <- TextReuseTextDocument(ca_nomatch, meta = list(id = "ca_nomatch"))
#'
#' # These two should have a higher Jaccard similarity coefficient
#' jaccard_similarity(ny, ca_match)
#'
#' # These two should have a lower Jaccard similarity coefficient
#' jaccard_similarity(ny, ca_nomatch)
#'
#' a <- c("a", "a", "a", "b")
#' b <- c("a", "a", "b", "b", "c")
#' jaccard_similarity(a, b)
#' jaccard_bag_similarity(a, b)
#' @references Jure Leskovec, Anand Rajaraman, Jeff Ullman,
#'   \href{http://www.mmds.org/#book}{\emph{Mining of Massive Datasets}}
#'   (Cambridge University Press, 2011).
#' @name jaccard
NULL

#' @rdname jaccard
#' @export
jaccard_similarity <- function(a, b) UseMethod("jaccard_similarity")

#' @export
jaccard_similarity.default <- function(a, b) {
  assert_that(all(class(a) == class(b)))
  length(intersect(a, b)) / length(union(a, b))
}

#' @export
jaccard_similarity.TextReuseTextDocument <- function(a, b) {
  assert_that(all(class(a) == class(b)))
  jaccard_similarity(a$hashes, b$hashes)
}

#' @rdname jaccard
#' @export
jaccard_dissimilarity <- function(a, b) UseMethod("jaccard_dissimilarity")

#' @export
jaccard_dissimilarity.default <- function(a, b) {
  1 - jaccard_similarity(a, b)
}

#' @rdname jaccard
#' @export
jaccard_bag_similarity <- function(a, b) UseMethod("jaccard_bag_similarity")

#' @export
jaccard_bag_similarity.default <- function(a, b) {
  matches <- intersect(a, b)
  counts <- vapply(matches, function(x) min(sum(x == a), sum(x == b)),
                   integer(1), USE.NAMES = FALSE)
  denominator <- length(a) + length(b)
  sum(counts) / denominator
}

#' @export
jaccard_bag_similarity.TextReuseTextDocument <- function(a, b) {
  assert_that(all(class(a) == class(b)))
  jaccard_bag_similarity(a$hashes, b$hashes)
}
