#' Measure similarity/dissimilarity in documents
#'
#' A set of functions which take two sets or bag of words and measure their
#' similarity or dissimilarity.
#'
#' @details The functions \code{jaccard_similarity} and
#'   \code{jaccard_dissimilarity} provide the Jaccard measures of similarity or
#'   dissimilarity for two sets. The coefficients will be numbers between
#'   \code{0} and \code{1}. For the similarity coefficient, the higher the
#'   number the more similar the two sets are. When applied to two documents of
#'   class \code{\link{TextReuseTextDocument}}, the hashes in those documents
#'   are compared. But this function can be passed objects of any class accepted
#'   by the set functions in base R. So it is possible, for instance, to pass
#'   this function two character vectors comprised of word, line, sentence, or
#'   paragraph tokens, or those character vectors hashed as integers.
#'
#'   The Jaccard similarity coeffecient is defined as follows:
#'
#'   \deqn{J(A, B) = \frac{ | A \cap B | }{ | A \cup B | }}{ length(intersect(a,
#'   b)) / length(union(a, b))}
#'
#'   The Jaccard dissimilarity is simply
#'
#'   \deqn{1 - J(A, B)}
#'
#'   The function \code{jaccard_bag_similarity} treats \code{a} and \code{b} as
#'   bags rather than sets, so that the result is a fraction where the numerator
#'   is the sum of each matching element counted the minimum number of times it
#'   appears in each bag, and the denominator is the sum of the lengths of both
#'   bags. The maximum value for the Jaccard bag similarity is \code{0.5}.
#'
#'   The function \code{ratio_of_matches} finds the ratio between the number of
#'   items in \code{b} that are also in \code{a} and the total number of items
#'   in \code{b}. Note that this similarity measure is directional: it measures
#'   how much \code{b} borrows from \code{a}, but says nothing about how much of
#'   \code{a} borrows from \code{b}.
#'
#' @param a The first set (or bag) to be compared. The origin bag for
#'   directional comparisons.
#' @param b The second set (or bag) to be compared. The destination bag for
#'   directional comparisons.
#'
#' @examples
#' jaccard_similarity(1:6, 3:10)
#' jaccard_dissimilarity(1:6, 3:10)
#'
#' a <- c("a", "a", "a", "b")
#' b <- c("a", "a", "b", "b", "c")
#' jaccard_similarity(a, b)
#' jaccard_bag_similarity(a, b)
#' ratio_of_matches(a, b)
#' ratio_of_matches(b, a)
#'
#' ny         <- system.file("extdata/legal/ny1850-match.txt", package = "textreuse")
#' ca_match   <- system.file("extdata/legal/ca1851-match.txt", package = "textreuse")
#' ca_nomatch <- system.file("extdata/legal/ca1851-nomatch.txt", package = "textreuse")
#'
#' ny         <- TextReuseTextDocument(file = ny,
#'                                     meta = list(id = "ny"))
#' ca_match   <- TextReuseTextDocument(file = ca_match,
#'                                     meta = list(id = "ca_match"))
#' ca_nomatch <- TextReuseTextDocument(file = ca_nomatch,
#'                                     meta = list(id = "ca_nomatch"))
#'
#' # These two should have higher similarity scores
#' jaccard_similarity(ny, ca_match)
#' ratio_of_matches(ny, ca_match)
#'
#' # These two should have lower similarity scores
#' jaccard_similarity(ny, ca_nomatch)
#' ratio_of_matches(ny, ca_nomatch)
#'
#' @references Jure Leskovec, Anand Rajaraman, and Jeff Ullman,
#'   \href{http://www.mmds.org/#book}{\emph{Mining of Massive Datasets}}
#'   (Cambridge University Press, 2011).
#' @name similarity-functions
NULL

#' @rdname similarity-functions
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

#' @rdname similarity-functions
#' @export
jaccard_dissimilarity <- function(a, b) UseMethod("jaccard_dissimilarity")

#' @export
jaccard_dissimilarity.default <- function(a, b) {
  1 - jaccard_similarity(a, b)
}

#' @rdname similarity-functions
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

#' @export
#' @rdname similarity-functions
ratio_of_matches <- function(a, b) UseMethod("ratio_of_matches")

#' @export
ratio_of_matches.default <- function(a, b) {
  assert_that(all(class(a) == class(b)))
  sum(b %in% a) / length(b)
}

#' @export
ratio_of_matches.TextReuseTextDocument <- function(a, b) {
  assert_that(all(class(a) == class(b)))
  ratio_of_matches(a$hashes, b$hashes)
}
