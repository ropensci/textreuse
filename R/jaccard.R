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
#' @param a The first set to be compared.
#' @param b The second set to be compared.
#'
#' @examples
#' jaccard_similarity(1:3, 2:4)
#' jaccard_dissimilarity(1:3, 2:4)
#'
#' ny         <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' ca_match   <- system.file("extdata/ca1851-match.txt", package = "textreuse")
#' ca_nomatch <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
#'
#' ny         <- TextReuseTextDocument(ny)
#' ca_match   <- TextReuseTextDocument(ca_match)
#' ca_nomatch <- TextReuseTextDocument(ca_nomatch)
#'
#' # These two should have a higher Jaccard similarity coefficient
#' jaccard_similarity(ny, ca_match)
#'
#' # These two should have a lower Jaccard similarity coefficient
#' jaccard_similarity(ny, ca_nomatch)
#'
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
