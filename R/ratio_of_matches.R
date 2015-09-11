#' Ratio of matches in two vectors or documents
#'
#' This function takes two vectors or documents, \code{origin} and
#' \code{destination}. It finds the ratio between the number of items in
#' \code{destination} that are also in \code{origin} and the total number of
#' items in \code{destination}. Note that this similarity measure is
#' directional: it measures how much \code{destination} borrows from
#' \code{origin}, but says nothing about how much of \code{origin} borrows from
#' destination. If this function is passed two documents of class
#' \code{\link{TextReuseTextDocument}}, it will compute the similarity using the
#' n-grams in those documents.
#'
#' @param origin The vector which is expected to be the origin.
#' @param destination The vector which is expected to be the destination
#'
#' @examples
#' ratio_of_matches(1:3, 2:4)
#'
#' ny         <- system.file("extdata/ny1850-match.txt", package = "textreuse")
#' ca_match   <- system.file("extdata/ca1851-match.txt", package = "textreuse")
#' ca_nomatch <- system.file("extdata/ca1851-nomatch.txt", package = "textreuse")
#'
#' ny         <- TextReuseTextDocument(ny)
#' ca_match   <- TextReuseTextDocument(ca_match)
#' ca_nomatch <- TextReuseTextDocument(ca_nomatch)
#'
#' # These two should have a higher ratio of matches
#' ratio_of_matches(ny, ca_match)
#'
#' # These two should have a very low ratio of matches
#' ratio_of_matches(ny, ca_nomatch)
#'
#' @export
ratio_of_matches <- function(origin, destination) UseMethod("ratio_of_matches")

#' @export
ratio_of_matches.default <- function(origin, destination) {
  assert_that(all(class(origin) == class(destination)))
  common  <- intersect(origin, destination)
  matches <- Filter(function(x) { x %in% common }, destination)
  length(matches) / length(destination)
}

#' @export
ratio_of_matches.TextReuseTextDocument <- function(origin, destination) {
  assert_that(all(class(origin) == class(destination)))
  ratio_of_matches(origin$hashes, destination$hashes)
}
