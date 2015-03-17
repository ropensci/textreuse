#' Measure the quality of an text created with OCR
#'
#' This function checks the quality of an OCR text. It will return a number
#' between \code{0} and \code{1}. The higher the number, the better the quality
#' of the OCR. These measures should not be taken in an absolute sense. That is,
#' a score of 1 does not indicate perfect OCR. They should only be used to
#' determine the relative quality of OCR within a corpus of texts. See the
#' description of methods used to calculate OCR quality below.
#'
#' @param text A character vector of length one
#' @param method The method used. \describe{ \item{\code{dictionary}}{Checks
#'   whether the words used in the text are found in a list of English words. It
#'   cannot identify OCR errors that result in the wrong English word. (See
#'   \code{\link{words_en}}.) Returns the ratio of words found in the list to
#'   the total number of words in the text.}}
#' @return A numeric value between \code{0} and \code{1}.
#' @examples
#' ocr_quality("Thle is a particularly harrd word for OCR engines.")
#'
#' @export
ocr_quality <- function(text, method = c("dictionary")) {
  assert_that(is.string(text))
  method <- match.arg(method)
  switch(method,
         dictionary = ocr_dictionary(text))
}
