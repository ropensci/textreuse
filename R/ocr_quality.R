#' Measure the quality of an text created with OCR
#'
#' This function checks the quality of an OCR text. It will return a number
#' between \code{0} and \code{1}. The higher the number, the better the quality
#' of the OCR. These measures should not be taken in an absolute sense. That is,
#' a score of 1 does not indicate perfect OCR. They should only be used to
#' determine the relative quality of OCR within a corpus of texts. See the
#' description of methods used to calculate OCR quality below. You can pass a
#' character vector of any length. So, if you split a text into chunks, you can
#' evaluate the OCR quality of each chunk.
#'
#' @param text A character vector. Each text (or chunk of a text) should be a
#'   value within the vector.
#' @param method The method used. \describe{ \item{\code{dictionary}}{Checks
#'   whether the words used in the text are found in a list of English words. It
#'   cannot identify OCR errors that result in the wrong English word. (See
#'   \code{\link{words_en}}.) Returns the ratio of words found in the list to
#'   the total number of words in the text.}}
#' @return A vector of numeric values between \code{0} and \code{1}.
#' @examples
#' paragraph <- "Fourr score and sleven years ago our fathers brought
#'   forth on this continent, a new nation, conceived in Liberty,
#'   and dedicated to tlhe proposition that all men are created equal."
#'
#' ocr_quality(paragraph)
#'
#' @export
ocr_quality <- function(text, method = c("dictionary")) {
  assert_that(is.character(text))
  method <- match.arg(method)
  switch(method,
         dictionary = ocr_dictionary(text))
}
