#' Break a text into n-grams
#'
#' @param string A character vector to be split into n-grams.
#' @param n The value of \code{n}.
#' @return A character vector containing the n-grams.
#' @details
#' As a side-effect this function strips punctuation.
#' @export
ngrams <- function(string, n = 3) {

  words <- str_split(string, boundary("word"))
  words <- words[[1]]

  # Rolling subset of words, joined together
  begin <- seq(1, length(words) - n + 1)

  selections <- lapply(begin, function(x) {
    seq(x, x + n - 1)
  })

  vapply(selections, function(x) {
    words[x] %>% str_c(collapse = " ")
  }, character(1))

}
