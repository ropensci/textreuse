#' @param string A character vector to be split into n-grams.
#' @param n The value of \code{n}.
#' @param lowercase Should the n-grams be made lowercase.
#' @return A character vector containing the n-grams.
#' @details
#' As a side-effect this function strips punctuation.
#' @export
#' @rdname tokenizers
ngrams <- function(string, n = 3, lowercase = TRUE) {

  words <- str_split(string, boundary("word"))
  words <- words[[1]]

  if (lowercase)
    words <- str_to_lower(words)

  # Rolling subset of words, joined together
  begin <- seq(1, length(words) - n + 1)

  selections <- lapply(begin, function(x) {
    seq(x, x + n - 1)
  })

  vapply(selections, function(x) {
    words[x] %>% str_c(collapse = " ")
  }, character(1))

}
