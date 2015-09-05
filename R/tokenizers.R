#' Split texts into tokens
#'
#' These functions each turn a text into tokens. The \code{tokenize_ngrams}
#' functions returns shingled n-grams.
#'
#' @name tokenizers
#' @param string A character vector of length 1 to be tokenized.
#' @param lowercase Should the tokens be made lower case.
#' @param n For n-gram tokenizers, the number of words in each n-gram.
#' @details These functions will strip all punctuation.
#' @return A character vector containing the tokens.
NULL

#' @export
#' @rdname tokenizers
tokenize_words <- function(string, lowercase = TRUE) {
  assert_that(assertthat::is.string(string))
  out <- str_split(string, boundary("word"))[[1]]
  if (lowercase) str_to_lower(out) else out
}

#' @export
#' @rdname tokenizers
tokenize_sentences <- function(string, lowercase = TRUE) {
  assert_that(assertthat::is.string(string))
  out <- str_split(string, boundary("sentence", skip_word_none = FALSE))[[1]]
  out <- str_replace_all(out, "[[:punct:]]", " ")
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_trim(out)
  if (lowercase) str_to_lower(out) else out
}

#' @export
#' @rdname tokenizers
tokenize_ngrams <- function(string, lowercase = TRUE, n = 3) {

  words <- tokenize_words(string, lowercase = lowercase)

  # Rolling subset of words, joined together
  begin <- seq(1, length(words) - n + 1)

  selections <- lapply(begin, function(x) {
    seq(x, x + n - 1)
  })

  vapply(selections, function(x) {
    words[x] %>% str_c(collapse = " ")
  }, character(1))

}
