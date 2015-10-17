#' Split texts into tokens
#'
#' These functions each turn a text into tokens. The \code{tokenize_ngrams}
#' functions returns shingled n-grams.
#'
#' @name tokenizers
#' @param string A character vector of length 1 to be tokenized.
#' @param lowercase Should the tokens be made lower case?
#' @param n For n-gram tokenizers, the number of words in each n-gram.
#' @param k For the skip n-gram tokenizer, the maximum skip distance between
#'   words. The function will compute all skip n-grams between \code{0} and
#'   \code{k}.
#' @details These functions will strip all punctuation.
#' @return A character vector containing the tokens.
#' @examples
#' dylan <- "How many roads must a man walk down? The answer is blowin' in the wind."
#' tokenize_words(dylan)
#' tokenize_sentences(dylan)
#' tokenize_ngrams(dylan, n = 2)
#' tokenize_skip_ngrams(dylan, n = 3, k = 2)
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
  assert_that(is.count(n),
              assertthat::is.string(string))
  words <- tokenize_words(string, lowercase = lowercase)
  assert_that(n < length(words))
  shingle_ngrams(words, n = n)
}

#' @export
#' @rdname tokenizers
tokenize_skip_ngrams <- function(string, lowercase = TRUE, n = 3, k = 1) {
  assert_that(is.count(n),
              is.count(k) | k == 0,
              assertthat::is.string(string))
  words <- tokenize_words(string, lowercase = lowercase)
  assert_that(n + n * k - k <= length(words))
  skip_ngrams(words, n = n, k = k)
}
