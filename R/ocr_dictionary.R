# Check words in a text against an English dictionary
ocr_dictionary <- function(text) {
  text_words <- word_list(text)
  mismatches <- setdiff(text_words, words_en)
  errors <- Filter(function(x) { x %in% mismatches }, text_words)
  1 - (length(errors) / length(text_words))
}
