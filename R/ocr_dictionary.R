# Check words in a text against an English dictionary
ocr_dictionary <- function(text, sample_size) {
  text <- text %>%
    str_to_lower() %>%
    str_split(boundary("word"))

  vapply(text, function(words) {

    if (0 < sample_size & sample_size < length(words))
      words <- sample(words, sample_size)

    mismatches <- setdiff(words, words_en)
    errors <- Filter(function(x) {x %in% mismatches}, words)

    1 - (length(errors) / length(words))

  }, numeric(1))

}
