# Generate a list of unique lowercase words used in a text
word_list <- function(text) {
  text %>%
    str_split(boundary("word")) %>%
    `[[`(1) %>%
    str_to_lower()
}
