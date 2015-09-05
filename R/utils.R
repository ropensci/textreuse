# Take results of readLines and turn it into a character vector of length 1
as_string <- function(x, collapse = " ") {
  x %>%
    str_c(collapse = " ") %>%
    NLP::as.String()
}
