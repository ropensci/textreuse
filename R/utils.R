# Take results of readLines and turn it into a character vector of length 1
as_string <- function(x, collapse = " ") {
  x %>%
    str_c(collapse = " ") %>%
    NLP::as.String()
}

# Pretty print the metadata for a document
pretty_print_metadata <- function(doc) {
  lapply(names(doc$meta), function(x) cat(x, ":", doc$meta[[x]], "\n"))
}

#' Check if an object is a \code{\link{TextReuseTextDocument}}.
#' @param x An R object to check.
#' @export
is.TextReuseTextDocument <- function(x) {
  inherits(x, "TextReuseTextDocument")
}
