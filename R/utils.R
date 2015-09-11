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

#' @param x An R object to check.
#' @export
#' @rdname TextReuseTextDocument
is.TextReuseTextDocument <- function(x) {
  inherits(x, "TextReuseTextDocument")
}

#' @export
#' @rdname TextReuseTextDocument
has_content <- function(x) {
  assert_that(is.TextReuseTextDocument(x))
  !is.null(x$content)
}

assertthat::on_failure(has_content) <- function(call, env) {
  paste0(deparse(call$x), " does not have text in its content field.")
}
