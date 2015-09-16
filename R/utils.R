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

# Check whether the number of minhashes is evenly divisble by number of bands
check_banding <- function(l, b) {
  l %% b == 0
}

assertthat::on_failure(check_banding) <- function(call, env) {
  "Bands times rows must equal the number of hashes."
}

# Sequences for subsetting by bands in minhash
band_seq <- function(l, b) {
  assert_that(check_banding(l, b))
  r <- l / b
  starts <- seq.int(from = 1, to = l, by = r)
  lapply(starts, function(n) seq.int(n, n + r - 1, 1))
}

# Append to value if key already exists
insert_into_hash <- function(k, v, hash) {
  if (hash::has.key(k, hash))
    hash[[k]] <- unique(c(hash[[k]], v))
  else
    hash[[k]] <- v
  invisible()
}

# Test that meta exists and that it has an ID value
has_id <- function(meta) {
  !is.null(meta$id)
}

assertthat::on_failure(has_id) <- function(call, env) {
  paste("When creating a document from a string instead of a file, the `id`",
        "field in the metadata list must be specified.")
}

sort_meta <- function(meta) {
  meta[order(names(meta))]
}

skip_on_appveyor <- function() {
  if (!identical(Sys.getenv("APPVEYOR"), "true"))
    return()
  testthat::skip("On Appveyor")
}
