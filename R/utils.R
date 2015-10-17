# Take results of readLines and turn it into a character vector of length 1
as_string <- function(x) {
  x %>%
    str_c(collapse = "\n") %>%
    NLP::as.String()
}

# Pretty print the metadata for a document
pretty_print_metadata <- function(doc) {
  lapply(names(doc$meta), function(x) cat(x, ":", doc$meta[[x]], "\n"))
}

# Check whether the number of minhashes is evenly divisible by number of bands
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
  if (!identical(Sys.getenv("APPVEYOR"), "True"))
    return()
  testthat::skip("On Appveyor")
}

sort_df_by_rows <- function(df) {
  assert_that(all(c("a", "b") %in% colnames(df)),
              is.data.frame(df))
  for (i in seq_len(nrow(df))) {
    ordered <- sort(c(df[i, "a"], df[i, "b"]))
    df[i, "a"] <- ordered[1]
    df[i, "b"] <- ordered[2]
  }
  df
}

sort_df_by_columns <- function(df) {
  assert_that(all(c("a", "b") %in% colnames(df)),
              is.data.frame(df))
  df <- df[with(df, order(a, b)), ]
  # rownames(df) <- NULL
  df
}
