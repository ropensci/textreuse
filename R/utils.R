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
check_banding <- function(h, b) {
  h %% b == 0
}

assertthat::on_failure(check_banding) <- function(call, env) {
  "The number of hashes must be evenly divisible by the number of bands."
}

# Sequences for subsetting by bands in minhash
band_seq <- function(l, b) {
  assert_that(check_banding(l, b))
  r <- l / b
  starts <- seq.int(from = 1, to = l, by = r)
  lapply(starts, function(n) seq.int(n, n + r - 1, 1))
}

# Test that meta exists and that it has an ID value
has_id <- function(meta) {
  !is.null(meta$id)
}

assertthat::on_failure(has_id) <- function(call, env) {
  paste("When creating a document from a string instead of a file, the `id`",
        "field in the metadata list must be specified.")
}

# People might row_bind() two of these data frames, so we can't rely just on
# the class.
is_lsh_buckets <- function(x) {
  class_check <- inherits(x, "lsh_buckets")
  col_check <- identical(names(x), c("doc", "buckets")) &
    inherits(x, "data.frame")
  class_check | col_check
}

assertthat::on_failure(is_lsh_buckets) <- function(call, env) {
  "Object is not a data frame of LSH buckets."
}


is_candidates_df <- function(x) {
  inherits(x, "textreuse_candidates")
}

assertthat::on_failure(is_candidates_df) <- function(call, env) {
  "Object is not a candidates data frame."
}

sort_meta <- function(meta) {
  meta[order(names(meta))]
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
