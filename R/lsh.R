#'Locality sensitive hashing for minhash
#'
#'Locality sensitive hashing (LSH) discovers potential matches among a corpus of
#'documents quickly, so that only likely pairs can be compared.
#'
#'@details Locality sensitive hashing is a technique for detecting document
#'  similarity that does not require pairwise comparisons. When comparing pairs
#'  of documents, the number of pairs grows rapidly, so that only the smallest
#'  corpora can be compared pairwise in a reasonable amount of computation time.
#'  Locality sensitive hashing, on the other hand, takes a document which has
#'  been tokenized and hashed using a minhash algorithm. (See
#'  \code{\link{minhash_generator}}.) Each set of minhash signatures is then
#'  broken into bands comprised of a certain number of rows. (For example, 200
#'  minhash signatures might be broken down into 20 bands each containing 10
#'  rows.) Each band is then hashed to a bucket. Documents with identical rows
#'  in a band will be hashed to the same bucket. The likelihood that a document
#'  will be marked as a potential duplicate is proportional to the number of
#'  bands and inversely proportional to the number of rows in each band.
#'
#'  This function returns a data frame with the additional class
#'  \code{lsh_buckets}. The LSH technique only requires that the signatures for
#'  each document be calculated once. So it is possible, as long as one uses the
#'  same minhash function and the same number of bands, to combine the outputs
#'  from this function at different times. The output can thus be treated as a
#'  kind of cache of LSH signatures.
#'
#'  To extract pairs of documents from the output of this function, see
#'  \code{\link{lsh_candidates}}.
#'
#'@param x A \code{\link{TextReuseCorpus}} or
#'  \code{\link{TextReuseTextDocument}}.
#'@param bands The number of bands to use for locality sensitive hashing. The
#'  number of hashes in the documents in the corpus must be evenly divisible by
#'  the number of bands. See \code{\link{lsh_threshold}} and
#'  \code{\link{lsh_probability}} for guidance in selecting the number of bands
#'  and hashes.
#'@param progress Display a progress bar while comparing documents.
#'
#'@return A data frame (with the additional class \code{lsh_buckets}),
#'  containing a column with the document IDs and a column with their LSH
#'  signatures, or buckets.
#'
#'@references Jure Leskovec, Anand Rajaraman, and Jeff Ullman,
#'  \href{http://www.mmds.org/#book}{\emph{Mining of Massive Datasets}}
#'  (Cambridge University Press, 2011), ch. 3. See also Matthew Casperson,
#'  "\href{http://matthewcasperson.blogspot.com/2013/11/minhash-for-dummies.html}{Minhash
#'   for Dummies}" (November 14, 2013).
#'
#'@seealso \code{\link{minhash_generator}}, \code{\link{lsh_candidates}},
#'  \code{\link{lsh_query}}, \code{\link{lsh_probability}},
#'  \code{\link{lsh_threshold}}
#'
#' @examples
#' dir <- system.file("extdata/legal", package = "textreuse")
#' minhash <- minhash_generator(200, seed = 235)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           minhash_func = minhash)
#' buckets <- lsh(corpus, bands = 50)
#' buckets
#'@export
lsh <- function(x, bands, progress = interactive()) {
  UseMethod("lsh", x)
}

#' @export
lsh.TextReuseCorpus <- function(x, bands, progress = interactive()) {

  assert_that(is.count(bands),
              has_minhashes_corpus(x))

  h <- length(minhashes(x[[1]])) # number of hashes
  d <- length(x) # number of documents
  r <- h / bands # number of rows

  assert_that(check_banding(h, bands))

  # To assign rows in data frame to bands
  b_assign <-  dplyr::data_frame(band =
      rep(vapply(1:bands, function(i) rep(i, r), integer(r)), d)
    )

  all_minhashes <- minhashes(x)
  col_names <- names(all_minhashes)

  buckets <- all_minhashes %>%
    dplyr::as_data_frame() %>%
    tidyr::gather_("doc", "hash", col_names) %>%
    dplyr::mutate_(doc = ~as.character(doc)) %>%
    dplyr::bind_cols(b_assign) %>%
    dplyr::group_by_(~doc, ~band)

    rm(b_assign)

    if (progress) {
      message("Calculating LSH buckets")
      pb <- txtProgressBar(min = 0, max = d * bands, style = 3)
    }

  # include the band in the signature hash to avoid false matches
  buckets <- buckets %>%
    dplyr::summarize_(buckets = ~digest_progress(list(hash, unique(band)),
                                                 pb, progress))

  if (progress) close(pb)

  buckets <- buckets %>%
    dplyr::select_(~-band) %>%
    dplyr::ungroup()

  class(buckets) <- c(class(buckets), "lsh_buckets")

  buckets

}

# A wrapper around digest to be able to use the progress bar
digest_progress <- function(x, pb, progress) {
  bucket <- digest::digest(x)
  if (progress) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  bucket
}

#' @export
lsh.TextReuseTextDocument <- function(x, bands, progress) {

  assert_that(is.count(bands),
              has_minhashes(x))

  all_minhashes <- minhashes(x)
  h <- length(all_minhashes) # number of hashes
  r <- h / bands # number of rows

  assert_that(check_banding(h, bands))

  # To assign rows in data frame to bands
  b_assign <-  dplyr::data_frame(band =
      rep(vapply(1:bands, function(i) rep(i, r), integer(r)), 1)
    )


  buckets <- dplyr::data_frame(doc = x$meta$id, hash = all_minhashes) %>%
    dplyr::bind_cols(b_assign) %>%
    dplyr::group_by_(~doc, ~band) %>%
    dplyr::summarize_(buckets = ~digest::digest(list(hash, unique(band)))) %>%
    dplyr::select_(~-band) %>%
    dplyr::ungroup()

  class(buckets) <- c(class(buckets), "lsh_buckets")

  buckets

}
