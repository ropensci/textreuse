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
#'  This technique means that documents hashed into a cache, so that the LSH
#'  signatures for each document only need to be computed once. Furthermore, so
#'  long as the same minhash and LSH functions are used, additional documents
#'  can be added to the cache at any time. And since minhash signatures are
#'  used, each document can be added to the cache fairly quickly.
#'
#'  This function returns a cache object. This object is a
#'  \href{https://en.wikipedia.org/wiki/Hash_table}{hash table} as implemented
#'  by the \href{https://cran.r-project.org/package=hash}{hash package}. Unlike
#'  every other type of R object, which has copy-on-modify semantics, this hash
#'  table/cache object does not. That means that the variable name is only a
#'  pointer to the actual hash table, and multiple variable names can point to
#'  the same hash table. If no cache object is passed to the function, then a
#'  new cache is generated.
#'
#'  This cache object can then be queried to detect pairs or clusters of
#'  documents.
#'
#'@param x A \code{\link{TextReuseCorpus}} or
#'  \code{\link{TextReuseTextDocument}} to add to the cache.
#'@param bands The number of bands to use for locality sensitive hashing. The
#'  number of hashes in the documents in the corpus must be evenly divisible by
#'  the number of bands.
#'@param buckets A \code{\link[hash]{hash}} object that caches the potential
#'  matches. If the value is \code{NULL} a new cache will be created. If a cache
#'  is passed to the function, it will be added to. Note that it is important
#'  that you keep the minhash function and the number of bands consistent when
#'  adding to the cache.
#'@return A \code{\link[hash]{hash}} object where the keys are hashed from the
#'  bands in locality sensitve hashing and the values are sets of potential
#'  matches.
#'@references Jure Leskovec, Anand Rajaraman, Jeff Ullman,
#'  \href{http://www.mmds.org/#book}{\emph{Mining of Massive Datasets}}
#'  (Cambridge University Press, 2011), ch. 3. See also Matthew Casperson,
#'  "\href{http://matthewcasperson.blogspot.com/2013/11/minhash-for-dummies.html}{Minhash
#'   for Dummies}" (November 14, 2013).
#'@seealso \code{\link{minhash_generator}}, \code{\link{lsh_candidates}},
#'  \code{\link{lsh_probability}}, \code{\link{lsh_threshold}}
#' @examples
#' dir <- system.file("extdata", package = "textreuse")
#' set.seed(253)
#' minhash <- minhash_generator(200)
#' corpus <- TextReuseCorpus(dir = dir,
#'                           tokenizer = tokenize_ngrams, n = 5,
#'                           hash_func = minhash)
#' names(corpus) <- filenames(names(corpus))
#' buckets <- lsh(corpus, bands = 50)
#' buckets
#'@export
lsh <- function(x, bands = 40, buckets = NULL) UseMethod("lsh", x)

#' @export
lsh.TextReuseCorpus <- function(x, bands = 40, buckets = NULL) {
  assert_that(is.count(bands))
  if (is.null(buckets))
    buckets <- hash::hash()
  else
    assert_that(hash::is.hash(buckets))

  hash_list <- hashes(x)

  subsets <- band_seq(length(hash_list[[1]]), bands)

  lapply(names(hash_list), function(n) {
    lapply(subsets, function(i) {
      key <- digest::digest(hash_list[[n]][i])
      insert_into_hash(key, n, buckets)
    })
  })

  buckets

}

#' @export
lsh.TextReuseTextDocument <- function(x, bands = 40, buckets = NULL) {
  assert_that(is.count(bands),
              has_id(meta(x)))
  if (is.null(buckets))
    buckets <- hash::hash()
  else
    assert_that(hash::is.hash(buckets))

  hash_vec <- hashes(x)

  subsets <- band_seq(length(hash_vec), bands)

  lapply(subsets, function(i) {
    key <- digest::digest(hash_vec[i])
    insert_into_hash(key, meta(x, "id"), buckets)
  })

  buckets

}
