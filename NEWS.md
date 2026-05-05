# textreuse 0.1.5

- Updates due to dplyr 1.0.0 release
- Add an `encoding` argument for reading files in `TextReuseTextDocument()` and
  `TextReuseCorpus()`
- Return an empty local alignment instead of an error when two texts have no
  matching words
- Add `preserve_punctuation` to `align_local()` for preserving punctuation in
  displayed alignments
- Add `count_matches()` and `matching_tokens()` helpers for inspecting absolute
  match counts and matched tokens
- Parallelize `lsh_compare()` when `options(mc.cores)` is set on non-Windows
  platforms
- Add user interrupt checks to long-running C++ hashing and n-gram loops
- Add `lsh_add()` for adding new documents to an existing LSH bucket cache
- Add `as_sparse_matrix()` and preserve all document IDs when converting
  candidate pairs to matrices

# textreuse 0.1.4

- Preventative maintenance release to avoid failing tests when new version of
  BH is released.

# textreuse 0.1.3

- Preventative maintenance release to avoid failing tests when new versions of 
  the dplyr and testthat packages are released.

# textreuse 0.1.2

- Fix memory error in `shingle_ngrams()`
- Fix tests for retokenizing on Windows
- More informative error message if using `lsh()` on corpora without minhashes

# textreuse 0.1.1

- Fix progress bars in vignettes

# textreuse 0.1.0

- Initial release
