# textreuse 1.0.0

This release brings together several years of maintenance and feature work to
make textreuse easier to use on current R installations and more practical for
larger document collections.

## Text input and corpus construction

- `TextReuseTextDocument()` and `TextReuseCorpus()` now accept an `encoding`
  argument, making it easier to read source files whose text encoding is known
  or differs from the platform default.
- `TextReuseCorpus()` now keeps skipped-document bookkeeping deterministic.
  Skipped documents are reported consistently, and skip metadata is available
  even when `skip_short = FALSE`.
- Very short documents are handled more predictably when skip n-grams are used,
  avoiding assertion failures and making corpus construction easier to diagnose.

## Alignment and match inspection

- `align_local()` now returns an empty local alignment instead of throwing an
  error when two texts have no matching words. This makes batch alignment
  workflows easier to run because no-match pairs can be represented directly.
- `align_local()` gains `preserve_punctuation`, allowing displayed alignments to
  keep punctuation from the original texts when that context is useful.
- New `count_matches()` and `matching_tokens()` helpers expose absolute match
  counts and the matched tokens themselves, so users can inspect what drove a
  similarity score rather than relying only on a ratio.

## Candidate generation and comparison

- New token-index helpers find candidate document pairs from shared n-grams,
  giving users another way to identify likely reuse pairs before running more
  expensive comparisons.
- `pairwise_candidates()` and matrix conversion now preserve all document IDs,
  including documents without returned candidate pairs.
- `as_sparse_matrix()` provides a sparse matrix representation of candidate
  results, which is more convenient for downstream modeling, graph analysis, and
  workflows with many documents.

## Locality-sensitive hashing

- `lsh_add()` can add new documents to an existing LSH bucket cache, so users can
  extend an index without rebuilding it from scratch.
- `lsh_compare()` can run comparisons in parallel on non-Windows platforms when
  `options(mc.cores)` is set.
- Long-running C++ hashing and n-gram loops now check for user interrupts, so
  expensive jobs can be stopped more cleanly from R.

## Compatibility and documentation

- Compatibility with current dplyr and tidyr releases has been refreshed.
- README, vignette, reference, and pkgdown examples were regenerated against
  current package output.
- Stale external links and documentation badges were updated so package checks
  and the public documentation site are cleaner.

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
