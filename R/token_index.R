#' Build an index of tokens and documents
#'
#' Build an inverted index from tokens to the documents that contain them. This
#' is useful for finding document pairs that share one or more n-grams without
#' comparing every document pair. The corpus must be created with
#' \code{keep_tokens = TRUE}.
#'
#' @param corpus A \code{\link{TextReuseCorpus}} with retained tokens.
#' @param min_doc_count Minimum number of documents a token must appear in to
#'   be retained. Increase this to remove rare tokens.
#' @param max_doc_count Maximum number of documents a token may appear in to be
#'   retained. Decrease this to remove very common tokens.
#' @return A \code{textreuse_token_index} data frame with columns \code{token},
#'   \code{docs}, and \code{n_docs}.
#' @export
token_index <- function(corpus, min_doc_count = 2, max_doc_count = Inf) {
  assert_that(is.TextReuseCorpus(corpus),
              is.count(min_doc_count),
              is.number(max_doc_count),
              all(vapply(tokens(corpus), Negate(is.null), logical(1))))

  entries <- lapply(names(corpus), function(doc_id) {
    tibble::tibble(token = unique(tokens(corpus[[doc_id]])), doc = doc_id)
  })

  index <- dplyr::bind_rows(entries) %>%
    dplyr::group_by(.data$token) %>%
    dplyr::summarize(docs = list(sort(.data$doc)),
                     n_docs = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::filter(.data$n_docs >= min_doc_count,
                  .data$n_docs <= max_doc_count) %>%
    dplyr::arrange(.data$token)

  class(index) <- c("textreuse_token_index", class(index))

  index
}

#' Extract candidate document pairs from a token index
#'
#' @param index A \code{textreuse_token_index} object returned by
#'   \code{\link{token_index}}.
#' @return A \code{textreuse_candidates} data frame.
#' @export
token_index_candidates <- function(index) {
  assert_that(inherits(index, "textreuse_token_index"))

  pair_matrices <- lapply(index$docs, function(doc_ids) {
    if (length(doc_ids) < 2) return(NULL)
    t(utils::combn(doc_ids, 2))
  })
  pair_matrices <- Filter(Negate(is.null), pair_matrices)

  if (length(pair_matrices) == 0) {
    candidates <- tibble::tibble(a = character(), b = character(),
                                 score = numeric())
  } else {
    candidates <- do.call(rbind, pair_matrices) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      stats::setNames(c("a", "b")) %>%
      dplyr::mutate(a = pmin(.data$a, .data$b),
                    b = pmax(.data$a, .data$b)) %>%
      dplyr::distinct(.data$a, .data$b) %>%
      dplyr::arrange(.data$a, .data$b) %>%
      dplyr::mutate(score = NA_real_)
  }

  class(candidates) <- c("textreuse_candidates", class(candidates))

  candidates
}
