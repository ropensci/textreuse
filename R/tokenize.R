#' Recompute the tokens for a document
#'
#' Given a \code{\link{TextReuseTextDocument}}, this function recomputes its
#' tokens and hashes with the functions specified.
#'
#' @param doc A TextReuseTextDocument.
#' @param tokenizer A function to split the text into tokens. See
#'   \code{\link{tokenizers}}.
#' @param ... Arguments passed on to the \code{tokenizer}.
#' @param hash_func A function to hash the tokens. See
#'   \code{\link{hash_string}}.
#' @param keep_tokens Should the tokens be saved in the document that is
#'   returned or discarded?
#' @param keep_text Should the text be saved in the document that is returned or
#'   discarded?
#' @return The modified \code{\link{TextReuseTextDocument}}.
#' @export
tokenize <- function(doc, tokenizer, ..., hash_func = hash_string,
                     keep_tokens = TRUE, keep_text = TRUE) {
  assert_that(is.TextReuseTextDocument(doc),
              has_content(doc))
  doc$tokens <- tokenizer(doc$content, ...)
  doc$hashes <- hash_func(doc$tokens)
  if (!keep_tokens) doc$tokens <- NULL
  if (!keep_text) doc$text <- NULL
  doc
}
