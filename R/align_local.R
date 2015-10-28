#' Local alignment of natural language texts
#'
#' This function takes two texts, either as strings or as
#' \code{TextReuseTextDocument} objects, and finds the optimal local alignment
#' of those texts. A local alignment finds the best matching subset of the two
#' documents. This function adapts the
#' \href{https://en.wikipedia.org/wiki/Smith-Waterman_algorithm}{Smith-Waterman
#' algorithm}, used for genetic sequencing, for use with natural language. It
#' compare the texts word by word (the comparison is case-insensitive) and
#' scores them according to a set of parameters. These parameters define the
#' score for a \code{match}, and the penalties for a \code{mismatch} and for
#' opening a \code{gap} (i.e., the first mismatch in a potential sequence). The
#' function then reports the optimal local alignment. Only the subset of the
#' documents that is a match is included. Insertions or deletions in the text
#' are reported with the \code{edit_mark} character.
#'
#' @param a A character vector of length one, or a
#'   \code{\link{TextReuseTextDocument}}.
#' @param b A character vector of length one, or a
#'   \code{\link{TextReuseTextDocument}}.
#' @param match The score to assign a matching word. Should be a positive
#'   integer.
#' @param mismatch The score to assign a mismatching word. Should be a negative
#'   integer or zero.
#' @param gap The penalty for opening a gap in the sequence. Should be a
#'   negative integer or zero.
#' @param edit_mark A single character used for displaying for displaying
#'   insertions/deletions in the documents.
#' @param progress Display a progress bar and messages while computing the
#'   alignment.
#'
#' @return A list with the class \code{textreuse_alignment}. This list contains
#'   several elements: \itemize{ \item \code{a_edit} and \code{b_edit}:
#'   Character vectors of the sequences with edits marked. \item \code{score}:
#'   The score of the optimal alignment. }
#'
#' @details
#'
#' The compute time of this function is proportional to the product of the
#' lengths of the two documents. Thus, longer documents will take considerably
#' more time to compute. This function has been tested with pairs of documents
#' containing about 25 thousand words each.
#'
#' If the function reports that there were multiple optimal alignments, then it
#' is likely that there is no strong match in the document.
#'
#' The score reported for the local alignment is dependent on both the size of
#' the documents and on the strength of the match, as well as on the parameters
#' for match, mismatch, and gap penalties, so the scores are not directly
#' comparable.
#'
#' @references For a useful description of the algorithm, see
#'   \href{http://etherealbits.com/2013/04/string-alignment-dynamic-programming-dna/}{this
#'   post}. For the application of the Smith-Waterman algorithm to natural
#'   language, see David A. Smith, Ryan Cordell, and Elizabeth Maddock Dillon,
#'   "Infectious Texts: Modeling Text Reuse in Nineteenth-Century Newspapers."
#'   IEEE International Conference on Big Data, 2013,
#'   \url{http://hdl.handle.net/2047/d20004858}.
#'
#' @examples
#' align_local("The answer is blowin' in the wind.",
#'             "As the Bob Dylan song says, the answer is blowing in the wind.")
#'
#' # Example of matching documents from a corpus
#' dir <- system.file("extdata/legal", package = "textreuse")
#' corpus <- TextReuseCorpus(dir = dir, progress = FALSE)
#' alignment <- align_local(corpus[["ca1851-match"]], corpus[["ny1850-match"]])
#' str(alignment)
#'
#' @export
align_local <- function(a, b, match = 2L, mismatch = -1L, gap = -1L,
                        edit_mark = "#", progress = interactive()) {
 assert_that(identical(class(a), class(b)))
 UseMethod("align_local", a)
}

#' @export
align_local.TextReuseTextDocument <- function(a, b, match = 2L, mismatch = -1L,
                                              gap = -1L, edit_mark = "#",
                                              progress = interactive()) {
  align_local(content(a), content(b), match = match, mismatch = mismatch,
              gap = gap, edit_mark = edit_mark)
}

#' @export
align_local.default <- function(a, b, match = 2L, mismatch = -1L, gap = -1L,
                                edit_mark = "#", progress = interactive()) {

  assert_that(is.string(a),
              is.string(b),
              is_integer_like(match),
              is_integer_like(mismatch),
              is_integer_like(gap),
              is.string(edit_mark))

  if (match <= 0 || mismatch > 0 || gap > 0 || !(str_length(edit_mark) == 1)) {
    stop("The scoring parameters should have the following characteristics:\n",
         "    - `match` should be a positive integer\n",
         "    - `mismatch` should be a negative integer or zero\n",
         "    - `gap` should be a negative integer or zero\n",
         "    - `edit_mark` should be a single character\n")
  }

  # Keep everything as integers because IntegerMatrix saves memory
  match    <- as.integer(match)
  mismatch <- as.integer(mismatch)
  gap      <- as.integer(gap)

  # Prepare the character vectors. Tokenize to words to compare word by word.
  # Use all lower case for the comparison, but use original capitalization in
  # the output.
  a_orig <- tokenize_words(a, lowercase = FALSE)
  b_orig <- tokenize_words(b, lowercase = FALSE)
  a <- str_to_lower(a_orig)
  b <- str_to_lower(b_orig)

  # Only show a progress bar for long computations
  n_rows <- length(b) + 1
  n_cols <- length(a) + 1
  if (n_rows * n_cols < 1e7) progress <- FALSE

  # Create the integer matrix
  if (progress) {
    message("Preparing a matrix with ",
            prettyNum(n_rows * n_cols, big.mark = ","),
            " elements.")
  }
  m <- matrix(0L, n_rows, n_cols)

  # Calculate the matrix of possible paths
  if (progress) message("Computing the optimal local alignment.")
  m <- sw_matrix(m, a, b, match, mismatch, gap, progress)

  # Find the starting place in the matrix
  alignment_score <- max(m)
  max_match <- which(m == alignment_score, arr.ind = TRUE, useNames = FALSE)

  if (nrow(max_match) > 1) {
    warning("Multiple optimal local alignments found; selecting only one of them.",
            call. = FALSE)
  }

  if (progress) message("Extracting the local alignment.")

  # Create output vectors which are as long as conceivably necessary
  a_out <- vector(mode = "character", length = max(max_match))
  b_out <- vector(mode = "character", length = max(max_match))
  a_out[] <- NA_character_
  b_out[] <- NA_character_

  # Initialize counters for the matrix and the output vector
  row_i <- max_match[1, 1]
  col_i <- max_match[1, 2]
  out_i <- 1L

  # Place our first known values in the output vectors
  b_out[out_i] <- b_orig[row_i - 1]
  a_out[out_i] <- a_orig[col_i - 1]
  out_i = out_i + 1L # Advance the out vector position

  # Begin moving up, left, or diagonally within the matrix till we hit a zero
  while (m[row_i - 1, col_i - 1] != 0) {

    # Values of the current cell, the cells up, left, diagonal, and the max
    up       <- m[row_i - 1, col_i]
    left     <- m[row_i, col_i - 1]
    diagn    <- m[row_i - 1, col_i - 1]
    max_cell <- max(up, left, diagn)

    # Move in the direction of the maximum cell. If there are ties, choose up
    # first, then left, then diagonal. Privilege up and left because they
    # preserve edits.
    #
    # In each case add the current words to the out vectors. For moves up and
    # and left there will be an insertion/deletion, so add a symbol like ####
    # that is the same number of characters as the word in the other vector.
    #
    # Note that the index of the matrix is offset by one from character vectors
    # a and b, so we use the row and column indices - 1. The column corresponds
    # to `a` and the rows correspond to `b`.
    if (up == max_cell) {
      row_i <- row_i - 1
      bword <- b_orig[row_i - 1]
      b_out[out_i] <- bword
      a_out[out_i] <- mark_chars(bword, edit_mark)
    } else if (left == max_cell) {
      col_i <- col_i - 1
      aword <- a_orig[col_i - 1]
      b_out[out_i] <- mark_chars(aword, edit_mark)
      a_out[out_i] <- aword
    } else if (diagn == max_cell) {
      row_i <- row_i - 1
      col_i <- col_i - 1
      bword <-  b_orig[row_i - 1]
      aword <- a_orig[col_i - 1]
      # Diagonals are a special case, because instead of an insertion or a
      # deletion we might have a substitution of words. If that is the case,
      # then treat it like a double insertion and deletion.
      if (str_to_lower(aword) == str_to_lower(bword)) {
        b_out[out_i] <- bword
        a_out[out_i] <- aword
      } else {
        b_out[out_i] <- bword
        a_out[out_i] <- mark_chars(bword, edit_mark)
        out_i <- out_i + 1
        b_out[out_i] <- mark_chars(aword, edit_mark)
        a_out[out_i] <- aword
      }
    }

    # Move forward one position in the out vectors, no matter which direction
    # we moved
    out_i <- out_i + 1

  }

  # Clean up the outputs
  b_out <- str_c(rev(b_out[!is.na(b_out)]), collapse = " ")
  a_out <- str_c(rev(a_out[!is.na(a_out)]), collapse = " ")

  # Create the alignment object
  alignment <- list(a_edits = a_out, b_edits = b_out, score = alignment_score)
  class(alignment) <- c("textreuse_alignment", "list")

  alignment

}

#' @export
print.textreuse_alignment <- function(x, ...) {
  cat("TextReuse alignment\n")
  cat("Alignment score:", x$score, "\n")
  cat("Document A:\n")
  cat(str_wrap(x$a_edits, width = 72))
  cat("\n\nDocument B:\n")
  cat(str_wrap(x$b_edits, width = 72))
  invisible(x)
}
