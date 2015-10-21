#' @export
# http://etherealbits.com/2013/04/string-alignment-dynamic-programming-dna/
align_local <- function(a, b, match = 2L, mismatch = -1L, gap = -1L,
                        edit_mark = "#") {
 assert_that(identical(class(a), class(b)))
 UseMethod("align_local", a)
}

#' @export
align_local.TextReuseTextDocument <- function(a, b, match = 2L, mismatch = -1L,
                                              gap = -1L, edit_mark = "#") {
  align_local(content(a), content(b), match = match, mismatch = mismatch,
              gap = gap, edit_mark = edit_mark)
}

#' @export
align_local.default <- function(a, b, match = 2L, mismatch = -1L, gap = -1L,
                                edit_mark = "#") {

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

  # Create the integer matrix
  m <- matrix(0L, length(b) + 1L, length(a) + 1L)
  # rownames(m) <- c(NA, b)
  # colnames(m) <- c(NA, a)

  # Calculate the matrix of possible paths
  m <- sw_matrix(m, a, b, match, mismatch, gap)

  # Find the starting place in the matrix
  max_match <- which(m == max(m), arr.ind = TRUE, useNames = FALSE)

  if (nrow(max_match) > 1) {
    message("Multiple best alignments found; selecting only one of them.")
  }

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
  b_out[out_i] <- b[row_i - 1]
  a_out[out_i] <- a[col_i - 1]
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
  alignment <- list(a_edits = a_out, b_edits = b_out)
  class(alignment) <- "textreuse_alignment"

  alignment

}
