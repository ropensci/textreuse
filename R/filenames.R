#' Filenames from paths
#'
#' This function takes a character vector of paths and returns just the file
#' name, by default without the extension. A \code{\link{TextReuseCorpus}} uses
#' the paths to the files in the corpus as the names of the list. This function
#' is intended to turn those paths into more manageable identifiers.
#'
#' @param paths A character vector of paths.
#' @param extension Should the file extension be preserved?
#' @seealso \code{\link{basename}}
#' @examples
#' paths <- c("corpus/one.txt", "corpus/two.md", "corpus/three.text")
#' filenames(paths)
#' filenames(paths, extension = TRUE)
#' @export
filenames <- function(paths, extension = FALSE) {
  assert_that(is.character(paths))
  f <- basename(paths)
  if (extension)
    return(f)
  else
    str_replace(f, "\\.[:alpha:]{1,}$", "")
}
