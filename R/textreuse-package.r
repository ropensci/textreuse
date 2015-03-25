#' textreuse: Detect Text Reuse and Document Similarity
#'
#' @references
#'
#' The sample data provided in the files \code{ca1851-match.txt},
#' \code{ca1851-nomatch.txt}, \code{ny1850-match.txt}, all of which are in the
#' \code{extdata/} directory, are taken from the following nineteenth-century
#' codes of civil procedure from California and New York.
#'
#' \emph{Final Report of the Commissioners on Practice and Pleadings}, in 2
#' \emph{Documents of the Assembly of New York}, 73rd Sess., No. 16, (1850):
#' 243-250, sections 597-613.
#' \href{http://books.google.com/books?id=9HEbAQAAIAAJ&pg=PA243#v=onepage&q&f=false}{Google
#' Books}.
#'
#' \emph{An Act To Regulate Proceedings in Civil Cases}, 1851 \emph{California
#' Laws} 51, 51-53 sections 4-17; 101, sections 313-316.
#' \href{http://books.google.com/books?id=4PHEAAAAIAAJ&pg=PA51#v=onepage&q&f=false}{Google
#' Books}.
#'
#' @name textreuse
#' @docType package
#' @useDynLib textreuse
#' @importFrom Rcpp sourceCpp
#' @import stringr
#' @import assertthat
NULL
