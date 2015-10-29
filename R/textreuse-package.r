#'@details
#' The best place to begin with this package in the introductory vignette.
#'
#' \code{vignette("textreuse-introduction", package = "textreuse")}
#'
#' After reading that vignette, the "pairwise" and "minhash" vignettes introduce
#' specific paths for working with the package.
#'
#' \code{vignette("textreuse-pairwise", package = "textreuse")}
#'
#' \code{vignette("textreuse-minhash", package = "textreuse")}
#'
#' \code{vignette("textreuse-alignment", package = "textreuse")}
#'
#' Another good place to beign with the package is the documentation for loading
#' documents (\code{\link{TextReuseTextDocument}} and
#' \code{\link{TextReuseCorpus}}), for \link{tokenizers},
#' \link[=similarity-functions]{similarity functions}, and
#' \link[=lsh]{locality-sensitive hashing}.
#'
#' @references The sample data provided in the \code{extdata/legal} directory is
#'   taken from a
#'   \href{http://lincolnmullen.com/blog/corpus-of-american-tract-society-publications/}{corpus
#'    of American Tract Society publications} from the nineteen-century,
#'   gathered from the \href{https://archive.org/}{Internet Archive}.
#'
#'   The sample data provided in the \code{extdata/legal} directory, are taken
#'   from the following nineteenth-century codes of civil procedure from
#'   California and New York.
#'
#'   \emph{Final Report of the Commissioners on Practice and Pleadings}, in 2
#'   \emph{Documents of the Assembly of New York}, 73rd Sess., No. 16, (1850):
#'   243-250, sections 597-613.
#'   \href{http://books.google.com/books?id=9HEbAQAAIAAJ&pg=PA243#v=onepage&q&f=false}{Google
#'    Books}.
#'
#'   \emph{An Act To Regulate Proceedings in Civil Cases}, 1851 \emph{California
#'   Laws} 51, 51-53 sections 4-17; 101, sections 313-316.
#'   \href{http://books.google.com/books?id=4PHEAAAAIAAJ&pg=PA51#v=onepage&q&f=false}{Google
#'    Books}.
#'
#' @useDynLib textreuse
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#' @import stringr
#' @import assertthat
#' @importFrom utils getTxtProgressBar setTxtProgressBar txtProgressBar
"_PACKAGE"

if (getRversion() >= "2.15.1") {
 utils::globalVariables(c("doc.x", "doc.y", "up", "dn", "a", "b"))
}
