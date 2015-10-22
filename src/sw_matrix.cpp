#include <progress.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
IntegerMatrix sw_matrix(IntegerMatrix m, CharacterVector a, CharacterVector b,
                        int match, int mismatch, int gap, bool progress) {

  int rows = b.length();
  int cols = a.length();

  Progress p(rows, progress);

  for (int row_i = 0; row_i < rows; ++row_i) {

    if (Progress::check_abort())
      stop("Local alignment interrupted by user.\n");
    p.increment();

    for (int col_i = 0; col_i < cols; ++col_i) {

      int score = (a[col_i] == b[row_i] ? match : mismatch) + m(row_i, col_i);
      int deletion = m(row_i, col_i + 1) + gap;
      int insertion = m(row_i +1, col_i) + gap;
      int value = max(NumericVector::create(0, score, deletion, insertion));

      m(row_i + 1, col_i + 1) = value;

    }
  }

  return m;
}
