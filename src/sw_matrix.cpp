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

    int score, deletion, insertion, value;

    for (int col_i = 0; col_i < cols; ++col_i) {

      score = (a[col_i] == b[row_i] ? match : mismatch) + m(row_i, col_i);
      deletion = m(row_i, col_i + 1) + gap;
      insertion = m(row_i +1, col_i) + gap;
      value = max(NumericVector::create(0, score, deletion, insertion));

      m(row_i + 1, col_i + 1) = value;

    }
  }

  return m;
}
