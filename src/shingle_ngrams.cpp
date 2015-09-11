#include <Rcpp.h>
using namespace Rcpp;

// Create shingled n-grams
// [[Rcpp::export]]
CharacterVector shingle_ngrams(CharacterVector words, int n) {
  int out_length = words.size() - n + 1;
  CharacterVector ngrams(out_length);
  for(int i = 0; i < out_length; i++) {
    CharacterVector subset = words[i - 1 + seq_len(n)];
    std::string ngram;
    for(int j = 0; j < n; j++) {
      ngram += subset[j];
      if(j != n - 1) ngram += " ";
    }
    ngrams[i] = ngram;
  }
  return ngrams;
}
