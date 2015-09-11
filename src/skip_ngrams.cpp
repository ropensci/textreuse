#include <Rcpp.h>
using namespace Rcpp;

// Skip n-grams
// @param n = number of words in an n-gram
// @param k = max number of words to skip
// [[Rcpp::export]]
CharacterVector skip_ngrams(CharacterVector words, int n, int k) {
  int w = words.size(); // w = number of words
  int g = 0; // g = number of n-grams
  for(int i = k; i >= 0; i--) { // i = current iteration of k
    int window = n + n * i - i; // width of n-grams plus skips
    if(window > w) continue;
    g += w - window + 1;
  }
  CharacterVector ngrams(g);

  int position = 0; // position = place to store current ngram
  while(k >= 0) { // loop over k in descending order

    int window = n + n * k - k;
    for(int i = 0; i < w - window + 1; i++) { // loop over the words
      NumericVector subset(n); // the subset we are going to make of words
      for(int j = 0; j < n; j++) { // loop over number of n in ngrams
        subset[j] = i + j + j * k;
        // Rcpp::Rcout << "j = " << j << std::endl;
        // Rcpp::Rcout << "j + j * k = " << j + j * k << std::endl;
      }

      CharacterVector words_subset = words[subset];


      // turn the vector of words into a string
      std::string ngram;
      for(int l = 0; l < n; l++) {
        ngram += words_subset[l];
        if(l != n - 1) ngram += " ";
      }
      // store the current ngram and iterate
      ngrams[position] = ngram;
      position++;

    }
    k--; // iterate k
  }

  return ngrams;
}
