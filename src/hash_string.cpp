#include <Rcpp.h>
#include <boost/functional/hash.hpp>
using namespace Rcpp;

//' Hash a string to an integer
//' @param x A character vector to be hashed.
//' @return A vector of integer hashes.
//' @examples
//' s <- c("How", "many", "roads", "must", "a", "man", "walk", "down")
//' hash_string(s)
//' @export
// [[Rcpp::export]]
IntegerVector hash_string(CharacterVector x) {

  boost::hash<std::string> hash_fn;
  int length = x.size();

  IntegerVector hash_vec(length);

  for(int i = 0; i < length; i++) {
    std::string str = Rcpp::as<std::string>(x[i]);
    hash_vec[i] = hash_fn(str);
  }

  return hash_vec;

}
