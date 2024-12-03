#include <Rcpp.h>
using namespace Rcpp;

//' @title GCD and LCM functions
//' @param x A numeric object that can be coerced to integer
//' @param y A numeric object that can be coerced to integer
//' @return The GCD or LCM
//' @rdname gcd_and_lcm
//' @export
// [[Rcpp::export]]
int gcd(int x, int y) {
  return  std::gcd(x, y);
}

//' @rdname gcd_and_lcm
//' @export
// [[Rcpp::export]]
int lcm(int x, int y) {
  return  std::lcm(x, y);
}
