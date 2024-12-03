// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// gcd
int gcd(int x, int y);
RcppExport SEXP _Rational_gcd(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(gcd(x, y));
    return rcpp_result_gen;
END_RCPP
}
// lcm
int lcm(int x, int y);
RcppExport SEXP _Rational_lcm(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(lcm(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Rational_gcd", (DL_FUNC) &_Rational_gcd, 2},
    {"_Rational_lcm", (DL_FUNC) &_Rational_lcm, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_Rational(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
