// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calcGseaStatCumulativeParallel
List calcGseaStatCumulativeParallel(NumericVector const& stats, int n, int k, double gseaParam, int m, NumericVector const& pathwayScores, IntegerVector const& pathwaysSizes, int iterations, bool setSeed, int seed);
RcppExport SEXP fgsea_calcGseaStatCumulativeParallel(SEXP statsSEXP, SEXP nSEXP, SEXP kSEXP, SEXP gseaParamSEXP, SEXP mSEXP, SEXP pathwayScoresSEXP, SEXP pathwaysSizesSEXP, SEXP iterationsSEXP, SEXP setSeedSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector const& >::type stats(statsSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type gseaParam(gseaParamSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector const& >::type pathwayScores(pathwayScoresSEXP);
    Rcpp::traits::input_parameter< IntegerVector const& >::type pathwaysSizes(pathwaysSizesSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< bool >::type setSeed(setSeedSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(calcGseaStatCumulativeParallel(stats, n, k, gseaParam, m, pathwayScores, pathwaysSizes, iterations, setSeed, seed));
    return rcpp_result_gen;
END_RCPP
}
