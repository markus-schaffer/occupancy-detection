/*
 Geometric / Exponentially Weighted Moving Average (EWMA)
 
 The EWMA is defined recursively as:
    average[t] = lambda * x[t] + (1 - lambda) * average[t - 1],
 with the initialization average[0] = x[0].
 
 This file provides two Rcpp-exported functions:
 
 1) geo_ma_avg(): compute the EWMA (aka geometric moving average) of a
 numeric vector.
 
 2) geo_ma(): for each observation, flag whether the value is greater than
 or equal to its current EWMA.
 
 Assumptions (no runtime checks):
 - `x` has length >= 1.
 - `lambda` ∈ [0, 1].

 */

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector geo_ma_avg(NumericVector x, double lambda) {
  const int n = x.size();
  NumericVector average(n);
  
  // Initialize with the first observation
  average[0] = x[0];
  
  // Recursive EWMA update for t = 1..n-1.
  for (int t = 1; t < n; t++) {
    average[t] = lambda * x[t] + (1 - lambda) * average[t - 1];
  }
  
  return average;
}

// [[Rcpp::export]]
LogicalVector geo_ma(NumericVector x, double lambda) {
  const int n = x.size();
  LogicalVector result(n);
  
  // Running EWMA; initialized to the first observation.
  double average = x[0];
  
  // The first element compares x[0] with its own EWMA (equal → TRUE)
  result[0] = true;
  
  // Update EWMA and compare from t = 1..n-1
  for (int t = 1; t < n; ++t) {
    average = lambda * x[t] + (1.0 - lambda) * average;
    result[t] = x[t] >= average;
  }
  
  return result;
}