/*
 * File name: hedge.cpp
 * Date:      2016/06/13 10:49
 * Author:    Michal Burda
 */


#include <Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export(name=".hedge")]]
NumericVector hedge(NumericVector x, NumericVector p)
{
    double p1 = p[0];
    double p2 = p[1];
    double p3 = p[2];

    NumericVector res(x.size());
    for (int i = 0; i < x.size(); ++i) {
        if (NumericVector::is_na(x[i])) {  // NA or NaN
            res[i] = NA_REAL;
        } else if (x[i] <= p1) {
            res[i] = 0;
        } else if (x[i] <= p2) {
            double t = x[i] - p1;
            res[i] = t * t / ((p2 - p1) * (p3 - p1));
        } else if (x[i] < p3) {
            double t = p3 - x[i];
            res[i] = 1 - t * t / ((p3 - p2) * (p3 - p1));
        } else {
            res[i] = 1;
        }
    }
    return res;
}
