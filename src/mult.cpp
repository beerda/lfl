/*
 * File name: mult.cpp
 * Date:      2016/02/02 10:49
 * Author:    Michal Burda
 */


#include <Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export(name=".mult")]]
RcppExport SEXP mult(NumericMatrix x, NumericMatrix y, Function f)
{
    NumericMatrix res(x.nrow(), y.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < y.ncol(); ++j) {
            NumericVector vec = f(x(i, _), y(_, j));
            if (vec.size() <= 0) {
                stop("Callback function returned empty vector");
            }
            res(i, j) = vec[0];
        }
    }
    return res;
}

