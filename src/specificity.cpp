/*
 * File name: specificity.cpp
 * Date:      2017/10/03 10:49
 * Author:    Michal Burda
 */


#include <Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export(name=".specificity")]]
bool specificity(NumericVector x, NumericVector y, NumericVector vars, NumericMatrix specs) {
    for (int j = 0; j < y.length(); j++) {
        int i = 0;
        while (i < x.length()) {
            if (vars[y[j]] == vars[x[i]]) {
                break;
            }
            i++;
        }
        if (i >= x.length()) {
            return FALSE;
        }
        if (x[i] != y[j] && specs(x[i], y[j]) == 0) {
            return FALSE;
        }
    }
    return TRUE;
}
