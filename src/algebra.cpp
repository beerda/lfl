/*
 * File name: algebra.cpp
 * Date:      2016/01/29 10:49
 * Author:    Michal Burda
 */


#include "algebra.h"
#include <common.h>


#define testInvalids(x) \
    if ((x) < 0 || (x) > 1) {                \
        stop("argument out of range 0..1");  \
    }                                        \
    if (R_IsNaN((x))) {                      \
        stop("NaN argument");                \
    }                                        \


using namespace Rcpp;
using namespace std;


RcppExport SEXP minNorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 1.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
        } else if (vals[i] < res) {
            res = vals[i];
        }
    }
    if (!naRm[0] & na & res > 0) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP lukNorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 1.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
            ++res;
        } else {
            res += vals[i];
        }
    }
    res -= vals.size();
    if (res <= 0) {
        return wrap(0.0);
    } else if (!naRm[0] & na) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP prodNorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 1.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
        } else {
            res = res * vals[i];
        }
    }
    if (!naRm[0] & na & res > 0) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP maxConorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 0.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
        } else if (vals[i] > res) {
            res = vals[i];
        }
    }
    if (!naRm[0] & na & res < 1) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP lukConorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 0.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
        } else {
            res += vals[i];
        }
    }
    if (res >= 1) {
        return wrap(1.0);
    } else if (!naRm[0] & na) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP prodConorm(SEXP aVals, SEXP aNaRm)
{
LFL_BEGIN_TRYCATCH
    NumericVector vals = aVals;
    LogicalVector naRm = aNaRm;
    double res = 0.0;
    bool na = false;
    for (int i = 0; i < vals.size(); ++i) {
        testInvalids(vals[i]);
        if (R_IsNA(vals[i])) {
            na = true;
        } else {
            res = res + vals[i] - res * vals[i];
        }
    }
    if (!naRm[0] & na & res < 1) {
        return wrap(NA_REAL);
    }
    return wrap(res);
LFL_END_TRYCATCH
}


RcppExport SEXP goedelImpl(SEXP aX, SEXP aY)
{
LFL_BEGIN_TRYCATCH
    NumericVector x = aX;
    NumericVector y = aY;
    int n = x.size() > y.size() ? x.size() : y.size();
    NumericVector res(n);
    for (int i = 0; i < n; ++i) {
        int xi = i % x.size();
        int yi = i % y.size();
        testInvalids(x[xi]);
        testInvalids(y[yi]);
        if (x[xi] == 0) {
            res[i] = 1;
        } else if (R_IsNA(x[xi]) || R_IsNA(y[yi])) {
            res[i] = NA_REAL;
        } else if (x[xi] <= y[yi]) {
            res[i] = 1;
        } else {
            res[i] = y[yi];
        }
    }
    return res;
LFL_END_TRYCATCH
}


RcppExport SEXP lukasImpl(SEXP aX, SEXP aY)
{
LFL_BEGIN_TRYCATCH
    NumericVector x = aX;
    NumericVector y = aY;
    int n = x.size() > y.size() ? x.size() : y.size();
    NumericVector res(n);
    for (int i = 0; i < n; ++i) {
        int xi = i % x.size();
        int yi = i % y.size();
        testInvalids(x[xi]);
        testInvalids(y[yi]);
        if (x[xi] == 0) {
            res[i] = 1;
        } else if (R_IsNA(x[xi]) || R_IsNA(y[yi])) {
            res[i] = NA_REAL;
        } else if (x[xi] <= y[yi]) {
            res[i] = 1;
        } else {
            res[i] = 1-x[xi] + y[yi];
        }
    }
    return res;
LFL_END_TRYCATCH
}


RcppExport SEXP goguenImpl(SEXP aX, SEXP aY)
{
LFL_BEGIN_TRYCATCH
    NumericVector x = aX;
    NumericVector y = aY;
    int n = x.size() > y.size() ? x.size() : y.size();
    NumericVector res(n);
    for (int i = 0; i < n; ++i) {
        int xi = i % x.size();
        int yi = i % y.size();
        testInvalids(x[xi]);
        testInvalids(y[yi]);
        if (x[xi] == 0) {
            res[i] = 1;
        } else if (R_IsNA(x[xi]) || R_IsNA(y[yi])) {
            res[i] = NA_REAL;
        } else if (x[xi] <= y[yi]) {
            res[i] = 1;
        } else {
            res[i] = y[yi] / x[xi];
        }
    }
    return res;
LFL_END_TRYCATCH
}


RcppExport SEXP involNeg(SEXP aX)
{
LFL_BEGIN_TRYCATCH
    NumericVector x = aX;
    NumericVector res(x.size());
    for (int i = 0; i < x.size(); ++i) {
        testInvalids(x[i]);
        if (R_IsNA(x[i])) {
            res[i] = NA_REAL;
        } else {
            res[i] = 1 - x[i];
        }
    }
    return res;
LFL_END_TRYCATCH
}


RcppExport SEXP strictNeg(SEXP aX)
{
LFL_BEGIN_TRYCATCH
    NumericVector x = aX;
    NumericVector res(x.size());
    for (int i = 0; i < x.size(); ++i) {
        testInvalids(x[i]);
        if (R_IsNA(x[i])) {
            res[i] = NA_REAL;
        } else if (x[i] == 0) {
            res[i] = 1;
        } else {
            res[i] = 0;
        }
    }
    return res;
LFL_END_TRYCATCH
}


