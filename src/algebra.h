/*
 * File name: algebra.h
 * Date:      2016/01/29 10:50
 * Author:    Michal Burda
 */


#ifndef __LFL__ALGEBRA_H__
#define __LFL__ALGEBRA_H__


#include <common.h>
#include "Rcpp.h"


RcppExport SEXP minNorm(SEXP aVal, SEXP aNaRm);
RcppExport SEXP lukNorm(SEXP aVal, SEXP aNaRm);
RcppExport SEXP prodNorm(SEXP aVal, SEXP aNaRm);

RcppExport SEXP maxConorm(SEXP aVal, SEXP aNaRm);
RcppExport SEXP lukConorm(SEXP aVal, SEXP aNaRm);
RcppExport SEXP prodConorm(SEXP aVal, SEXP aNaRm);

RcppExport SEXP goedelImpl(SEXP aX, SEXP aY);
RcppExport SEXP lukasImpl(SEXP aX, SEXP aY);
RcppExport SEXP goguenImpl(SEXP aX, SEXP aY);

RcppExport SEXP involNeg(SEXP aX);
RcppExport SEXP strictNeg(SEXP aX);
#endif
