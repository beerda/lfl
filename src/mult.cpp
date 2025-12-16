/**********************************************************************
 * lfl: Linguistic Fuzzy Logic
 * Copyright (C) 2025 Michal Burda
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 **********************************************************************/


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

