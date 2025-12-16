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
