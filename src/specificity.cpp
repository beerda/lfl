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
