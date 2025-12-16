#######################################################################
# lfl: Linguistic Fuzzy Logic
# Copyright (C) 2025 Michal Burda
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.
#######################################################################


#' lfl - Linguistic Fuzzy Logic
#'
#' Various algorithms related (not only) to linguistic fuzzy logic: mining for linguistic fuzzy association
#' rules, composition of fuzzy relations, performing perception-based logical deduction (PbLD),
#' and forecasting time-series using fuzzy rule-based ensemble (FRBE).
#'
#' Other methods include fuzzy transform and computation of Sugeno integrals. Also, basic algebraic
#' functions related to fuzzy logic are included, which allow handling missing values using different
#' styles such as Kleene, Bochvar, Sobocinski and others.
#'

#' @keywords internal
"_PACKAGE"

#' @importFrom e1071 skewness
#' @importFrom e1071 kurtosis
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom forecast auto.arima
#' @importFrom forecast ets
#' @importFrom forecast rwf
#' @importFrom forecast thetaf
#' @importFrom forecast forecast
#' @importFrom plyr colwise
#' @importFrom plyr laply
#' @importFrom Rcpp evalCpp
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats frequency
#' @importFrom stats lm
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats ts
#' @importFrom stats ts.plot
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom tseries adf.test
#' @importFrom utils str
#' @useDynLib lfl, .registration = TRUE
#'
NULL
