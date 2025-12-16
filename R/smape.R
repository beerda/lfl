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


#' Compute Symmetric Mean Absolute Percentage Error (SMAPE)
#'
#' SMAPE is computed as \eqn{mean(abs(forecast - validation) / ((abs(forecast) + abs(validation)) / 2))}.
#'
#' @param forecast A numeric vector of forecasted values
#' @param validation A numeric vector of actual (real) values
#' @return A Symmetric Mean Absolute Percentage Error (SMAPE)
#' @author Michal Burda
#' @seealso [rmse()], [mase()], [frbe()]
#' @export
smape <- function(forecast, validation) {
  .mustBeNumericVector(forecast)
  .mustBeNumericVector(validation)
  .mustNotHaveNA(forecast)
  .mustNotHaveNA(validation)
  .mustBe(length(forecast) == length(validation), "Length of 'forecast' and 'validation' must be equal")
  .mustBe(length(forecast) > 1, "Length of both 'forecast' and 'validation' must be greater than zero")

  return(mean(abs(forecast - validation) / ((abs(forecast) + abs(validation)) / 2)))
}
