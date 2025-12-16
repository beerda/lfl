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


#' Compute Mean Absolute Scaled Error (MASE)
#'
#' MASE is computed as \eqn{sum(abs(validation - forecast)) / sum(abs(validation[-1] - validation[-n])) / (n/(n-1))}.
#' @param forecast A numeric vector of forecasted values
#' @param validation A numeric vector of actual (real) values
#' @return A Mean Absolute Scaled Error (MASE)
#' @author Michal Burda
#' @seealso [rmse()], [smape()], [frbe()]
#' @export
mase <- function(forecast, validation) {
  .mustBeNumericVector(forecast)
  .mustBeNumericVector(validation)
  .mustNotHaveNA(forecast)
  .mustNotHaveNA(validation)
  .mustBe(length(forecast) == length(validation), "Length of 'forecast' and 'validation' must be equal")
  .mustBe(length(forecast) > 1, "Length of both 'forecast' and 'validation' must be greater than zero")

  n <- length(validation)

  #mase <- mean(abs(validation - forecast)) / mean(abs(history[-length(history)] - history[-1]))
  mase <- sum(abs(validation - forecast)) / sum(abs(validation[-1] - validation[-n])) / (n/(n-1))

  return(mase)
}
