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


#' Test whether `x` is a valid object of the S3 `frbe` class
#'
#' Test whether `x` has a valid format for the objects of the S3 `frbe` class.
#'
#' This function tests whether `x` inherits from `frbe` i.e. whether
#' it is a list with the following elements: `forecasts` data frame,
#' `features` data frame, `weights` vector, and `mean` vector.
#' Instances of the S3 class `frbe` are usually created by the [frbe()] function.
#'
#' @param x An object being tested.
#' @return `TRUE` if `x` is a valid `frbe` object and `FALSE` otherwise.
#' @author Michal Burda
#' @seealso [frbe()]
#' @references Štěpnička, M., Burda, M., Štěpničková, L. Fuzzy Rule Base
#' Ensemble Generated from Data by Linguistic Associations Mining. FUZZY SET
#' SYST. 2015.
#' @keywords models robust
#' @export
is.frbe <- function(x) {
    return(inherits(x, 'frbe') &&
           is.list(x) &&
           is.data.frame(x$forecasts) &&
           is.data.frame(x$features) &&
           is.vector(x$weights) &&
           is.vector(x$mean))
}
