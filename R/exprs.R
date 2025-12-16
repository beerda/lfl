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


#' Factories for functions that convert numeric data into membership degrees of fuzzy sets
#'
#' These functions create functions with a single argument `x` that compute membership degrees of `x` to a fuzzy set
#' of either triangular or raised-cosine shape that is defined by `lo`, `center`, and `hi`.
#'
#' The arguments must satisfy `lo <= center <= hi`. Functions compute membership degrees of triangular or
#' raised-cosine fuzzy sets. `x` values equal to `center obtain membership degree equal to 1, `x` values lower than `lo`
#' or greater than `hi` obtain membership degree equal to 0. A transition of the triangular (resp. raised cosine) shape
#' (with peak at `center` is computed for `x` values between `lo` and `hi`.
#'
#' If `lo == -Inf` then any value that is lower or equal to center gets membership degree 1.  Similarly, if `hi == Inf`
#' then any value that is greater or equal to center gets membership degree 1. `NA` and `NaN` values remain unchanged.
#'
#' `triangular()` produces fuzzy sets of a triangular shape (with peak at `center`), `raisedcosine()` produces
#' fuzzy sets defined as a raised cosine hill.
#'
#' @aliases triangular raisedcosine
#' @param lo A lower bound (can be -Inf).
#' @param center A peak value.
#' @param hi An upper bound (can be Inf).
#' @return A function with single argument `x` that should be a numeric vector to be converted.
#' @author Michal Burda
#' @seealso [fcut()]
#' @keywords models robust multivariate
#' @examples
#'
#' tr <- triangular(1, 2, 3)
#' tr(1:30 / 3)
#'
#' rc <- raisedcosine(1, 2, 3)
#' rc(1:30 / 3)
#'
#' plot(triangular(-1, 0, 1), from=-2, to=3)
#' plot(triangular(-1, 0, 2), from=-2, to=3)
#' plot(triangular(-Inf, 0, 1), from=-2, to=3)
#' plot(triangular(-1, 0, Inf), from=-2, to=3)
#'
#' plot(raisedcosine(-1, 0, 1), from=-2, to=3)
#' plot(raisedcosine(-1, 0, 2), from=-2, to=3)
#' plot(raisedcosine(-Inf, 0, 1), from=-2, to=3)
#' plot(raisedcosine(-1, 0, Inf), from=-2, to=3)
#'
#' @export
triangular <- function(lo, center, hi) {
    .mustBeNumericScalar(lo)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(hi)

    if (lo > center || lo > hi) {
        .stop('"lo" must be the lower-bound of the interval <lo, hi>')
    }
    if (hi < center || hi < lo) {
        .stop('"hi" must be the upper-bound of the interval <lo, hi>')
    }
    if (center < lo || center > hi) {
        .stop('"center" must be within the interval <lo, hi>')
    }

    ctx <- as.numeric(c(lo, center, hi))
    return(function(x) {
        .Call('_lfl_triangle', as.numeric(as.vector(x)), ctx, PACKAGE='lfl')
    })
}

#' @rdname triangular
#' @export
raisedcosine <- function(lo, center, hi) {
    .mustBeNumericScalar(lo)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(hi)

    if (lo > center || lo > hi) {
        .stop('"lo" must be the lower-bound of the interval <lo, hi>')
    }
    if (hi < center || hi < lo) {
        .stop('"hi" must be the upper-bound of the interval <lo, hi>')
    }
    if (center < lo || center > hi) {
        .stop('"center" must be within the interval <lo, hi>')
    }

    ctx <- as.numeric(c(lo, center, hi))
    return(function(x) {
        .Call('_lfl_raisedcos', as.numeric(as.vector(x)), ctx, PACKAGE='lfl')
    })
}
