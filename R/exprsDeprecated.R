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


#' Deprecated functions to compute membership degrees of numeric fuzzy sets
#'
#' These functions compute membership degrees of numeric fuzzy sets with
#' triangular or raised-cosine shape. These functions are *deprecated*.
#' Please use [triangular()] or [raisedcosine()] functions instead.
#'
#' @param x A numeric vector to be transformed.
#' @param lo A lower bound (can be -Inf).
#' @param center A peak value.
#' @param hi An upper bound (can be Inf).
#' @return A numeric vector of membership degrees of `x` to a fuzzy set with the shape
#' determined with `lo`, `center`, `hi`.
#' @seealso [triangular()], [raisedcosine()]
#' @author Michal Burda
#' @export
triangle <- function(x, lo, center, hi) {
    .Deprecated('triangular')
    if (lo > center || lo > hi) {
        stop('"lo" must be the lower-bound of the interval <lo, hi>')
    }
    if (hi < center || hi < lo) {
        stop('"hi" must be the upper-bound of the interval <lo, hi>')
    }
    if (center < lo || center > hi) {
        stop('"center" must be within the interval <lo, hi>')
    }

    return(sapply(x, .singleTriangle, lo, center, hi))
}


#' @rdname triangle
#' @export
raisedcos <- function(x, lo, center, hi) {
    .Deprecated('raisedcosine')
    if (lo > center || lo > hi) {
        stop('"lo" must be the lower-bound of the interval <lo, hi>')
    }
    if (hi < center || hi < lo) {
        stop('"hi" must be the upper-bound of the interval <lo, hi>')
    }
    if (center < lo || center > hi) {
        stop('"center" must be within the interval <lo, hi>')
    }

    return(sapply(x, .singleRaisedcos, lo, center, hi))
}


.singleTriangle <- function(x, lo, center, hi) {
    if (x < center) {
        if (lo == -Inf) {
            return(1)
        }
        return(pmax(0, (x - lo) / (center - lo)))
    } else if (x == center) {
        return(1)
    } else {
        if (hi == Inf) {
            return(1)
        }
        return(pmax(0, (hi - x) / (hi - center)))
    }
}


.singleRaisedcos <- function(x, lo, center, hi) {
    if (x < lo || x > hi) {
        return(0)
    } else if (x < center) {
        if (lo == -Inf) {
            return(1)
        }
        return((cos((x - center) * pi / (center - lo)) + 1) / 2)
    } else if (x == center) {
        return(1)
    } else {
        if (hi == Inf) {
            return(1)
        }
        return((cos((x - center) * pi / (hi - center)) + 1) / 2)
    }
}
