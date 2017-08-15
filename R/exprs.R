#' Factories for functions that convert numeric data into membership degrees of fuzzy sets
#'
#' These functions create functions with a single argument `x` that compute membership degrees of `x` to a fuzzy set
#' of either triangular or raised-cosine shape that is defined by `lo`, `center`, and `hi`.
#'
#' The arguments must satisfy `lo \le center \le hi`. Functions compute membership degrees of triangular or
#' raised-cosine fuzzy sets. `x` values equal to `center obtain membership degree equal to 1, `x` values lower than `lo`
#' or greater than `hi` obtain memberhsip degree equal to 0. A transition of the triangular (resp. raised cosinal) shape
#' (with peak at `center` is computed for `x` values between `lo` and `hi`.
#'
#' If `lo == -Inf` then any value that is lower or equal to center gets memberhsip degree 1.  Similarly, if `hi == Inf`
#' then any value that is greater or equal to center gets memberhsip degree 1.
#'
#' \code{triangle} produces fuzzy sets of a triangular shape (with peak at \code{center}), \code{raisedcos} produces
#' fuzzy sets defined as a raised cosine hill.
#'
#' @aliases triangle raisedcos
#' @param lo A lower bound (can be -Inf).
#' @param center A peak value.
#' @param hi An upper bound (can be Inf).
#' @return A function with single argument `x` that should be a numeric vector to be converted.
#' @author Michal Burda
#' @seealso \code{\link{fcut}}
#' @keywords models robust multivariate
#' @examples
#'
#' tr <- triangle(1, 2, 3)
#' tr(1:30 / 3)
#'
#' rc <- raisedcos(1, 2, 3)
#' rc(1:30 / 3)
#'
#' plot(triangle(-1, 0, 1), from=-2, to=3)
#' plot(triangle(-1, 0, 2), from=-2, to=3)
#' plot(triangle(-Inf, 0, 1), from=-2, to=3)
#' plot(triangle(-1, 0, Inf), from=-2, to=3)
#'
#' plot(raisedcos(-1, 0, 1), from=-2, to=3)
#' plot(raisedcos(-1, 0, 2), from=-2, to=3)
#' plot(raisedcos(-Inf, 0, 1), from=-2, to=3)
#' plot(raisedcos(-1, 0, Inf), from=-2, to=3)
#'
#' @export
triangle <- function(lo, center, hi) {
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

#' @rdname triangle
#' @export
raisedcos <- function(lo, center, hi) {
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
