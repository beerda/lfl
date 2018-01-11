#' Deprecated functions to compute membership degrees of numeric fuzzy sets
#'
#' These functions compute membership degrees of numeric fuzzy sets with
#' triangular or raised-cosinal shape. These functions are *deprecated*.
#' Please use [triangular()] or [raisedcosinal()] functions instead.
#'
#' @param x A numeric vector to be transformed.
#' @param lo A lower bound (can be -Inf).
#' @param center A peak value.
#' @param hi An upper bound (can be Inf).
#' @return A numeric vector of membership degrees of `x` to a fuzzy set with the shape
#' determined with `lo`, `center`, `hi`.
#' @seealso [triangular()], [raisedcosinal()]
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
    .Deprecated('raisedcosinal')
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
