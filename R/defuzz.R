#' Convert fuzzy set into a crisp numeric value
#'
#' Take a discretized fuzzy set (i.e. a vector of membership degrees and a vector
#' of numeric values that correspond to that degrees) and perform a selected
#' type of defuzzification, i.e. conversion of the fuzzy set into a single
#' crisp value.
#'
#' Function converts input fuzzy set into a crisp value. The definition of
#' input fuzzy set is provided by the arguments `degrees` and
#' `values`. These arguments should be numeric vectors of the same length,
#' the former containing membership degrees in the interval \eqn{[0, 1]} and
#' the latter containing the corresponding crisp values: i.e., `values[i]`
#' has a membership degree `degrees[i]`.
#'
#' @param degrees A fuzzy set in the form of a numeric vector of membership
#' degrees of values provided as the `values` argument.
#' @param values A universe for the fuzzy set.
#' @param type Type of the requested defuzzification method. The possibilities are:
#' * `'mom'`: Mean of Maxima - maximum membership degrees are
#'   found and a mean of values that correspond to that degrees is returned;
#' * `'fom'`: First of Maxima - first value with maximum membership
#'   degree is returned;
#' * `'lom'`: Last of Maxima - last value with maximum membership degree
#'    is returned;
#' * `'dee'`: Defuzzification of Evaluative Expressions - method used
#'   by the [pbld()] inference mechanism that combines the former three
#'   approaches accordingly to the shape of the `degrees` vector:
#'   If `degrees` is non-increasing then `'lom'` type is used,
#'   if it is non-decreasing then `'fom'` is applied, else `'mom'` is selected;
#' * `'cog'`: Center of Gravity - the result is a mean of `values` weighted by `degrees`;
#' * `'exp1'`: Experimental 1.
#' @return A defuzzified value.
#' @author Michal Burda
#' @seealso [fire()], [aggregateConsequents()], [perceive()], [pbld()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#'
#' # returns mean of maxima, i.e., mean of 6, 7, 8
#' defuzz(c(0, 0, 0, 0.1, 0.3, 0.9, 0.9, 0.9, 0.2, 0),
#'        1:10,
#'        type='mom')
#'
#' @export defuzz
defuzz <- function(degrees,
                   values,
                   type=c('mom', 'fom', 'lom', 'dee', 'cog', 'expun', 'expw1', 'expw2')) {
    .mustBeNumericVector(degrees)
    .mustBeNumericVector(values)
    .mustBe(length(degrees) == length(values), "The length of 'degrees' and 'values' must be the same")
    .mustBe(length(degrees) >= 3, "The length of 'degrees' must be at least 3")
    .mustBe(min(degrees) >= 0 && max(degrees) <= 1, "Values of 'degrees' must be truth values in the interval [0,1]")

    type <- match.arg(type)

    if (type == 'cog') {
        return(weighted.mean(values, degrees))

    } else if (type %in% c('expun', 'expw1', 'expw2')) {
        alpha <- unique(degrees)
        alpha <- setdiff(alpha, 0)
        alpha <- sort(alpha)
        center <- sapply(alpha, function(a) mean(values[degrees >= a]))
        p <- seq(1, length(alpha) - 1)
        n <- seq(2, length(alpha))
        alphadiff <- alpha[n] - alpha[p]
        mins <- pmin(center[p], center[n])
        maxs <- pmax(center[p], center[n])

        if (type == 'expw1') {
            weight <- alphadiff * alpha[n]
        } else  if (type == 'expw2') {
            weight <- alphadiff * (alpha[n])^2
        } else {
            weight <- alphadiff
        }

        return(weighted.mean((mins + maxs) / 2, weight))

    } else {
        o <- order(values)
        values <- values[o]
        degrees <- degrees[o]
        i <- which(degrees == max(degrees))

        if (type == 'dee') {
            l <- length(degrees)
            diff <- degrees[-1] - degrees[-l]
            if (all(diff >= 0)) {
                type <- 'fom'
            } else if (all(diff <= 0)) {
                type <- 'lom'
            } else {
                type <- 'mom'
            }
        }

        if (type == 'fom') {
            return(values[min(i)])

        } else if (type == 'lom') {
            return(values[max(i)])

        } else if (type == 'mom') {
            return(mean(values[i]))

        } else {
            stop("Unknown defuzzification type")
        }
    }
}
