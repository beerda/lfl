#' Creating linguistic context directly from values
#'
#' This function creates a context (i.e. an instance of S3 class
#' [ctx3()], [ctx3bilat()], [ctx5()], or [ctx5bilat()]) based on values
#' of the numeric vector `x`. In default, the context is based on minimum
#' and maximum value of `x` in the following way:
#' * `ctx3`, `ctx5`: low = minimum, high = maximum value of `x`;
#' * `ctx3bilat`, `ctx5bilat`: negMax = minimum, max = maximum value of `x`,
#'    origin = mean of minimum and maximum.
#'
#' Other values are computed accordingly to defaults as defined in the constructors
#' [ctx3()], [ctx3bilat()], [ctx5()], and [ctx5bilat()]).
#'
#' @param x A numeric vector to compute the context from
#' @param type A type of the context to be returned. Must be one of:
#'   `ctx3`, `ctx5`, `ctx3bilat` or `ctx5bilat`
#' @param ... other parameters to be passed to the appropriate constructor
#'   ([ctx3()], [ctx3bilat()], [ctx5()], and [ctx5bilat()]) that is called internally.
#'   These values overwrite the defaults computed by `minmax` -- see the examples.
#' @examples
#'   minmax(0:100)                # returns ctx3: 0, 50, 100
#'   minmax(0:100, high=80)       # returns ctx3: 0, 40, 80
#'   minmax(0:100, relCenter=0.4) # returns ctx3: 0, 40, 100
#'   minmax(0:100, type='ctx5')   # returns ctx5: 0, 25, 50, 75, 100
#' @export
minmax <- function(x,
                   type=c('ctx3', 'ctx5', 'ctx3bilat', 'ctx5bilat'),
                   ...) {
    .mustBeNumericVector(x)
    type <- match.arg(type)
    dots <- list(...)
    mini <- min(x, na.rm=TRUE)
    maxi <- max(x, na.rm=TRUE)
    if (type %in% c('ctx3', 'ctx5')) {
        if (is.null(dots[['low']])) {
            dots[['low']] <- mini
        }
        if (is.null(dots[['high']])) {
            dots[['high']] <- maxi
        }
    } else {
        if (is.null(dots[['negMax']])) {
            dots[['negMax']] <- mini
        }
        if (is.null(dots[['max']])) {
            dots[['max']] <- maxi
        }
        if (is.null(dots[['origin']])) {
            dots[['origin']] <- mean(c(mini, maxi))
        }
    }
    do.call(type, dots)
}
