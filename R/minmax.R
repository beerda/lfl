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
