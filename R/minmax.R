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
        dots[['low']] <- mini
        dots[['high']] <- maxi
    } else {
        dots[['negMax']] <- mini
        dots[['max']] <- maxi
    }
    do.call(type, dots)
}
