#.u2 <- function(predicted, real, rw) {
    #sqrt(sum((real - predicted)^2) / length(real)) / sqrt(sum((real - rw)^2) / length(real))
#}




#' Evaluate the performance of the FRBE forecast
#'
#' Take a FRBE forecast and compare it with real values using arbitrary error
#' function.
#'
#' Take a FRBE forecast and compare it with real values by evaluating a given
#' error measure.  FRBE forecast should be made for a horizon of the same value
#' as length of the vector of real values.
#'
#' @param fit A FRBE model of class `frbe` as returned by the [frbe()] function.
#' @param real A numeric vector of real (known) values. The vector must
#' correspond to the values being forecasted, i.e. the length must be the same
#' as the horizon forecasted by [frbe()].
#' @param error Error measure to be computed. It can be either Symmetric Mean
#' Absolute Percentage Error (`smape`), Mean Absolute Scaled Error (`mase`), or
#' Root Mean Squared Error (`rmse`). See also [smape()], [mase()], and [rmse()]
#' for more details.
#' @return Function returns a data.frame with single row and columns
#' corresponding to the error of the individual forecasting methods that the
#' FRBE is computed from. Additionally to this, a column "avg" is added with
#' error of simple average of the individual forecasting methods and a column
#' "frbe" with error of the FRBE forecasts.
#' @author Michal Burda
#' @seealso [frbe()], [smape()], [mase()], [rmse()]
#' @references Štěpnička, M., Burda, M., Štěpničková, L. Fuzzy Rule Base
#' Ensemble Generated from Data by Linguistic Associations Mining. FUZZY SET
#' SYST. 2015.
#' @keywords models robust
#' @examples
#'
#'   # prepare data (from the forecast package)
#'   library(forecast)
#'   horizon <- 10
#'   train <- wineind[-1 * (length(wineind)-horizon+1):length(wineind)]
#'   test <- wineind[(length(wineind)-horizon+1):length(wineind)]
#'   f <- frbe(ts(train, frequency=frequency(wineind)), h=horizon)
#'   evalfrbe(f, test)
#'
#' @export
#' @importFrom plyr colwise
evalfrbe <- function(fit,
                     real,
                     error=c('smape', 'mase', 'rmse')) {
    .mustBe(is.frbe(fit), "'fit' must be an instance of class 'frbe'")
    .mustBeNumericVector(real)

    error <- match.arg(error)
    if (error == 'smape') {
        errorFunc <- smape
    } else if (error == 'mase') {
        errorFunc <- mase
    } else if (error == 'rmse') {
        errorFunc <- rmse
    } else {
        stop("Unknown error function name")
    }

    d <- fit$forecasts
    d$avg <- rowSums(d) / ncol(d)
    d$frbe <- fit$mean

    if (length(real) > nrow(d)) {
        length(real) <- nrow(d)
    } else {
        d <- d[seq_along(real), ]
    }

    r <- colwise(function(col) { errorFunc(col, real) })(d)
    #u2 <- colwise(function(col) { .u2(col, real, fit$forecasts$randomWalk) })(d)
    #names(u2) <- paste('u2', names(u2))
    #return(cbind(r, u2))
    return(r)
}
