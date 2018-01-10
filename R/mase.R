#' Compute Mean Absolute Scaled Error (MASE)
#'
#' MASE is computed as \eqn{sum(abs(validation - forecast)) / sum(abs(validation[-1] - validation[-n])) / (n/(n-1))}.
#' @param forecast A numeric vector of forecasted values
#' @param validation A numeric vector of actual (real) values
#' @return A Mean Absolute Scaled Error (MASE)
#' @author Michal Burda
#' @seealso [rmse()], [smape()], [frbe()]
#' @export
mase <- function(forecast, validation) {
  .mustBeNumericVector(forecast)
  .mustBeNumericVector(validation)
  .mustNotHaveNA(forecast)
  .mustNotHaveNA(validation)
  .mustBe(length(forecast) == length(validation), "Length of 'forecast' and 'validation' must be equal")
  .mustBe(length(forecast) > 1, "Length of both 'forecast' and 'validation' must be greater than zero")

  n <- length(validation)

  #mase <- mean(abs(validation - forecast)) / mean(abs(history[-length(history)] - history[-1]))
  mase <- sum(abs(validation - forecast)) / sum(abs(validation[-1] - validation[-n])) / (n/(n-1))

  return(mase)
}
