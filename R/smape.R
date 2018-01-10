#' Compute Symmetric Mean Absolute Percentage Error (SMAPE)
#'
#' SMAPE is computed as \eqn{mean(abs(forecast - validation) / ((abs(forecast) + abs(validation)) / 2))}.
#'
#' @param forecast A numeric vector of forecasted values
#' @param validation A numeric vector of actual (real) values
#' @return A Symmetric Mean Absolute Percentage Error (SMAPE)
#' @author Michal Burda
#' @seealso [rmse()], [mase()], [frbe()]
#' @export
smape <- function(forecast, validation) {
  .mustBeNumericVector(forecast)
  .mustBeNumericVector(validation)
  .mustNotHaveNA(forecast)
  .mustNotHaveNA(validation)
  .mustBe(length(forecast) == length(validation), "Length of 'forecast' and 'validation' must be equal")
  .mustBe(length(forecast) > 1, "Length of both 'forecast' and 'validation' must be greater than zero")

  return(mean(abs(forecast - validation) / ((abs(forecast) + abs(validation)) / 2)))
}
