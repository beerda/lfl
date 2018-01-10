#' Compute Root Mean Squared Error (RMSE)
#'
#' RMSE is computed as \eqn{sqrt(mean((forecast - validation)^2))}.
#'
#' @param forecast A numeric vector of forecasted values
#' @param validation A numeric vector of actual (real) values
#' @return A Root Mean Squared Error (RMSE)
#' @author Michal Burda
#' @seealso [smape()], [mase()], [frbe()]
#' @export
rmse <- function(forecast, validation) {
  .mustBeNumericVector(forecast)
  .mustBeNumericVector(validation)
  .mustNotHaveNA(forecast)
  .mustNotHaveNA(validation)
  .mustBe(length(forecast) == length(validation), "Length of 'forecast' and 'validation' must be equal")
  .mustBe(length(forecast) > 1, "Length of both 'forecast' and 'validation' must be greater than zero")

  return(sqrt(mean((forecast - validation)^2)))
}
