#' lfl - Linguistic Fuzzy Logic
#'
#' Various algorithms related (not only) to linguistic fuzzy logic: mining for linguistic fuzzy association
#' rules, composition of fuzzy relations, performing perception-based logical deduction (PbLD),
#' and forecasting time-series using fuzzy rule-based ensemble (FRBE).
#'
#' Other methods include fuzzy transform and computation of Sugeno integrals. Also basic algebraic
#' functions related to fuzzy logic are contained, which allow to handle missing values using different
#' styles such as Kleene, Bochvar, Sobocinski and other.
#'

#' @keywords internal
"_PACKAGE"

#' @importFrom e1071 skewness
#' @importFrom e1071 kurtosis
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom forecast auto.arima
#' @importFrom forecast ets
#' @importFrom forecast rwf
#' @importFrom forecast thetaf
#' @importFrom forecast forecast
#' @importFrom plyr colwise
#' @importFrom plyr laply
#' @importFrom Rcpp evalCpp
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats frequency
#' @importFrom stats lm
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats ts
#' @importFrom stats ts.plot
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom tseries adf.test
#' @importFrom utils str
#' @useDynLib lfl, .registration = TRUE
#'
NULL
