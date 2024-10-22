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

#' @useDynLib lfl, .registration = TRUE
#' @importFrom Rcpp evalCpp
#'
NULL
