#' Return equifrequent breaks
#'
#' If both `left` and `right` equal to `"none"`, the function returns a vector of `n` values from `x`
#' that divide the range of values in `x` into `n - 1` equidistant intervals. If the `left` (resp. `right`)
#' argument equals to `"infinity"`, `-Inf` (resp. `Inf`) is prepended (resp. appended) to the result. If
#' it equals to `"same"`, the first (resp. last) value is doubled. See [fcut()] for what such vectors
#' mean.
#'
#' If the `left` (resp. `right`) argument equals to `"infinity"`, `-Inf` (resp. `Inf`) is prepended
#' (resp. appended) to the result. If it equals to `"same"`, the first (resp. last) value is doubled.
#' Such functionality is beneficial if using the result of this function with e.g. the [fcut()] function:
#' `Inf` values at the beginning (resp. at the end) of the vector of breaks means that the fuzzy set
#' partition starts with a fuzzy set with kernel going to negative (resp. positive) infinity; the doubled
#' value at the beginning (resp. end) results in half-cut (trimmed) fuzzy set.
#'
#' @param x A numeric vector of input values
#' @param n The number of breaks of `x` to find (`n` must be at least 2)
#' @param left The left border of the returned vector of breaks: `"infinity"`, `"same"` or `"none"`
#'     (see the description below)
#' @param right The right border of the returned vector of breaks: `"infinity"`, `"same"` or `"none"`
#'     (see the description below)
#' @return A vector of equifrequent breaks
#' @seealso [equidist()], [fcut()]
#' @author Michal Burda
#' @keywords models robust
#' @export
equifreq <- function(x,
                     n,
                     left = c('infinity', 'same', 'none'),
                     right = c('infinity', 'same', 'none')) {
    left <- match.arg(left)
    right <- match.arg(right)

    .mustBeNumericVector(x)
    .mustBeNumericScalar(n)
    .mustBe(n >= 2, "'n' must be greater or equal to 2")
    .mustBeCharacterScalar(left)
    .mustBeCharacterScalar(right)

    i <- seq(from = 1,
             to = length(x),
             length.out = n)
    i <- round(i)
    res <- sort(x)[i]

    if (left == 'infinity') {
        res <- c(-Inf, res)
    } else if (left == 'same') {
        res <- c(res[1], res)
    }

    if (right == 'infinity') {
        res <- c(res, Inf)
    } else if (right == 'same') {
        res <- c(res, res[length(res)])
    }

    res
}
