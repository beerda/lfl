#' Return equidistant breaks
#'
#' If both `left` and `right` equal to `"none"`, the function returns a vector of `n` values from `x`
#' that divide the range of values in `x` into `n - 1` equidistant intervals. If the `left` (resp. `right`)
#' argument equals to `"infinity"`, `-Inf` (resp. `Inf`) is prepended (resp. appended) to the result. If
#' it equals to `"same"`, the first (resp. last) value is doubled.
#'
#' @return A vector of equidistant breaks
#' @seealso [fcut()]
#' @author Michal Burda
#' @export
#' @name equidist
equidist <- function(x,
                     n,
                     left = c('infinity', 'same', 'none'),
                     right = c('infinity', 'same', 'none')) {
    .mustBeNumericVector(x)
    .mustBeNumericScalar(n)
    .mustBe(n >= 2, "'n' must be greater or equal to 2")
    .mustBeCharacterScalar(left)
    .mustBeCharacterScalar(right)

    left <- match.arg(left)
    right <- match.arg(right)

    res <- seq(from = min(x, na.rm = TRUE),
               to = max(x, na.rm = TRUE),
               length.out = n)

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

