#' Return equifrequent breaks
#'
#' If both `left` and `right` equal to `"none"`, the function returns a vector of `n` values from `x`
#' that divide the range of values in `x` into `n - 1` equidistant intervals. If the `left` (resp. `right`)
#' argument equals to `"infinity"`, `-Inf` (resp. `Inf`) is prepended (resp. appended) to the result. If
#' it equals to `"same"`, the first (resp. last) value is doubled. See [fcut()] for what such vectors
#' mean.
#'
#' @return A vector of equifrequent breaks
#' @seealso [equidist()], [fcut()]
#' @author Michal Burda
#' @export
#' @name equifreq
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
