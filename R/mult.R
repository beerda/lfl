#' Callback-based Multiplication of Matrices
#'
#' Perform a custom multiplication of the matrices `x` and `y` by
#' using the callback function `f`.
#'
#' For a matrix `x` of size \eqn{(u,v)} and a matrix `y` of size
#' \eqn{(v,w)}, `mult` calls the function `f` \eqn{uw}-times to
#' create a resulting matrix of size \eqn{(u,w)}.  Each \eqn{(i,j)}-th element
#' of the resulting matrix is obtained from a call of the function `f`
#' with `x`'s \eqn{i}-th row and `y`'s \eqn{j}-th column passed as its arguments.
#'
#' @param x A first matrix. The number of columns must match with the number of
#' rows of the `y` matrix.
#' @param y A second matrix. The number of rows must match with the number of
#' columns of the `x` matrix.
#' @param f A function to be applied to the matrices in order to compute the
#' multiplication.  It must accept at least two arguments.
#' @param ...  Additional arguments that are passed to the function `f`.
#' @return A matrix with \eqn{v} rows and \eqn{w} columns, where \eqn{v} is the
#' number of rows of `x` and \eqn{w} is the number of columns of `y`.
#' @author Michal Burda
#' @seealso [compose()]
#' @keywords models robust multivariate
#' @examples
#'     x <- matrix(runif(24, -100, 100), ncol=6)
#'     y <- matrix(runif(18, -100, 100), nrow=6)
#'
#'     mult(x, y, function(xx, yy) sum(xx * yy)) # the same as "x %*% y"
#'
#' @export mult
mult <- function(x, y, f, ...) {
    .mustBeMatrix(x)
    .mustBeMatrix(y)
    .mustNotBeZeroLength(x)
    .mustNotBeZeroLength(y)
    .mustBe(ncol(x) == nrow(y), "Number of columns of 'x' must equal to the number of rows of 'y'")
    .mustBeFunction(f)
    .mustBe(length(formals(f)) >= 2, "'f' must be a function with at least 2 arguments")

    if (is.null(colnames(x))) {
        colnames(x) <- rownames(y)
    }
    if (is.null(rownames(y))) {
        rownames(y) <- colnames(x)
    }
    if (!is.null(colnames(x))) {
        .mustBe(all(colnames(x) == rownames(y)))
    }

    ff <- f
    if (length(list(...)) > 0) {
        ff <- function(xx, yy) {
            f(xx, yy, ...)
        }
    }
    res <- .Call('_lfl_mult', x, y, ff, PACKAGE='lfl')
    colnames(res) <- colnames(y)
    rownames(res) <- rownames(x)
    res
}
