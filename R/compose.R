#' Composition of Fuzzy Relations
#'
#' Composition of Fuzzy Relations
#'
#' Function composes a fuzzy relation `x` (i.e. a numeric matrix of size
#' \eqn{(u,v)}) with a fuzzy relation `y` (i.e. a numeric matrix of size
#' \eqn{(v,w)}) and possibly with the deprecated use of an exclusion fuzzy relation
#' `e` (i.e. a numeric matrix of size \eqn{(v,w)}).
#'
#' The style of composition is determined by the algebra `alg`, the
#' composition type `type`, and possibly also by a deprecated `quantifier`.
#'
#' This function performs four main composition types, the basic composition (
#' also known as direct product), the Bandler-Kohout subproduct (also subdirect
#' product), the Bandler-Kohout superproduct (also supdirect product), and finally,
#' the Bandler-Kohout square product. More complicated composition operations
#' may be performed by using the [mult()] function and/or by combining multiple
#' composition results with the [algebra()] operations.
#'
#' @param x A first fuzzy relation to be composed. It must be a numeric matrix
#' with values within the \eqn{[0,1]} interval. The number of columns must
#' match with the number of rows of the `y` matrix.
#' @param y A second fuzzy relation to be composed. It must be a numeric matrix
#' with values within the \eqn{[0,1]} interval. The number of columns must
#' match with the number of rows of the `x` matrix.
#' @param e Deprecated. An excluding fuzzy relation. If not NULL,
#' it must be a numeric matrix with dimensions equal to the `y` matrix.
#' @param alg An algebra to be used for composition. It must be one of
#' `'goedel'` (default), `'goguen'`, or `'lukasiewicz'`, or an instance of class `algebra`
#' (see [algebra()]).
#' @param type A type of a composition to be performed. It must be one of
#' `'basic'` (default), `'sub'`, `'super'`, or `'square'`.
#' @param quantifier Deprecated. If not NULL, it must be a function taking a single
#' argument, a vector of relative cardinalities, that would be translated into
#' membership degrees. A result of the [lingexpr()] function is a
#' good candidate for that. Note that the vector of relative cardinalities contains also
#' two attributes, `x` and `y`, which carry the original `R`'s data row (in `x`) and `S`'s
#' feature column (in `y`). These attributes are accessible using the standard [base::attr()]
#' function. Find examples below that define some quantifiers.
#' @param sorting Deprecated. Sorting function used within quantifier application. The given function
#' must sort the membership degrees and allow the `decreasing` argument as in [base::sort()].
#' This function have to be explicitly specified typically if performing compositions that
#' handle `NA` values.
#' @return A matrix with \eqn{v} rows and \eqn{w} columns, where \eqn{v} is the
#' number of rows of `x` and \eqn{w} is the number of columns of `y`.
#' @author Michal Burda
#' @seealso [algebra(), [mult()], [lingexpr()]
#' @keywords models robust multivariate
#' @examples
#'     R <- matrix(c(0.1, 0.6, 1, 0, 0, 0,
#'                   0, 0.3, 0.7, 0.9, 1, 1,
#'                   0, 0, 0.6, 0.8, 1, 0,
#'                   0, 1, 0.5, 0, 0, 0,
#'                   0, 0, 1, 1, 0, 0), byrow=TRUE, nrow=5)
#'
#'     S <- matrix(c(0.9, 1, 0.9, 1,
#'                   1, 1, 1, 1,
#'                   0.1, 0.2, 0, 0.2,
#'                   0, 0, 0, 0,
#'                   0.7, 0.6, 0.5, 0.4,
#'                   1, 0.9, 0.7, 0.6), byrow=TRUE, nrow=6)
#'
#'     RS <- matrix(c(0.6, 0.6, 0.6, 0.6,
#'                    1, 0.9, 0.7, 0.6,
#'                    0.7, 0.6, 0.5, 0.4,
#'                    1, 1, 1, 1,
#'                    0.1, 0.2, 0, 0.2), byrow=TRUE, nrow=5)
#'
#'     compose(R, S, alg='goedel', type='basic') # should be equal to RS
#' @export compose
compose <- function(x,
                    y,
                    e=NULL,
                    alg=c('goedel', 'goguen', 'lukasiewicz'),
                    type=c('basic', 'sub', 'super', 'square'),
                    quantifier=NULL,
                    sorting=sort) {

    .mustBeNumericMatrix(x)
    .mustBeNumericMatrix(y)
    .mustBe(nrow(x) > 0, "'x' must be a matrix with at least 1 row")
    .mustBe(ncol(y) > 0, "'y' must be a matrix with at least 1 column")
    .mustBe(ncol(x) == nrow(y), "The number of columns of 'x' must be equal to the number of rows of 'y'")

    if (!is.null(e)) {
        .Deprecated('mult',
                    msg=paste('The "e" argument is deprecated. Complex compositions',
                                'have to be computed by using the "compose()" or "mult()"',
                                'functions and combined with algebra() operations.'))
        .mustBeNumericMatrix(e)
        .mustBe(nrow(y) == nrow(e) && ncol(y) == ncol(e), "'e' must have the same dimensions as 'y'")

    }

    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    if (is.character(type)) {
        type <- match.arg(type)
        if (type == 'basic') {
            type <- alg$pt
            merge <- alg$s
        } else if (type == 'sub') {
            type <- alg$r
            merge <- alg$i
        } else if (type == 'super') {
            type <- function(x, y) { alg$r(y, x) }
            merge <- alg$i
        } else if (type == 'square') {
            type <- alg$b
            merge <- alg$i
        } else {
            stop('Unrecognized composition type')
        }
    } else if (is.function(type)) {
        .mustBe(is.function(quantifier), "if 'type' is a function then 'quantifier' must be a function too")
    }

    .mustBe(is.function(type), "'type' must be either one of 'basic', 'sub', 'super', 'square', or a function with 1 argument")

    f <- NULL
    # TODO: the 'quantifier' argument is not a real quantifier! It is more a linguistic expression giving degrees to relative
    # cardinalitites. The quantifiers are tightly related to algebras, e.g. the 'sort' function is a property of algebra (especially
    # if considering algebras that work with NAs)
    if (is.function(quantifier)) {
        .Deprecated('mult',
                    msg=paste('The "quantifier" argument is deprecated. Complex compositions',
                                'have to be computed by using the "compose()" or "mult()"',
                                'functions and combined with algebra() operations.'))
        merge <- function(val, x, y) {
            res <- sorting(val, decreasing=TRUE)
            relcard <- seq_along(res) / length(res)
            attr(relcard, 'x') <- x
            attr(relcard, 'y') <- y
            alg$s(alg$pt(res, quantifier(relcard)))
        }
        f <- function(x, y) {
          merge(type(x, y), x, y)
        }

    } else if (!is.null(quantifier)) {
        stop("'quantifier' must be a function or NULL")

    } else {
      f <- function(x, y) {
          merge(type(x, y))
      }
    }

    res <- mult(x, y, f)
    if (!is.null(e)) {
      # TODO: test excluding features!
      # TODO: excluding should not be inside of this function - it should be a separate function
      fe <- function(x, y) {
        alg$s(alg$pt(x, y))
      }
      e <- mult(x, e, fe)
      res <- alg$pt(res, alg$n(e))
    }
    return(res)
}
