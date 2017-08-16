#' S3 class representing a set of fuzzy sets on the fixed universe
#'
#' The aim of the `fsets` S3 class is to store several fuzzy sets in the
#' form of numeric matrix where columns represent fuzzy sets, rows are
#' elements from the universe, and therefore a value of `i`-th row and `j`-th
#' column is a membership degree of `i`-th element of the universe to `j`-th fuzzy
#' set. The \code{fsets} object also stores the information about
#' the origin of the fuzzy sets as well as a relation of specificity among
#' them.
#'
#' The `fsets()` function is a constructor of an object of type `fsets`.
#' Each object stores two attributes: `vars` and `specs`. The functions [vars()]
#' and [specs()]). can be used to access these attributes.
#'
#' It is assumed that the fuzzy sets
#' are derived from some raw variables, e.g. numeric vectors or factors. `vars`
#' attribute is a character vector of names of raw variables with size equal
#' to the number of fuzzy sets in `fsets` object. It is assumed that
#' two fuzzy sets with the same name in [vars()] attribute are derived from
#' the same variable.
#'
#' `specs` attribute gives a square numeric matrix of size equal to the number
#' of fuzzy sets in `fsets`. `specs[i][j] == 1` if and only if the `i`-th fuzzy
#' set is more specific than `j`-th fuzzy set. Specificity of fuzzy sets means
#' the nestedness of fuzzy set: for instance, `very small` is more specific than
#' `small`; similarly, `extremely big` is more specific than `very big`; on the
#' other hand, `very big` and `extremely small` are incomparable. A necessary
#' condition for specificity is subsethood.
#'
#' @param x A matrix of membership degrees. Columns of the matrix represent
#'   fuzzy sets, colnames are names of the fuzzy sets (and must not be NULL). Rows
#'   of the matrix represent elements of the universe.
#' @param vars A character vector that must correspond to the
#'   columns of `x`. It is a vector of names of original variables that the
#'   fuzzy sets were created from. In other words, the `vars` vector should
#'   contain the same value for each `x`'s column that corresponds to the same
#'   variable. Moreover, the names of the `vars` vector must be the same as `colnames(x)`.
#'   For instance, an [fcut()] function can transform a single numeric
#'   vector into several different fuzzy sets. To indicate that all of them in
#'   fact describe the same original variable, the same name is stored on
#'   appropriate positions of the `vars` vector.
#' @param specs A square numeric matrix containing values from `{0, 1}`.
#'   It is a specificity matrix, for which both rows and columns correspond to
#'   `x`'s columns and where `specs[i][j] == 1` if and only if `i`-th fuzzy
#'   set (i.e. `x[, i]`) is more specific (is a subset of) than `j`-th fuzzy
#'   set (i.e. `x[, j]`).
#' @param f An instance of S3 class `fsets`.
#' @param value Attribute values to be set to the object.
#' @param ... further arguments to be passed to the underlying functions.
#' @return `fsets()` returns an object of S3 class `fsets`. `vars()` returns
#'   a vector of original variable names of the `fsets` object. `specs`
#'   returns the specificity matrix.
#' @author Michal Burda
#' @seealso [fcut()], [lcut()], [is.specific()]
#' @keywords models robust
#' @examples
#'     # create a matrix of random membership degrees
#'     m <- matrix(runif(30), ncol=5)
#'     colnames(m) <- c('a1', 'a2', 'a12', 'b1', 'b2')
#'
#'     # create vars - first three (a1, a2, a3) and next two (b1, b2)
#'     # fuzzy sets originate from the same variable
#'     v <- c('a', 'a', 'a', 'b', 'b')
#'     names(v) <- colnames(m)
#'
#'     # create specificity matrix - a1 and a2 are more specific than a12,
#'     # the rest is incomparable
#'     s <- matrix(c(0, 0, 1, 0, 0,
#'                   0, 0, 1, 0, 0,
#'                   0, 0, 0, 0, 0,
#'                   0, 0, 0, 0, 0,
#'                   0, 0, 0, 0, 0), byrow=TRUE, ncol=5)
#'     colnames(s) <- colnames(m)
#'     rownames(s) <- colnames(m)
#'
#'     # create a valid instance of the fsets class
#'     o <- fsets(m, v, s)
#'
#' @export
fsets <- function(x, vars, specs) {
    .mustBeNumericMatrix(x)
    .mustBe(is.vector(vars) && ncol(x) == length(vars), "'vars' must be a vector of size equal to 'ncol(x)'")

    if (is.null(colnames(x)) != is.null(names(vars)) || any(colnames(x) != names(vars))) {
        stop("'vars' must be a vector with names equal to 'colnames(x)'")
    }
    if (!is.matrix(specs) || !is.numeric(specs) || ncol(x) != ncol(specs) || ncol(x) != nrow(specs)) {
        stop("'specs' must be a square numeric matrix of size equal to 'ncol(x)'")
    }
    if (is.null(colnames(x)) != is.null(colnames(specs)) ||
            is.null(colnames(x)) != is.null(rownames(specs)) ||
            any(colnames(x) != colnames(specs)) ||
            any(colnames(x) != rownames(specs))) {
        stop("'specs' must be a numeric matrix with colnames and rownames equal to 'colnames(x)'")
    }
    return(structure(x,
                     class=c('fsets', 'matrix'),
                     vars=vars,
                     specs=specs))
}


#' @rdname fsets
#' @export
vars <- function(f) {
    attr(f, 'vars')
}


#' @rdname fsets
#' @export
`vars<-` <- function(f, value) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    .mustBe(is.vector(value) && ncol(f) == length(value), "'value' must be a vector of size equal to 'ncol(f)'")
    if (is.null(colnames(f)) != is.null(names(value)) || any(colnames(f) != names(value))) {
        stop("'value' must be a vector with names equal to 'colnames(f)'")
    }
    attr(f, 'vars') <- value
}


#' @rdname fsets
#' @export
specs <- function(f) {
    attr(f, 'specs')
}


#' @rdname fsets
#' @export
`specs<-` <- function(f, value) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    if (!is.matrix(value) || !is.numeric(value) || ncol(f) != ncol(value) || ncol(f) != nrow(value)) {
        stop("'value' must be a square numeric matrix of size equal to 'ncol(f)'")
    }
    if (is.null(colnames(f)) != is.null(colnames(value)) ||
            is.null(colnames(f)) != is.null(rownames(value)) ||
            any(colnames(f) != colnames(value)) ||
            any(colnames(f) != rownames(value))) {
        stop("'value' must be a numeric matrix with colnames and rownames equal to 'colnames(f)'")
    }
    attr(f, 'specs') <- value
}


#' Test whether `x` is a valid object of the S3 `fsets` class
#'
#' This function tests wheter `x` inherits from S3 `fsets` class and whether
#' it is a numeric matrix with [vars()] and [specs()]
#' attributes of correct size and column/row names.
#'
#' @param x An object being tested.
#' @return TRUE if `x` is a valid `fsets` object and FALSE otherwise.
#' @author Michal Burda
#' @seealso [fsets()]
#' @keywords models robust
#' @export
is.fsets <- function(x) {
    cn <- dimnames(x)[[2]]
    return(inherits(x, 'fsets') &&
           is.matrix(x) &&
           is.vector(vars(x)) &&
           ncol(x) == length(vars(x)) &&
           all(cn == names(vars(x))) &&
           is.matrix(specs(x)) &&
           ncol(x) == ncol(specs(x)) &&
           ncol(x) == nrow(specs(x)) &&
           all(cn == colnames(specs(x))) &&
           all(cn == rownames(specs(x))))
}


#' Convert an object of `fsets` class into a matrix or data frame
#' This function converts an instance of S3 class [fsets] into a
#' matrix or a data frame. The [vars()] and [specs()] attributes
#' of the original object are deleted.
#'
#' @param x An instance of class [fsets] to be converted
#' @param ... arguments further passed to `as.data.frame` after converting
#'   to matrix in `as.data.frame.fsets`. Unused in `as.matrix.fsets`.
#' @return  A numeric matrix or data frame of membership degrees.
#' @author Michal Burda
#' @seealso [fsets()], [fcut()], [lcut()]
#' @keywords models robust
#' @export
as.data.frame.fsets <- function(x, ...) {
    .mustBe(is.fsets(x), "'x' must be an instance of S3 class 'fsets'")
    return(as.data.frame(as.matrix(x), ...))
}


#' @rdname as.data.frame.fsets
#' @export
as.matrix.fsets <- function(x, ...) {
    .mustBe(is.fsets(x), "'x' must be an instance of S3 class 'fsets'")
    class(x) <- 'matrix'
    attr(x, 'vars') <- NULL
    attr(x, 'specs') <- NULL
    return(x)
}
