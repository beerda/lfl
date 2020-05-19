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
#' to the number of fuzzy sets in `fsets` object. It is then assumed that
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
#'   variable. Names of the `vars` vector are ignored.
#'   For instance, an [fcut()] function can transform a single numeric
#'   vector into several different fuzzy sets. To indicate that all of them in
#'   fact describe the same original variable, the same name is stored on
#'   appropriate positions of the `vars` vector.
#' @param specs A square numeric matrix containing values from `{0, 1}`.
#'   It is a specificity matrix, for which both rows and columns correspond to
#'   `x`'s columns and where `specs[i][j] == 1` if and only if `i`-th fuzzy
#'   set (i.e. `x[, i]`) is more specific (is a subset or equal to) than `j`-th fuzzy
#'   set (i.e. `x[, j]`).
#' @param f An instance of S3 class `fsets`.
#' @param value Attribute values to be set to the object.
#' @return [fsets()] returns an object of S3 class `fsets`. [vars()] returns
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
fsets <- function(x,
                  vars=rep(deparse(substitute(x)), ncol(x)),
                  specs=matrix(0, nrow=ncol(x), ncol=ncol(x))) {
    .mustBeNumericMatrix(x)

    .mustBeCharacterVector(vars)
    .mustNotHaveNA(vars)
    .mustBe(ncol(x) == length(vars), "'vars' must be a vector of size equal to 'ncol(x)'")

    .mustBeNumericMatrix(specs)
    .mustNotHaveNA(specs)
    .mustBe(all(specs == 0 | specs == 1), "'specs' must be a binary matrix (only 0 and 1 are allowed)")
    .mustBe(identical(ncol(x), ncol(specs)) && identical(ncol(specs), nrow(specs)),
            "'specs' must be a square numeric matrix of size equal to 'ncol(x)'")

    return(structure(x,
                     class=c('fsets', 'matrix'),
                     vars=as.vector(vars),   # remove all attributes
                     specs=matrix(specs, nrow=nrow(specs))))   # remove all attributes
}


#' @rdname fsets
#' @export
vars <- function(f) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    attr(f, 'vars')
}


#' @rdname fsets
#' @export
`vars<-` <- function(f, value) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    .mustBeCharacterVector(value)
    .mustNotHaveNA(value)
    .mustBe(ncol(f) == length(value), "'value' must be a vector of size equal to fsets' ncol")
    attr(f, 'vars') <- as.vector(value)
    return(f)
}


#' @rdname fsets
#' @export
specs <- function(f) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    attr(f, 'specs')
}


#' @rdname fsets
#' @export
`specs<-` <- function(f, value) {
    .mustBe(is.fsets(f), "'f' must be an instance of S3 'fsets' class")
    .mustBeNumericMatrix(value)
    .mustNotHaveNA(value)
    .mustBe(all(value == 0 | value == 1), "'value' must be a binary matrix (only 0 and 1 are allowed)")
    .mustBe(identical(ncol(f), ncol(value)) && identical(ncol(value), nrow(value)),
            "'value' must be a square numeric matrix of size equal to fsets' ncol")
    attr(f, 'specs') <- matrix(value, nrow=nrow(value))
    return(f)
}


#' Test whether `x` is a valid object of the S3 `fsets` class
#'
#' This function tests whether `x` inherits from S3 `fsets` class.
#'
#' @param x An object being tested.
#' @return TRUE if `x` is a valid `fsets` object and FALSE otherwise.
#' @author Michal Burda
#' @seealso [fsets()]
#' @keywords models robust
#' @export
is.fsets <- function(x) {
    (return(inherits(x, 'fsets') &&
            is.matrix(x)))
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
    return(matrix(x, nrow=nrow(x), dimnames=dimnames(x)))
}


#' Combine several 'fsets' objects into a single one
#'
#' Take a sequence of objects of class 'fsets' and combine them by columns.
#' This version of cbind takes care of the [vars()] and [specs()]
#' attributes of the arguments and merges them to the result. If some argument
#' does not inherit from the 'fsets' class, an error is thrown.
#'
#' The [vars()] attribute is merged by concatenating the [vars()] attributes
#' of each argument. Also the [specs()] attributes of the arguments are merged together.
#'
#' @param ...  A sequence of objects of class 'fsets' to be merged by columns.
#' @param deparse.level This argument has currently no function and is added
#' here only for compatibility with generic [cbind()] function.
#' @param warn Whether to issue warning when combining two fsets having the same vars
#' about the fact that specs may not be accurate
#' @return An object of class 'fsets' that is created by merging the arguments
#' by columns.  Also the arguments' attributes [vars()] and [specs()] are merged together.
#' @author Michal Burda
#' @seealso [vars()], [specs()], [fcut()], [lcut()]
#' @keywords models robust
#'
#' @examples
#'     d1 <- fcut(CO2[, 1:2])
#'     d2 <- fcut(CO2[, 3:4], breaks=list(conc=1:4*1000/4))
#'     r <- cbind(d1, d2)
#'
#'     print(colnames(d1))
#'     print(colnames(d2))
#'     print(colnames(r))
#'
#'     print(vars(d1))
#'     print(vars(d2))
#'     print(vars(r))
#'
#'     print(specs(d1))
#'     print(specs(d2))
#'     print(specs(r))
#'
#' @export
cbind.fsets <- function(..., deparse.level = 1, warn = TRUE) {
    dots <- list(...)
    m <- NULL
    v <- NULL
    s <- NULL
    for (i in seq_along(dots)) {
        arg <- dots[[i]]
        argName <- names(dots)[i]

        if (!is.null(arg)) {
            .mustBe(is.fsets(arg),
                    "Cannot cbind arguments that are not valid 'fsets' objects")
            if (is.null(m)) {
                v <- vars(arg)
                s <- specs(arg)
                class(arg) <- 'matrix'
                m <- arg
            } else {
                if (warn && length(intersect(v, vars(arg))) > 0) {
                    warning("Binding fsets with the same 'vars' - resulting 'specs' may be incorrect")
                    warn <- FALSE
                }
                v <- c(v, vars(arg))
                o1 <- matrix(0, nrow=nrow(s), ncol=ncol(specs(arg)))
                o2 <- matrix(0, nrow=nrow(specs(arg)), ncol=ncol(s))
                s <- rbind(cbind(s, o1), cbind(o2, specs(arg)))
                class(arg) <- 'matrix'
                m <- cbind(m, arg)
            }
        }
    }
    return(fsets(m, v, s))
}


#' @export
rbind.fsets <- function(..., deparse.level = 1) {
    dots <- list(...)
    m <- NULL
    v <- NULL
    s <- NULL
    for (i in seq_along(dots)) {
        arg <- dots[[i]]

        if (!is.null(arg)) {
            .mustBe(is.fsets(arg),
                    "Cannot rbind arguments that are not valid 'fsets' objects")
            if (is.null(m)) {
                v <- vars(arg)
                s <- specs(arg)
                class(arg) <- 'matrix'
                m <- arg
            } else {
                .mustBe(identical(colnames(m), colnames(arg)),
                        "Cannot rbind fsets having not equal column names")
                .mustBe(identical(v, vars(arg)),
                        "Cannot rbind fsets with unequal 'vars'");
                .mustBe(identical(s, specs(arg)),
                        "Cannot rbind fsets with unequal 'specs'");
                class(arg) <- 'matrix'
                m <- rbind(m, arg)
            }
        }
    }

    return(fsets(m, v, s))
}


#' @export
`[.fsets` <- function(x, i, j, drop=FALSE) {
    if (drop) warning('drop ignored')
    v <- vars(x)
    s <- specs(x)
    m <- as.matrix(x)

    names(v) <- colnames(x)
    colnames(s) <- colnames(x)
    rownames(s) <- colnames(x)

    return(fsets(m[i, j, drop=FALSE],
                 vars=v[j],
                 specs=s[j, j, drop=FALSE]))
}


#' Print an instance of the [fsets()] class
#'
#' Format an object of the [fsets()] class into human readable form
#' and print it to the output.
#'
#' @param x An instance of the [fsets()] class
#' @param ...  Unused.
#' @return Nothing
#' @author Michal Burda
#' @seealso [fsets()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#'     d <- fcut(CO2[, 1:2])
#'     print(d)
#'
#' @export
print.fsets <- function(x, ...) {
    .mustBe(is.fsets(x), "'x' is not a valid 'fsets' object")
    v <- vars(x)
    s <- specs(x)
    print(as.matrix(x))
    cat("\nvars:\n")
    print(v)
    cat("\nspecs:\n")
    print(s)
}


#' Plot membership degrees stored in the instance of the S3 class
#' [fsets()] as a line diagram.
#'
#' This function plots the membership degrees stored in the instance of the
#' [fsets()] class. Internally, the membership degrees are
#' transformed into a time-series object and viewed in a plot using the
#' [ts.plot()] function. This function is useful mainly to see the
#' shape of fuzzy sets on regularly sampled inputs.
#'
#' @param x An instance of class [fsets()]
#' @param ...  Other arguments that are passed to the underlying [ts.plot()]
#' function.
#' @return Result of the [ts.plot()] method.
#' @author Michal Burda
#' @seealso [fsets()], [fcut()], [lcut()], [ts.plot()]
#' @keywords models robust multivariate
#' @examples
#' d <- lcut(0:1000/1000, name='x')
#' plot(d)
#'
#' # Additional arguments are passed to the ts.plot method
#' # Here thick lines represent atomic linguistic expressions,
#' # i.e. ``small'', ``medium'', and ``big''.
#' plot(d,
#'      ylab='membership degree',
#'      xlab='values',
#'      gpars=list(lwd=c(rep(1, 3), 5, rep(1, 5), 5, rep(1, 7), 5, rep(1,4))))
#' @export
#' @importFrom stats ts
#' @importFrom stats ts.plot
plot.fsets <- function(x, ...) {
    n <- nrow(x)
    args <- list(...)
    x <- as.list(as.data.frame(x))
    x <- plyr::llply(x, ts, start=0, frequency=n)
    x <- c(x, args)
    if (is.null(args[['xlab']])) {
        x[['xlab']] <- ''
    }
    if (is.null(args[['ylab']])) {
        x[['ylab']] <- ''
    }
    do.call('ts.plot', x)
}
