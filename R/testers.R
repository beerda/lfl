.mustBeNumericScalar <- function(x) {
    if (!isTRUE(is.vector(x) && is.numeric(x) && identical(length(x), 1L))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric vector of size 1"),
             call.=FALSE)
    }
 }

.mustBeIntegerScalar <- function(x) {
    if (!isTRUE(is.vector(x) && is.integer(x) && identical(length(x), 1L))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be an integer vector of size 1"),
             call.=FALSE)
    }
 }

.mustBeCharacterScalar <- function(x) {
    if (!isTRUE(is.vector(x) && is.character(x) && identical(length(x), 1L))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a character vector of size 1"),
             call.=FALSE)
    }
}

.mustBeLogicalScalar <- function(x) {
    if (!isTRUE(is.vector(x) && is.logical(x) && identical(length(x), 1L))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a logical vector of size 1"),
             call.=FALSE)
    }
}

.mustBeInInterval <- function(x, a, b) {
    if (!isTRUE(all(x >= a & x <= b))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must contain values within interval ", a, " and ", b),
             call.=FALSE)
    }
}

.mustBeNumericVector <- function(x) {
    if (!isTRUE(is.vector(x) && is.numeric(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric vector"),
             call.=FALSE)
    }
 }

.mustBeCharacterVector <- function(x) {
    if (!isTRUE(is.vector(x) && is.character(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a character vector"),
             call.=FALSE)
    }
 }

.mustBeFactor <- function(x) {
    if (!isTRUE(is.factor(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a factor"),
             call.=FALSE)
    }
 }

.mustBeLogicalVector <- function(x) {
    if (!isTRUE(is.vector(x) && is.logical(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a logical vector"),
             call.=FALSE)
    }
 }

.mustBeList <- function(x) {
    if (!isTRUE(is.list(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a list"),
             call.=FALSE)
    }
 }

.mustBeMatrix <- function(x) {
    if (!isTRUE(is.matrix(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a matrix"),
             call.=FALSE)
    }
 }

.mustBeNumericMatrix <- function(x) {
    if (!isTRUE(is.matrix(x) && is.numeric(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric matrix"),
             call.=FALSE)
    }
 }

.mustBeDataFrame <- function(x) {
    if (!isTRUE(is.data.frame(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a data frame"),
             call.=FALSE)
    }
 }

.mustBeTs <- function(x) {
    if (!isTRUE(stats::is.ts(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a time series object"),
             call.=FALSE)
    }
 }

.mustBeAlgebra <- function(x) {
    if (!isTRUE(is.algebra(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be the algebra object"),
             call.=FALSE)
    }
 }

.mustBeFunction <- function(x) {
    if (!isTRUE(is.function(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a function"),
             call.=FALSE)
    }
 }

.mustNotBeNull <- function(x) {
    if (!isTRUE(!is.null(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not be NULL"),
             call.=FALSE)
    }
 }

.mustNotHaveNA <- function(x) {
    if (!isTRUE(!anyNA(x))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not contain NA values"),
             call.=FALSE)
    }
 }

.mustNotBeZeroLength <- function(x) {
    if (!isTRUE(length(x) > 0)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not be of zero length"),
             call.=FALSE)
    }
 }

.stop <- function(msg) {
    stop(paste0(as.list(sys.call(-1))[[1]], ": ", msg), call.=FALSE)
}

.mustBe <- function(cond, msg) {
    if (!isTRUE(as.vector(cond))) {  # as.vector removes all attributes
        stop(paste0(as.list(sys.call(-1))[[1]], ": ", msg), call.=FALSE)
    }
}

.mustBeOneOf <- function(x, vals) {
    if (!isTRUE(all(x %in% vals))) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be one of '",
                    paste(vals, collapse="', '"), "'"), call.=FALSE)
    }
}

