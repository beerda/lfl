.mustBeNumericScalar <- function(x) {
    if (!is.vector(x) || !is.numeric(x) || length(x) != 1) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric vector of size 1"),
             call.=FALSE)
    }
 }

.mustBeCharacterScalar <- function(x) {
    if (!is.vector(x) || !is.character(x) || length(x) != 1) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a character vector of size 1"),
             call.=FALSE)
    }
 }

.mustBeNumericVector <- function(x) {
    if (!is.vector(x) || !is.numeric(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric vector"),
             call.=FALSE)
    }
 }

.mustBeCharacterVector <- function(x) {
    if (!is.vector(x) || !is.character(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a character vector"),
             call.=FALSE)
    }
 }

.mustBeList <- function(x) {
    if (!is.list(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a list"),
             call.=FALSE)
    }
 }

.mustBeMatrix <- function(x) {
    if (!is.matrix(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a matrix"),
             call.=FALSE)
    }
 }

.mustBeNumericMatrix <- function(x) {
    if (!is.matrix(x) || !is.numeric(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a numeric matrix"),
             call.=FALSE)
    }
 }

.mustBeDataFrame <- function(x) {
    if (!is.data.frame(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a data frame"),
             call.=FALSE)
    }
 }

.mustBeTs <- function(x) {
    if (!stats::is.ts(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a time series object"),
             call.=FALSE)
    }
 }

.mustBeFunction <- function(x) {
    if (!is.function(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be a function"),
             call.=FALSE)
    }
 }

.mustNotBeNull <- function(x) {
    if (is.null(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not be NULL"),
             call.=FALSE)
    }
 }

.mustNotHaveNA <- function(x) {
    if (anyNA(x)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not contain NA values"),
             call.=FALSE)
    }
 }

.mustNotBeZeroLength <- function(x) {
    if (length(x) <= 0) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must not be of zero length"),
             call.=FALSE)
    }
 }

.stop <- function(msg) {
    stop(paste0(as.list(sys.call(-1))[[1]], ": ", msg), call.=FALSE)
}

.mustBe <- function(cond, msg) {
    if (!cond) {
        stop(paste0(as.list(sys.call(-1))[[1]], ": ", msg), call.=FALSE)
    }
}

.mustBeOneOf <- function(x, vals) {
    if (!all(x %in% vals)) {
        stop(paste0(as.list(sys.call(-1))[[1]],
                    ": '", deparse(substitute(x)), "' must be one of '",
                    paste(vals, collapse="', '"), "'"), call.=FALSE)
    }
}

