#' Transform data into a `fsets` S3 class using shapes derived from
#' triangles or raised cosines
#'
#' This function creates a set of fuzzy attributes from crisp data. Factors,
#' numeric vectors, matrix or data frame columns are transformed into a set of
#' fuzzy attributes, i.e. columns with membership degrees. Unlike
#' [lcut()], for transformation is not used the linguistic linguistic
#' approach, but partitioning using regular shapes of the fuzzy sets (such as
#' triangle, raised cosine).
#'
#' The aim of this function is to transform numeric data into a set of fuzzy
#' attributes.  The result is in the form of the object of class "fsets", i.e.
#' a numeric matrix whose columns represent fuzzy sets (fuzzy attributes) with
#' values being the membership degrees.
#'
#' The function behaves differently to the type of input `x`.
#'
#' If `x` is a factor or a logical vector (or other non-numeric data) then
#' for each distinct value of an input, a fuzzy set is created, and data would
#' be transformed into crisp membership degrees 0 or 1 only.
#'
#' If `x` is a numeric vector then fuzzy sets are created accordingly to
#' break-points specified in the `breaks` argument with 1st, 2nd and 3rd
#' break-point specifying the first fuzzy set, 2nd, 3rd and 4th break-point
#' specifying th second fuzzy set etc. The shape of the fuzzy set is determined
#' by the `type` argument that may be equal either to a string
#' `'triangle'` or `'raisedcos'` or it could be a function that
#' computes the membership degrees for itself (see [triangular()] or
#' [raisedcosine()] functions for details). Additionally, super-sets of
#' these elementary sets may be created by specifying the `merge`
#' argument. Values of this argument specify how many consecutive fuzzy sets
#' should be combined (by using the Lukasiewic's t-conorm) to produce
#' super-sets - see the description of `merge` above.
#'
#' If a matrix (resp. data frame) is provided to this function instead of
#' single vector, all columns are processed separately as described above and
#' the result is combined with the [cbind.fsets()] function.
#'
#' The function sets up properly the [vars()] and [specs()]
#' properties of the result.
#'
#' @aliases fcut fcut.default fcut.numeric fcut.matrix fcut.data.frame
#' @param x Data to be transformed: a vector, matrix, or data frame.
#' Non-numeric data are allowed.
#' @param breaks This argument determines the break-points of the positions of
#' the fuzzy sets. It should be an ordered vector of numbers such that the
#' \eqn{i}-th index specifies the beginning, \eqn{(i+1)}-th the center, and
#' \eqn{(i+2)}-th the ending of the \eqn{i}-th fuzzy set.
#'
#' I.e. the minimum number of breaks-points is 3; \eqn{n-2} elementary fuzzy
#' sets would be created for \eqn{n} break-points.
#'
#' If considering an i-th fuzzy set (of `type='triangle'`), `x`
#' values lower than \eqn{i}-th break (and greater than \eqn{(i+2)}-th break)
#' would result in zero membership degree, values equal to \eqn{(i+1)}-th break
#' would have membership degree equal 1 and values between them the appropriate
#' membership degree between 0 and 1.
#'
#' The resulting fuzzy sets would be named after the original data by adding
#' dot (".") and a number \eqn{i} of fuzzy set.
#'
#' Unlike [base::cut()], `x` values, that are lower or greater than
#' the given break-points, will have all membership degrees equal to zero.
#'
#' For non-numeric data, this argument is ignored. For `x` being a numeric
#' vector, it must be a vector of numeric values. For `x` being a numeric
#' matrix or data frame, it must be a named list containing a numeric vector
#' for each column - if not, the values are repeated for each column.
#' @param name A name to be added as a suffix to the created fuzzy attribute
#' names. This parameter can be used only if `x` is a vector. If `x`
#' is a matrix or data frame, `name` should be NULL because the fuzzy
#' attribute names are taken from column names of the argument `x`.
#' @param type The type of fuzzy sets to create. Currently, `'triangle'` or
#' `'raisedcos'` may be used. The `type` argument may be also a
#' function with 3 or 4 arguments:
#' * if `type` is a 4-argument function, it is assumed that that it computes
#' membership degrees from values of the first argument while considering
#' the boundaries given by the next 3 arguments;
#' * if `type` is a 3-argument function, it is assumed that it is a factory
#' function similar to [triangular()] or [raisedcosine()], which, from given
#' three boundaries, creates a function that computes membership degrees.
#' @param merge This argument determines whether to derive additional fuzzy
#' sets by merging the elementary fuzzy sets (whose position is determined with
#' the `breaks` argument) into super-sets.  The argument is ignored for
#' non-numeric data in `x`.
#'
#' `merge` may contain any integer number from `1` to
#' `length(breaks) - 2`.  Value `1` means that the elementary fuzzy
#' sets should be present in the output.  Value `2` means that the two
#' consecutive elementary fuzzy sets should be combined by using the Lukasiewic
#' t-conorm, value `3` causes combining three consecutive elementary fuzzy
#' sets etc.
#'
#' The names of the derived (merged) fuzzy sets is derived from the names of
#' the original elementary fuzzy sets by concatenating them with the "|" (pipe)
#' separator.
#' @param parallel Whether the processing should be run in parallel or not.
#' Parallelization is implemented using the [foreach::foreach()] function.
#'  The parallel environment must be set properly in advance, e.g. with
#' the [doMC::registerDoMC()] function.  Currently this argument is
#' applied only if `x` is a matrix or data frame.
#' @param ...  Other parameters to some methods.
#' @return An object of class "fsets" is returned, which is a numeric matrix
#' with columns representing the fuzzy attributes. Each source column of the
#' `x` argument corresponds to multiple columns in the resulting matrix.
#' Columns have names that indicate the name of the source as well as a index
#' \eqn{i} of fuzzy set(s) -- see the description of arguments `breaks`
#' and `merge` above.
#'
#' The resulting object would also have set the [vars()] and
#' [specs()] properties with the former being created from original
#' column names (if `x` is a matrix or data frame) or the `name`
#' argument (if `x` is a numeric vector). The [specs()]
#' incidency matrix would be created to reflect the superset-hood of the merged
#' fuzzy sets.
#' @author Michal Burda
#' @seealso [lcut()], [farules()], [pbld()], [vars()], [specs()], [cbind.fsets()]
#' @keywords models robust multivariate
#' @examples
#'
#' # fcut on non-numeric data
#' ff <- factor(substring("statistics", 1:10, 1:10), levels = letters)
#' fcut(ff)
#'
#' # transform a single vector into a single fuzzy set
#' x <- runif(10)
#' fcut(x, breaks=c(0, 0.5, 1), name='age')
#'
#' # transform single vector into a partition of the interval 0-1
#' # (the boundary triangles are right-angled)
#' fcut(x, breaks=c(0, 0, 0.5, 1, 1), name='age')
#'
#' # also create supersets
#' fcut(x, breaks=c(0, 0, 0.5, 1, 1), name='age', merge=c(1, 2))
#'
#' # transform all columns of a data frame
#' # with different breakpoints
#' data <- CO2[, c('conc', 'uptake')]
#' fcut(data, breaks=list(conc=c(95, 95, 350, 1000, 1000),
#'                        uptake=c(7, 7, 28.3, 46, 46)))
#'
#' # using a custom 3-argument function (a function factory):
#' f <- function(a, b, c) {
#'   return(function(x) ifelse(a <= x & x <= b, 1, 0))
#' }
#' fcut(x, breaks=c(0, 0.5, 1), name='age', type=f)
#'
#' # using a custom 4-argument function:
#' f <- function(x, a, b, c) {
#'   return(ifelse(a <= x & x <= b, 1, 0))
#' }
#' fcut(x, breaks=c(0, 0.5, 1), name='age', type=f)
#'
#' @export
fcut <- function(x, ...) {
    UseMethod('fcut')
}


#' @rdname fcut
#' @export
fcut.default <- function(x, ...) {
    .stop(paste0("'fcut' not implemented for variable of class '", class(x), "'"))
}


#' @rdname fcut
#' @export
#' @importFrom stats model.matrix
#' @importFrom plyr laply
fcut.factor <- function(x,
                        name=deparse(substitute(x)),
                        ...) {
    .mustBeFactor(x)
    .mustNotBeNull(name)
    .mustBeCharacterScalar(name)

    res <- laply(x, function(a) { a == levels(x) }) + 0
    res <- as.matrix(res)
    colnames(res) <- paste(name, levels(x), sep='=')

    theVars <- rep(name, ncol(res))
    theSpecs <- matrix(0, nrow=ncol(res), ncol=ncol(res))
    return(fsets(res,
                 vars=theVars,
                 specs=theSpecs))
}


#' @rdname fcut
#' @export
fcut.logical <- function(x,
                        name=deparse(substitute(x)),
                        ...) {
    .mustBeLogicalVector(x)
    .mustNotBeNull(name)
    .mustBeCharacterScalar(name)

    res <- matrix(c(x+0, (!x)+0), byrow=FALSE, ncol=2)
    colnames(res) <- c(name, paste0('not.', name))

    theVars <- rep(name, ncol(res))
    theSpecs <- matrix(0, nrow=ncol(res), ncol=ncol(res))
    return(fsets(res,
                 vars=theVars,
                 specs=theSpecs))
}


#' @rdname fcut
#' @export
#' @importFrom zoo rollapply
fcut.numeric <- function(x,
                         breaks,
                         name=deparse(substitute(x)),
                         type=c('triangle', 'raisedcos'),
                         merge=1,
                         parallel=FALSE,
                         ...) {
    n <- length(breaks) - 2

    .mustBeNumericVector(x)
    .mustBeNumericVector(breaks)
    .mustBe(length(breaks) >= 3, "'breaks' must be a numeric vector with at least 3 elements")
    .mustNotBeNull(name)
    .mustBeCharacterScalar(name)
    .mustBeNumericVector(merge)
    .mustBe(min(merge) >= 1 && max(merge) <= n, "'merge' must contain integers from 1 to length(breaks)-2")

    func <- NULL
    if (is.function(type)) {
        func <- type
    } else {
        type <- match.arg(type)
        if (type == 'triangle') {
            func <- triangular
        } else {
            func <- raisedcosine
        }
    }
    if (length(names(formals(func))) == 3L) {
        old <- func
        func <- function(x, a, b, c) {
            old(a, b, c)(x)
        }
    }

    # split 'x' accordingly to 'breaks'
    singles <- rollapply(breaks, 3, function(b) {
        func(x, b[1], b[2], b[3])
    })
    singles <- t(as.matrix(singles))
    colnames(singles) <- paste(name, 1:ncol(singles), sep='=')

    # handle merging
    merge <- as.integer(merge)
    res <- NULL
    if (identical(merge, 1L)) {
        res <- singles
    } else {
        for (w in merge) {
            add <- rollapply(seq_len(ncol(singles)), w, function(ii) {
                m <- singles[, ii, drop=FALSE]
                l <- split(m, col(m))
                do.call(plukas.tconorm, l)
            })
            add <- t(add)
            colnames(add) <- rollapply(colnames(singles), w, paste, collapse='|')

            if (is.null(res)) {
                res <- add
            } else {
                res <- cbind(res, add)
            }
        }
    }

    .firstCol <- function(n, i, rowlen) {
        res <- NULL
        k <- n
        while (length(res) < rowlen) {
            if (i <= 1) {
                res <- c(res, rep(0, k))
            } else {
                res <- c(res, rep(1, i), rep(0, k - i))
            }
            k <- k - 1
            i <- i - 1
        }
        return(res)
    }

    .subMat <- function(n, i, rowlen) {
        first <- .firstCol(n, i, rowlen)
        res <- lapply(1:(n-i+1), function(a) {
            r <- c(rep(0, a-1), first)[1:rowlen]
        })
        return(unlist(res))
    }

    .whatToSelect <- function(n, merge) {
        what <- rep(0, n)
        what[merge] <- 1
        counts <- n:1
        res <- sapply(1:n, function(i) {
            rep(what[i], counts[i])
        })
        return(as.logical(unlist(res)))
    }

    # generate vars vector
    vars <- rep(name, ncol(res))

    # generate specs matrix
    rowlen <- (1+n)*n/2
    vec <- unlist(sapply(seq_len(n), function(i) .subMat(n, i, rowlen)))
    specs <- matrix(vec, byrow=FALSE, nrow=rowlen)
    whatToSelect <- .whatToSelect(n, merge)
    specs <- specs[whatToSelect, whatToSelect, drop=FALSE]

    return(fsets(res, vars=vars, specs=specs))
}


#' @rdname fcut
#' @export
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
fcut.data.frame <- function(x,
                            breaks=NULL,
                            name=NULL,
                            type=c('triangle', 'raisedcos'),
                            merge=1,
                            parallel=FALSE,
                            ...) {

    .mustBeDataFrame(x);
    .mustBe(is.null(name), "If 'x' is a matrix or data frame then 'name' must be NULL")
    .mustBe(!is.null(colnames(x)), "Columns of 'x' must have names")

    if (!is.list(breaks)) {
        breaks <- rep(list(breaks), ncol(x))
        names(breaks) <- colnames(x)
    }
    if (!is.list(merge)) {
        merge <- rep(list(merge), ncol(x))
        names(merge) <- colnames(x)
    }
    if (!is.list(type)) {
        type <- rep(list(type), ncol(x))
        names(type) <- colnames(x)
    }

    loopBody <- function(n) {
        aBreaks <- breaks[[n]]
        aMerge <- merge[[n]]
        aType <- type[[n]]

        res <- fcut(x[[n]],
                    breaks=aBreaks,
                    name=n,
                    type=aType,
                    merge=aMerge,
                    parallel=FALSE,
                    ...)
        return(res)
    }

    n <- NULL
    if (parallel) {
        result <- foreach(n=colnames(x), .combine=cbind.fsets) %dopar% {
            return(loopBody(n))
        }
    } else {
        result <- foreach(n=colnames(x), .combine=cbind.fsets) %do% {
            return(loopBody(n))
        }
    }
    return(result)
}


#' @rdname fcut
#' @export
fcut.matrix <- function(x, ...) {
    result <- fcut(as.data.frame(x), ...)
    return(result)
}
