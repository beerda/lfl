#' Transform data into a `fsets` S3 class of linguistic fuzzy attributes
#'
#' This function creates a set of linguistic fuzzy attributes from crisp data.
#' Numeric vectors, matrix or data frame columns are transformed into a set of
#' fuzzy attributes, i.e. columns with membership degrees. Factors and other
#' data types are transformed to fuzzy attributes by calling the
#' [fcut()] function.
#'
#' The aim of this function is to transform numeric data into a set of fuzzy
#' attributes.  The resulting fuzzy attributes have direct linguistic
#' interpretation. This is a unique variant of fuzzification that is suitable
#' for the inference mechanism based on Perception-based Linguistic Description
#' (PbLD) -- see [pbld()].
#'
#' A numeric vector is transformed into a set of fuzzy attributes accordingly
#' to the following scheme:
#'
#' \eqn{<hedge> <atomic expression>}
#'
#' where \eqn{<atomic expression>} is an atomic linguistic expression, a value
#' from the following possibilities (note that the allowance of atomic expressions
#' is influenced with `context` being used - see [ctx] for details):
#' * `neg.bi`: big negative (far from zero)
#' * `neg.um`: upper medium negative (between medium negative and big negative)
#' * `neg.me`: medium negative
#' * `neg.lm`: lower medium negative (between medium negative and small negative)
#' * `neg.sm`: small negative (close to zero)
#' * `ze`: zero
#' * `sm`: small
#' * `lm`: lower medium
#' * `me`: medium
#' * `um`: upper medium
#' * `bi`: big
#'  A \eqn{<hedge>} is a modifier that further concretizes the atomic expression
#'  (note that not each combination of hedge and atomic expression is allowed -
#'  see [allowed.lingexpr] for more details):
#' * `ex`: extremely,
#' * `si`: significantly,
#' * `ve`: very,
#' * `ty`: typically,
#' * `-`: empty hedge (no hedging),
#' * `ml`: more or less,
#' * `ro`: roughly,
#' * `qr`: quite roughly,
#' * `vr`: very roughly.
#'
#' Accordingly to the theory developed by Novak (2008), not every hedge is
#' suitable with each atomic #' expression (see the description of the `hedges`
#' argument).  The hedges to be used can be selected with the `hedges` argument.
#' Function takes care of not to use hedge together with an un-applicable atomic
#' expression by itself.
#'
#' Obviously, distinct data have different meaning of what is "small",
#' "medium", or "big" etc.  Therefore, a `context` has to be set that
#' specifies sensible values for these linguistic expressions.
#'
#' If a matrix (resp. data frame) is provided to this function instead of
#' a single vector, all columns are processed the same way.
#'
#' The function also sets up properly the [vars()] and
#' [specs()] properties of the result.
#'
#' @param x Data to be transformed: if it is a numeric vector, matrix, or data
#' frame, then the creation of linguistic fuzzy attributes takes place. For
#' other data types the [fcut()] function is called implicitly.
#' @param context A definition of context of a numeric attribute. It must be
#' an instance of an S3 class [ctx3()], [ctx5()], [ctx3bilat()] or [ctx5bilat()].
#'
#' If `x` is a matrix or data frame then `context` should be a named
#' list of contexts for each `x`'s column.
#' @param atomic A vector of atomic linguistic expressions to be used for
#' creation of fuzzy attributes.
#' @param hedges A vector of linguistic hedges to be used for creation of fuzzy
#' attributes.
#' @param name A name to be added as a suffix to the created fuzzy attribute
#' names. This parameter can be used only if `x` is a numeric vector. If
#' `x` is a matrix or data frame, `name` should be NULL because the
#' fuzzy attribute names are taken from column names of parameter `x`.
#' The `name` is also used as a value for the `vars` attribute of the resulting
#' [fsets()] instance.
#' @param hedgeParams Parameters that determine the shape of the hedges
#' @param ...  Other parameters to some methods.
#' @return An object of S3 class `fsets` is returned, which is a numeric matrix
#' with columns representing the fuzzy attributes. Each source columm of the
#' `x` argument corresponds to multiple columns in the resulting matrix.
#' Columns will have names derived from used \eqn{hedges}, atomic expression,
#' and \eqn{name} specified as the optional parameter.
#'
#' The resulting object would also have set the [vars()] and [specs()]
#' properties with the former being created from original
#' column names (if `x` is a matrix or data frame) or the `name`
#' argument (if `x` is a numeric vector). The [specs()]
#' incidency matrix would be created to reflect the following order of the
#' hedges: \eqn{"ex" < "si" < "ve" < "-" < "ml" < "ro" < "qr" < "vr"} and
#' \eqn{"ty" < "" < "ml" < "ro" < "qr" < "vr"}.  Fuzzy attributes created from
#' the same source numeric vector (or column) would be ordered that way, with other fuzzy attributes
#' (from the other source) being incomparable.
#' @author Michal Burda
#' @seealso [fcut()], [fsets()], [vars()], [specs()]
#' @references V. Novak, A comprehensive theory of trichotomous evaluative
#' linguistic expressions, Fuzzy Sets and Systems 159 (22) (2008) 2939--2969.
#' @keywords models robust multivariate
#' @examples
#'
#' # transform a single vector
#' x <- runif(10)
#' lcut(x, name='age')
#'
#' # transform single vector with a custom context
#' lcut(x, context=ctx5(0, 0.2, 0.5, 0.7, 1), name='age')
#'
#' # transform all columns of a data frame
#' # and do not use any hedges
#' data <- CO2[, c('conc', 'uptake')]
#' lcut(data)
#'
#'
#' # definition of custom contexts for different columns
#' # of a data frame while selecting only "ve" and "ro" hedges.
#' lcut(data,
#'      context=list(conc=minmax,
#'                   uptake=ctx3(0, 25, 50)),
#'      hedges=c('ve', 'ro'))
#'
#'
#' # lcut on non-numeric data is the same as fcut()
#' ff <- factor(substring("statistics", 1:10, 1:10), levels = letters)
#' lcut(ff)
#'
#' @export
#' @name lcut
lcut <- function(x, ...) {
    UseMethod('lcut')
}


#' @rdname lcut
#' @export
lcut.default <- function(x, ...) {
    .stop(paste0("'lcut' not implemented for variable of class '", class(x), "'"))
}


#' @rdname lcut
#' @export
lcut.factor <- function(x,
                        name=deparse(substitute(x)),
                        ...) {
    fcut(x, name=name, ...)
}


#' @rdname lcut
#' @export
lcut.logical <- function(x,
                         name=deparse(substitute(x)),
                         ...) {
    fcut(x, name=name, ...)
}


.allHedges=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr')
.sharpness <- matrix(0,
                     nrow=length(.allHedges),
                     ncol=length(.allHedges),
                     dimnames=list(.allHedges, .allHedges))
.sharpness[row(.sharpness) < col(.sharpness)] <- 1
.sharpness[, 'ty'] <- 0


#' @rdname lcut
#' @export
lcut.numeric <- function(x,
                         context=minmax,
                         atomic=c('sm', 'me', 'bi', 'lm', 'um', 'ze',
                                  'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um'),
                         hedges=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'),
                         name=NULL,
                         hedgeParams=defaultHedgeParams,
                         ...) {
    atomic <- match.arg(atomic, several.ok=TRUE)
    hedges <- match.arg(hedges, several.ok=TRUE)

    .mustBeNumericVector(x)
    .mustNotBeZeroLength(atomic)
    .mustNotBeZeroLength(hedges)
    .mustNotBeNull(name)

    if (is.function(context)) {
        context <- context(x)
    }

    type <- NULL
    for (clazz in names(.horizonAllowedTable)) {
        if (inherits(context, clazz)) {
            type <- clazz
        }
    }
    .mustBe(!is.null(type),
            paste0("'context' must be an object of one of the following classes: '",
                   paste(names(.horizonAllowedTable), collapse="', '"), "' or a function returning such object"))

    allowedAtomic <- names(.horizonAllowedTable[[type]])
    if (!identical(atomic, eval(formals(lcut.numeric)$atomic))) {
        # non-default atomic argument
        wrongAtomic <- setdiff(atomic, allowedAtomic)
        if (length(wrongAtomic) > 0) {
            warning(paste0("The following atomic expressions are not allowed for context '",
                         type, "': ", paste(wrongAtomic, collapse=', ')))
        }
    }
    atomic <- intersect(atomic, allowedAtomic)

    res <- NULL
    for (a in atomic) {
        allowedHedges <- names(which(allowed.lingexpr[, a]))
        allowedHedges <- intersect(allowedHedges, hedges)
        m <- lapply(allowedHedges, function(h) {
            lingexpr(context, atomic=a, hedge=h, hedgeParams=hedgeParams)(x)
        })
        m <- matrix(unlist(m, use.names=FALSE), ncol=length(m), byrow=FALSE)

        n <- paste(allowedHedges, a, name, sep='.')
        n <- sub('-.', '', n, fixed=TRUE)
        colnames(m) <- n

        v <- rep(name, ncol(m))
        s <- .sharpness[allowedHedges, allowedHedges, drop=FALSE]
        f <- fsets(m, v, s)
        res <- cbind.fsets(res, f, warn=FALSE)
    }
    return(res)
}


.byNameOrDefault <- function(x, name, default) {
    res <- x[[name]]
    if (is.null(res)) {
        res <- default
    }
    return(res)
}


#' @rdname lcut
#' @export
lcut.data.frame <- function(x,
                            context=minmax,
                            atomic=c('sm', 'me', 'bi', 'lm', 'um', 'ze',
                                     'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um'),
                            hedges=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'),
                            ...) {
    .mustBeDataFrame(x)
    if (is.null(colnames(x))) {
        stop("Columns of 'x' must have names")
    }
    if (!is.list(context)) {
        context <- rep(list(context), ncol(x))
        names(context) <- colnames(x)
    }
    if (!is.list(atomic)) {
        atomic <- rep(list(atomic), ncol(x))
        names(atomic) <- colnames(x)
    }
    if (!is.list(hedges)) {
        hedges <- rep(list(hedges), ncol(x))
        names(hedges) <- colnames(x)
    }
    res <- NULL
    for (i in colnames(x)) {
        f <- lcut(x[[i]],
                  context=.byNameOrDefault(context, i, minmax),
                  atomic=.byNameOrDefault(atomic, i, c('sm', 'me', 'bi', 'lm', 'um', 'ze', 'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um')),
                  hedges=.byNameOrDefault(hedges, i, c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr')),
                  name=i,
                  ...)
        res <- cbind.fsets(res, f)
    }
    return(res)
}


#' @rdname lcut
#' @export
lcut.matrix <- function(x, ...) {
    lcut(as.data.frame(x), ...)
}
