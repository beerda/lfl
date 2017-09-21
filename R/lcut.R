lcut3 <- function(x, ...) {
    .Deprecated('lcut', 'lfl')
    UseMethod('lcut3')
}


lcut5 <- function(x, ...) {
    .Deprecated('lcut', 'lfl')
    UseMethod('lcut5')
}


#' Transform data into a set of linguistic fuzzy attributes
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
#' where \eqn{<atomic expression>} is a linguistic expression "small" ("sm"),
#' "lower medium" ("lm"), "medium" ("me"), "upper medium" ("um") or "big"
#' ("bi") -- see the `atomic` argument. A \eqn{<hedge>} is a modifier that
#' further concretizes the atomic expression. It can be empty ("") or some
#' value of:
#' * `ty` - typically;
#' * `ex` - extremely;
#' * `si` - significantly;
#' * `ve` - very;
#' * `ml` - more or less;
#' * `ro` - roughly;
#' * `qr` - quite roughly;
#' * `vr` - very roughly.
#'
#' Accordingly to the theory developed by Novak (2008), not every hedge is
#' suitable with each atomic #' expression (see the description of the `hedges`
#' argument).  The hedges to be used can be selected with the `hedges` argument.
#' Function takes care of not to use hedge together with an un-applicable atomic
#' expression by itself.
#'
#' Obviously, distinct data have different meaning of what is "small",
#' "medium", or "big".  Therefore, a `context` has to be set that
#' specifies sensible values for these linguistic expressions.
#'
#' If a matrix (resp. data frame) is provided to this function instead of
#' single vector, all columns are processed the same way.
#'
#' The function also sets up properly the [vars()] and
#' [specs()] properties of the result.
#'
#' @param x Data to be transformed: if it is a numeric vector, matrix, or data
#' frame, then the creation of linguistic fuzzy attributes takes place. For
#' other data types the [fcut()] function is called.
#' @param context A definition of context of a numeric attribute. Context
#' determines how people understand the notions "small", "medium", or "big"
#' with respect to that attribute.  If `x` is a numeric vector then
#' context should be a vector of 3 numbers: typical small, medium, and big
#' value. If the context is set to NULL, these values are taken directly from
#' `x` as follows:
#' * small \eqn{= min(x)};
#' * medium\eqn{=(max(x) - min(x)) * defaultCenter + min(x)};
#' * big\eqn{= max(x)}.
#'
#' If `x` is a matrix or data frame then `context` should be a named
#' list of contexts for each `x`'s column. If some context is omitted, it
#' will be determined directly from data as explained above.
#'
#' Regardless of the value of the `atomic` argument, all 3 numbers of the
#' context must be provided everytime.
#' @param defaultCenter A value used to determine a typical "medium" value from
#' data (see `context` above). If `context` is not specified then
#' typical "medium" is determined as \deqn{(max(x) - min(x)) * defaultCenter +
#' min(x).} Default value of `defaultCenter` is 0.5, however, some
#' literature specifies 0.42 as another sensible value with proper linguistic
#' interpretation.
#' @param atomic A vector of atomic linguistic expressions to be used for
#' creation of fuzzy attributes. The possible values for `lcut3` are:
#' * `sm` - small;
#' * `me` - medium;
#' * `bi` - big.
#' For `lcut5`, the following values are possible:
#' * `sm` - small;
#' * `lm` - lower medium;
#' * `me` - medium;
#' * `um` - upper medium;
#' * `bi` - big.
#' Several values are allowed in this argument.
#' @param hedges A vector of linguistic hedges to be used for creation of fuzzy
#' attributes.
#'
#' For `lcut3` variant, the following hedges are allowed:
#' * `ex` - extremely (sm, bi);
#' * `si` - significantly (sm, bi);
#' * `ve` - very (sm, bi);
#' * `ml` - more or less (sm, me, bi);
#' * `ro` - roughly (sm, me, bi);
#' * `qr` - quite roughly (sm, me, bi);
#' * `vr` - very roughly (sm, me, bi).
#'
#' For `lcut5` variant, the following hedges are allowed:
#' * exextremely (sm, bi);
#' * vevery (sm, bi);
#' * mlmore or less (sm, #' me, bi);
#' * roroughly (sm, me, bi);
#' * tytypically (me).
#'
#' By default, a fuzzy attribute is created for each atomic expression (i.e.
#' "small", "medium", "big") with empty hedge. Additionally, another fuzzy
#' attributes are created based on the set of hedges selected with this
#' argument. Not all hedges are usable to any atomic expression. In the list
#' above, one can find the allowed atomic expressions in parentheses.
#' @param name A name to be added as a suffix to the created fuzzy attribute
#' names. This parameter can be used only if `x` is a numeric vector. If
#' `x` is a matrix or data frame, `name` should be NULL because the
#' fuzzy attribute names are taken from column names of parameter `x`.
#' @param parallel Whether the processing should be run in parallel or not.
#' Parallelization is implemented using the [foreach::foreach()]
#' package. The parallel environment must be set properly in advance, e.g. with
#' the [doMC::registerDoMC()] function.
#' @param ...  Other parameters to some methods.
#' @return An object of class "fsets" is returned, which is a numeric matrix
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
#' hedges: \eqn{"ex" < "si" < "ve" < "" < "ml" < "ro" < "qr" < "vr"} and
#' \eqn{"ty" < ""}.  Fuzzy attributes created from the same source numeric
#' vector (or column) would be ordered that way, with other fuzzy attributes
#' (from the other source) being incomparable.
#' @author Michal Burda
#' @seealso [fcut()], [farules()], [pbld()], [vars()], [specs()], [cbind.fsets()]
#' @references V. Novak, A comprehensive theory of trichotomous evaluative
#' linguistic expressions, Fuzzy Sets and Systems 159 (22) (2008) 2939--2969.
#' @keywords models robust multivariate
#' @examples
#'
#' # transform a single vector
#' x <- runif(10)
#' lcut3(x, name='age')
#' lcut5(x, name='age')
#'
#'
#' # transform single vector with custom context
#' lcut3(x, context=c(0, 0.2, 0.5), name='age')
#' lcut5(x, context=c(0, 0.2, 0.5), name='age')
#'
#'
#' # transform all columns of a data frame
#' # and do not use any hedges
#' data <- CO2[, c('conc', 'uptake')]
#' lcut3(data, hedges=NULL)
#' lcut5(data, hedges=NULL)
#'
#'
#' # definition of custom contexts for different columns
#' # of a data frame while selecting only "ve" and "ro" hedges.
#' lcut3(data,
#'      context=list(conc=c(0, 500, 1000),
#'                   uptake=c(0, 25, 50)),
#'      hedges=c('ve', 'ro'))
#'
#'
#' # lcut on non-numeric data is the same as fcut()
#' ff <- factor(substring("statistics", 1:10, 1:10), levels = letters)
#' lcut3(ff)
#' lcut5(ff)
#'
#' @export
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


#' @rdname lcut
#' @export
lcut.numeric <- function(x,
                         context=minmax(x),
                         atomic=c('sm', 'me', 'bi', 'lm', 'um', 'ze',
                                  'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um'),
                         hedges=c('ex', 'si', 've', '-', 'ml', 'ro', 'qr', 'vr', 'ty'),
                         name=NULL,
                         ...) {
    atomic <- match.arg(atomic, several.ok=TRUE)
    hedges <- match.arg(hedges, several.ok=TRUE)

    .mustBeNumericVector(x)
    .mustNotBeZeroLength(atomic)
    .mustNotBeZeroLength(hedges)
    .mustNotBeNull(name)

    type <- NULL
    for (clazz in names(.horizonAllowedTable)) {
        if (inherits(context, clazz)) {
            type <- clazz
        }
    }
    .mustBe(!is.null(type),
            paste0("'context' must be an object of one of the following classes: '",
                   paste(names(.horizonAllowedTable), collapse="', '"), "'"))

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

    grid <- expand.grid(hedges, atomic)
    m <- apply(grid, 1, function(row) {
        h <- row[1]
        a <- row[2]
        if (allowed.lingexpr[h, a]) {
            lingexpr(context, atomic=a, hedge=h)(x)
        }
    })
    nulls <- sapply(m, is.null)
    m <- m[!nulls]
    m <- matrix(unlist(m, use.names=FALSE), ncol=length(m), byrow=FALSE)

    n <- apply(grid, 1, function(row) { paste(row, collapse='.')})
    n <- n[!nulls]
    n <- sub('-.', '', n, fixed=TRUE)
    n <- paste(n, name, sep='.')
    colnames(m) <- n

    v <- rep(name, ncol(m))

    s <- matrix(0, nrow=ncol(m), ncol=ncol(m))
    m
}
