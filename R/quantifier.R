#' @export
quant <- function(measure,
                  weighting=TRUE,
                  relative=TRUE,
                  alg=c('goedel', 'goguen', 'lukasiewicz')) {
    .mustBeFunction(measure)
    .mustBeLogicalScalar(weighting)
    .mustBeLogicalScalar(relative)
    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    cumcard <- if (weighting) cumsum else seq_along
    relate <- if (relative) function(u) { u / u[length(u)] } else identity

    function(x, u) {
        .mustBeNumericVector(x)
        .mustBeNumericVector(u)

        l <- max(length(x), length(u))
        x <- rep_len(x, l)
        u <- rep_len(u, l)
        o <- alg$order(x, decreasing=TRUE)
        m <- measure(relate(cumcard(u[o])))
        alg$s(alg$pi(x[o], m))
    }
}


#' @export
quantifier <- function(type=c('all', 'almost.all', 'most', 'many', 'few', 'several', 'some', 'at.least', 'at.most'),
                       context=ctx3(),
                       n=0,
                       weighting=TRUE,
                       alg=c('goedel', 'goguen', 'lukasiewicz')) {
    type <- match.arg(type)
    .mustBeNumericScalar(n)

    m <- NULL
    r <- NULL
    if (type == 'all') {
        m <- function(x) { (x >= 1) + 0 }
        r <- TRUE
    } else if (type == 'almost.all') {
        m <- lingexpr(context=context, atomic='bi', hedge='ex')
        r <- TRUE
    } else if (type == 'most') {
        m <- lingexpr(context=context, atomic='bi', hedge='ve')
        r <- TRUE
    } else if (type == 'many') {
        m <- lingexpr(context=context, atomic='sm', hedge='-', negated=TRUE)
        r <- TRUE
    } else if (type == 'few') {
        m <- lingexpr(context=context, atomic='sm', hedge='si')
        r <- TRUE
    } else if (type == 'several') {
        m <- lingexpr(context=context, atomic='sm', hedge='ve')
        r <- TRUE
    } else if (type == 'some') {
        m <- function(x) { (x > 0) + 0 }
        r <- FALSE
    } else if (type == 'at.least') {
        m <- function(x) { (x >= n) + 0 }
        r <- FALSE
    } else if (type == 'at.most') {
        m <- function(x) { (x <= n) + 0 }
        r <- FALSE
    } else {
        .stop(paste0('Unknown quantifier type: ', type))
    }

    quant(measure=m, relative=r, weighting=weighting, alg=alg)
}
