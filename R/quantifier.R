#' @export
quant <- function(measure,
                  relative=TRUE,
                  alg=c('lukasiewicz', 'goedel', 'goguen')) {
    .mustBeFunction(measure)
    .mustBeLogicalScalar(relative)
    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    relate <- if (relative) function(u) { u / u[length(u)] } else identity

    function(x, u=1) {
        .mustBeNumericVector(x)
        .mustBeNumericVector(u)

        l <- max(length(x), length(u))
        x <- rep_len(x, l)
        u <- rep_len(u, l)
        o <- alg$order(x, decreasing=TRUE)
        m <- measure(relate(cumsum(u[o])))
        alg$s(alg$pi(x[o], m))
    }
}


#' @export
quantifier <- function(quantity=c('all', 'almost.all', 'most', 'many', 'some', 'at.least'),
                       context=ctx3(),
                       n=0,
                       alg=c('lukasiewicz', 'goedel', 'goguen')) {
    quantity <- match.arg(quantity)
    .mustBeNumericScalar(n)

    m <- NULL
    r <- NULL
    if (quantity == 'all') {
        m <- function(x) { (x >= 1) + 0 }
        r <- TRUE
    } else if (quantity == 'almost.all') {
        m <- lingexpr(context=context, atomic='bi', hedge='ex')
        r <- TRUE
    } else if (quantity == 'most') {
        m <- lingexpr(context=context, atomic='bi', hedge='ve')
        r <- TRUE
    } else if (quantity == 'many') {
        m <- lingexpr(context=context, atomic='sm', hedge='-', negated=TRUE)
        r <- TRUE

    # quantifiers with decreasing measure function are still unclear to me
    #} else if (quantity == 'few') {
        #m <- lingexpr(context=context, atomic='sm', hedge='si')
        #r <- TRUE
    #} else if (quantity == 'several') {
        #m <- lingexpr(context=context, atomic='sm', hedge='ve')
        #r <- TRUE
    #} else if (quantity == 'at.most') {
        #m <- function(x) { (x <= n) + 0 }
        #r <- FALSE

    } else if (quantity == 'some') {
        m <- function(x) { (x > 0) + 0 }
        r <- FALSE
    } else if (quantity == 'at.least') {
        m <- function(x) { (x >= n) + 0 }
        r <- FALSE
    } else {
        .stop(paste0('Unknown quantity: ', quantity))
    }

    quant(measure=m, relative=r, alg=alg)
}
