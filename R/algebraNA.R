.elementWisely <- function(f) {
    function(...) {
        elts <- list(...)
        if (length(elts) <= 0L) {
            return(NULL)
        }
        vals <- lapply(elts, as.numeric)
        res <- do.call('mapply', c(list(f), vals))
        mostattributes(res) <- attributes(elts[[1L]])
        return(res)
    }
}


.algebraModification <- function(call, algebra, norm, conorm, resid, neg, invol, ord) {
    resN <- neg(algebra$n)
    resNI <- invol(algebra$ni)
    resT <- norm(algebra$t)
    resPT <- .elementWisely(resT)
    resC <- conorm(algebra$c)
    resR <- resid(algebra$r)
    resI <- norm(algebra$i)
    resPI <- .elementWisely(resI)
    resS <- conorm(algebra$s)
    resB <- function(x, y) { resPI(resR(x, y), resR(y, x)) }
    res <- list(n=resN,
                ni=resNI,
                t=resT,
                pt=resPT,
                c=resC,
                pc=.elementWisely(resC),
                r=resR,
                b=resB,
                i=resI,
                pi=resPI,
                s=resS,
                ps=.elementWisely(resS),
                order=ord,
                algebratype=c(algebra$algebratype, call))
    class(res) <- c('algebra', 'list')
    res
}


.neg0 <- function(f) {
    return(function(x) {
        res <- f(x)
        res[is.na(x)] <- 0
        res
    })
}


.neg1 <- function(f) {
    return(function(x) {
        res <- f(x)
        res[is.na(x)] <- 1
        res
    })
}


.negNA <- function(f) {
    return(function(x) {
        res <- f(x)
        res[is.na(x)] <- NA_real_
        res
    })
}


.normKleene <- function(f) {
    return(function(...) {
        dots <- c(...)
        nonadots <- na.omit(dots)
        res <- f(nonadots)
        if (length(dots) != length(nonadots) && !is.na(res) && res > 0) {
            return(NA_real_)
        }
        res
    })
}


.conormKleene <- function(f) {
    return(function(...) {
        dots <- c(...)
        nonadots <- na.omit(dots)
        res <- f(nonadots)
        if (length(dots) != length(nonadots) && !is.na(res) && res < 1) {
            return(NA_real_)
        }
        res
    })
}


.conormDragon <- function(f) {
    return(function(...) {
        dots <- c(...)
        nonadots <- na.omit(dots)
        res <- f(nonadots)
        if (length(dots) != length(nonadots) && !is.na(res) && res == 0) {
            return(NA_real_)
        }
        res
    })
}


.undefinedOrder <- function(x, decreasing=FALSE) {
    stop('Cannot complete the computation, "order" is unimplemented for the algebra.')
}


.dragonflyOrder <- function(x, decreasing=FALSE) {
    large <- sum(!is.na(x) & x > 0)
    zeros <- sum(!is.na(x) & x == 0)
    nas <- sum(is.na(x))
    ordered <- order(x, decreasing=TRUE, na.last=TRUE)
    res <- c(ordered[seq(from=1, length.out=large)],
             ordered[seq(from=large + zeros + 1, length.out=nas)],
             ordered[seq(from=large + 1, length.out=zeros)])
    if (decreasing) {
        return(res)
    } else {
        return(rev(res))
    }
}


#' Modify algebra's way of computing with `NA` values.
#'
#' By default, the objects created with the [algebra()] function represent a mathematical
#' algebra capable to work on the \eqn{[0,1]} interval. If `NA` appears as a value instead,
#' it is propagated to the result. That is, any operation with `NA` results in `NA`, by default.
#' This scheme of handling missing values is also known as Bochvar's. To change this default
#' behavior, the following functions may be applied.
#'
#' The `sobocinski()`, `kleene()`, `nelson()`, `lowerEst()` and `dragonfly()` functions modify the algebra to
#' handle the `NA` in a different way than is the default. Sobocinski's algebra simply ignores `NA` values
#' whereas Kleene's algebra treats `NA` as "unknown value". Dragonfly approach is a combination
#' of Sobocinski's and Bochvar's approach, which preserves the ordering `0 <= NA <= 1`
#' to obtain from compositions (see [compose()])
#' the lower-estimate in the presence of missing values.
#'
#' In detail, the behavior of the algebra modifiers is defined as follows:
#'
#' Sobocinski's negation for `n` being the underlying algebra:
#' \tabular{ll}{
#'   a  \tab n(a)\cr
#'   NA \tab 0
#' }
#'
#' Sobocinski's operation for `op` being one of `t`, `pt`, `c`, `pc`, `i`, `pi`, `s`, `ps`
#' from the underlying algebra:
#' \tabular{lll}{
#'      \tab b        \tab NA\cr
#'   a  \tab op(a, b) \tab a \cr
#'   NA \tab b        \tab NA
#' }
#'
#' Sobocinski's operation for `r` from the underlying algebra:
#' \tabular{lll}{
#'       \tab b       \tab NA  \cr
#'    a  \tab r(a, b) \tab n(a)\cr
#'    NA \tab b       \tab NA
#' }
#'
#' Kleene's negation is identical to `n` from the underlying algebra.
#'
#' Kleene's operation for `op` being one of `t`, `pt`, `i`, `pi` from the underlying algebra:
#' \tabular{llll}{
#'      \tab b        \tab NA \tab 0\cr
#'   a  \tab op(a, b) \tab NA \tab 0\cr
#'   NA \tab NA       \tab NA \tab 0\cr
#'   0  \tab 0        \tab 0  \tab 0
#' }
#'
#' Kleene's operation for `op` being one of `c`, `pc`, `s`, `ps` from the underlying algebra:
#' \tabular{llll}{
#'      \tab b        \tab NA \tab 1\cr
#'   a  \tab op(a, b) \tab NA \tab 1\cr
#'   NA \tab NA       \tab NA \tab 1\cr
#'   1  \tab 1        \tab 1  \tab 1
#' }
#'
#' Kleene's operation for `r` from the underlying algebra:
#' \tabular{llll}{
#'      \tab b        \tab NA \tab 1\cr
#'   a  \tab r(a, b)  \tab NA \tab 1\cr
#'   NA \tab NA       \tab NA \tab 1\cr
#'   0  \tab 1        \tab 1  \tab 1
#' }
#'
#' Dragonfly negation is identical to `n` from the underlying algebra.
#'
#' Dragonfly operation for `op` being one of `t`, `pt`, `i`, `pi` from the underlying algebra:
#' \tabular{lllll}{
#'      \tab b        \tab NA \tab 0 \tab 1\cr
#'   a  \tab op(a, b) \tab NA \tab 0 \tab a\cr
#'   NA \tab NA       \tab NA \tab 0 \tab NA\cr
#'   0  \tab 0        \tab 0  \tab 0 \tab 0\cr
#'   1  \tab b        \tab NA \tab 0 \tab 1
#' }
#'
#' Dragonfly operation for `op` being one of `c`, `pc`, `s`, `ps` from the underlying algebra:
#' \tabular{lllll}{
#'      \tab b        \tab NA \tab 0  \tab 1\cr
#'   a  \tab op(a, b) \tab a  \tab a  \tab 1\cr
#'   NA \tab b        \tab NA \tab NA \tab 1\cr
#'   0  \tab b        \tab NA \tab 0  \tab 1\cr
#'   1  \tab 1        \tab 1  \tab 1  \tab 1
#' }
#'
#' Dragonfly operation for `r` from the underlying algebra:
#' \tabular{lllll}{
#'      \tab b       \tab NA \tab 0    \tab 1\cr
#'   a  \tab r(a, b) \tab NA \tab n(a) \tab 1\cr
#'   NA \tab b       \tab 1  \tab NA   \tab 1\cr
#'   0  \tab 1       \tab 1  \tab 1    \tab 1\cr
#'   1  \tab b       \tab NA \tab 0    \tab 1
#' }
#'
#' @param algebra the underlying algebra object to be modified -- see the [algebra()] function
#' @return A list of function of the same structure as is the list returned from the [algebra()] function
#' @author Michal Burda
#' @keywords models robust
#' @examples
#' a <- algebra('lukas')
#' b <- sobocinski(a)
#'
#' a$t(0.3, NA)  # NA
#' b$t(0.3, NA)  # 0.3
#'
#' @export
#' @importFrom stats na.omit
#' @rdname algebraNA
#' @aliases algebraNA
sobocinski <- function(algebra) {
    .mustBeAlgebra(algebra)

    norm <- function(f) {
        return(function(...) {
            nonadots <- na.omit(c(...))
            if (length(nonadots) <= 0) {
                return(NA_real_)
            }
            f(nonadots)
        })
    }

    resid <- function(f) {
        return(function(x, y) {
            res <- f(x, y)
            xNA <- is.na(x)
            yNA <- is.na(y)
            res[yNA] <- algebra$n(x[yNA])
            res[xNA] <- y[xNA]
            res[xNA & yNA] <- NA_real_
            res
        })
    }

    .algebraModification('sobocinski', algebra, norm, norm, resid, .neg0, .negNA, .undefinedOrder)
}


#' @export
#' @rdname algebraNA
kleene <- function(algebra) {
    .mustBeAlgebra(algebra)

    resid <- function(f) {
        return(function(x, y) {
            res <- f(x, y)
            res[is.na(x)] <- NA_real_
            res[is.na(y)] <- NA_real_
            res[x == 0] <- 1
            res[y == 1] <- 1
            res
        })
    }

    .algebraModification('kleene', algebra, .normKleene, .conormKleene, resid, .negNA, .negNA, .undefinedOrder)
}


#' @export
#' @rdname algebraNA
dragonfly <- function(algebra) {
    .mustBeAlgebra(algebra)

    resid <-function(f) {
        return(function(x, y) {
            res <- f(x, y)
            xNA <- is.na(x)
            yNA <- is.na(y)
            x0 <- !xNA & x == 0
            y0 <- !yNA & y == 0
            res[xNA] <- y[xNA]
            res[yNA] <- NA_real_
            res[xNA & yNA] <- 1
            res[yNA & x0] <- 1
            res[xNA & y0] <- NA_real_
            return(res)
        })
    }

    alg <- .algebraModification('dragonfly', algebra, .normKleene, .conormDragon, resid, .negNA, .negNA, .dragonflyOrder)
    return(alg)
}


#' @export
#' @rdname algebraNA
nelson <- function(algebra) {
    .mustBeAlgebra(algebra)

    resid <- function(f) {
        return(function(x, y) {
            res <- f(x, y)
            xNA <- is.na(x)
            yNA <- is.na(y)
            x0 <- !xNA & x == 0
            y0 <- !yNA & y == 0
            y1 <- !yNA & y == 1
            res[yNA] <- NA_real_
            res[x0 & yNA] <- 1
            res[xNA] <- 1
            res[xNA & !(y0 | y1 | yNA)] <- NA_real_
            res
        })
    }

    .algebraModification('nelson', algebra, .normKleene, .conormKleene, resid, .neg1, .negNA, .undefinedOrder)
}


#' @export
#' @rdname algebraNA
lowerEst <- function(algebra) {
    .mustBeAlgebra(algebra)

    resid <-function(f) {
        return(function(x, y) {
            res <- f(x, y)
            xNA <- is.na(x)
            yNA <- is.na(y)
            x0 <- !xNA & x == 0
            res[yNA] <- NA_real_
            res[yNA & x0] <- 1
            res[xNA] <- y[xNA]
            res
        })
    }

    alg <- .algebraModification('lowerEst', algebra, .normKleene, .conormDragon, resid, .neg0, .negNA, .dragonflyOrder)
    return(alg)
}
