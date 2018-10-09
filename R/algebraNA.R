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


.algebraModification <- function(algebra, norm, conorm, resid, neg) {
    resN <- neg(algebra$n)
    resT <- norm(algebra$t)
    resPT <- .elementWisely(resT)
    resC <- conorm(algebra$c)
    resR <- resid(algebra$r)
    resB <- function(x, y) { resPT(resR(x, y), resR(y, x)) }
    resI <- norm(algebra$i)
    resS <- conorm(algebra$s)
    return(list(n=resN,
                t=resT,
                pt=resPT,
                c=resC,
                pc=.elementWisely(resC),
                r=resR,
                b=resB,
                i=resI,
                pi=.elementWisely(resI),
                s=resS,
                ps=.elementWisely(resS)))
}


#' Modify algebra's way of computing with `NA` values.
#'
#' By default, the objects created with the [algebra()] function represent a mathematical
#' algebra capable to work on the \eqn{[0,1]} interval. If `NA` appears as a value instead,
#' it is propagated to the result. That is, any operation with `NA` results in `NA`, by default.
#' This scheme of handling missing values is also known as Bochvar's.
#'
#' The `sobocinski()`, `kleene()` and `dragonfly()` functions modify the algebra to
#' handle the `NA` in a different way than default. Sobocinski's algebra simply ignores `NA` values
#' whereas Kleene's algebra treats `NA` as "unknown value". Dragonfly approach is a combination
#' of Sobocinski's and Bochvar's approach, which preserves the ordering `0 <= NA <= 1`
#' to obtain from compositions (see [compose()])
#' the lower-estimate in the presence of missing values.
#'
#' In detail, the behaviour of the algebra modifiers is defined as follows:
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
sobocinski <- function(algebra) {
    neg <- function(f) {
        return(function(x) {
            res <- f(x)
            res[is.na(x)] <- 0
            res
        })
    }

    norm <- function(f) {
        return(function(...) {
            f(na.omit(c(...)))
        })
    }

    resid <- function(f) {
        return(function(x, y) {
            res <- f(x, y)
            naX <- is.na(x)
            naY <- is.na(y)
            res[naY] <- algebra$n(x[naY])
            res[naX] <- y[naX]
            res
        })
    }

    .algebraModification(algebra, norm, norm, resid, neg)
}


#' @export
#' @rdname sobocinski
kleene <- function(algebra) {
    norm <- function(f) {
        return(function(...) {
            dots <- c(...)
            res <- f(na.omit(dots))
            if (!is.na(res) && res > 0 && any(is.na(dots))) {
                return(NA_real_)
            }
            return(res)
        })
    }

    conorm <- function(f) {
        return(function(...) {
            dots <- c(...)
            res <- f(na.omit(dots))
            if (!is.na(res) && res < 1 && any(is.na(dots))) {
                return(NA_real_)
            }
            return(res)
        })
    }

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

    .algebraModification(algebra, norm, conorm, resid, identity)
}


#' @export
#' @rdname sobocinski
dragonfly <- function(algebra) {
    norm <- function(f) {
        return(function(...) {
            dots <- c(...)
            res <- f(na.omit(dots))
            if (!is.na(res) && res > 0 && any(is.na(dots))) {
                return(NA_real_)
            }
            return(res)
        })
    }

    conorm <- function(f) {
        return(function(...) {
            dots <- c(...)
            res <- f(na.omit(dots))
            if (!is.na(res) && res == 0 && any(is.na(dots))) {
                return(NA_real_)
            }
            return(res)
        })
    }

    resid <-function(f) {
        return(function(x, y) {
            res <- f(x, y)
            naX <- is.na(x)
            naY <- is.na(y)
            res[naX] <- y[naX]
            res[naY] <- NA_real_
            res[naX & naY] <- 1
            res[naY & x == 0] <- 1
            res[naX & y == 0] <- NA_real_
            return(res)
        })
    }

    alg <- .algebraModification(algebra, norm, conorm, resid, identity)
    alg$b <- function(x, y) { stop('dragonfly bi-residuum not implemented') }
    return(alg)
}
