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


#' @author Michal Burda
#' @keywords models robust
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
lowerEst <- function(algebra) {
    neg <- function(f) {
        return(f)
    }

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

    alg <- .algebraModification(algebra, norm, conorm, resid, neg)
    alg$b <- function(x, y) { stop('lowerEst bi-residuum not implemented') }
    return(alg)
}
