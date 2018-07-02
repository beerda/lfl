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


.algebraModification <- function(algebra, norm, conorm, resid) {
    resT <- norm(algebra$t)
    resPT <- .elementWisely(resT)
    resC <- conorm(algebra$c)
    resR <- resid(algebra$r)
    resB <- function(x, y) { resPT(resR(x, y), resR(y, x)) }
    resI <- norm(algebra$i)
    resS <- conorm(algebra$s)
    return(list(n=algebra$n,
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
            res[naY] <- invol.neg(x[naY])
            res[naX] <- y[naX]
            res
        })
    }

    .algebraModification(algebra, norm, norm, resid)
}
