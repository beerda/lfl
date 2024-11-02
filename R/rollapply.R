.rollapply <- function(x, window, f, ...) {
    if (length(x) < window) {
        return(NULL)
    }

    res <- sapply(seq_len(length(x) - window + 1), function(i) {
        ii <- seq(from = i, to = i + window - 1)
        f(x[ii], ...)
    })

    if (is.matrix(res)) {
        res <- t(res)
    }

    res
}
