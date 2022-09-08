test_that('ft', {
    y <- (1:30)^2
    x <- data.frame(a = 1:30, b = 30:1)
    x <- as.matrix(x)

    xmemb <- fcut(x,
                  breaks = list(a = equidist(x[, 'a'], 3),
                                b = equidist(x[, 'b'], 3)))

    res <- ft(x, xmemb, y, order = 2)
    expect_true(is.ft(res))
    expect_true(is.list(res))
    expect_equal(res$inputs, c('a', 'b'))
    expect_equal(res$partitions,
                 list(c('a=1', 'a=2', 'a=3'), c('b=1', 'b=2', 'b=3')))
    expect_equal(res$order, 2)
    expect_equal(nrow(res$antecedents), 9)
    expect_equal(ncol(res$antecedents), 2)
    expect_equal(nrow(res$consequents), 5)
    expect_equal(ncol(res$consequents), 9)

    yy <- ftinv(res, x, xmemb)
    expect_equal(yy, y)
})


test_that('ft extrapolation 1', {
    y <- (1:30)
    x <- data.frame(a = 1:30)
    x <- as.matrix(x)
    xbreaks <- equidist(x[, 'a'], 3, left='infinity', right='infinity')
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    res <- ft(x, xmemb, y, order = 1)

    x <- data.frame(a = 20:50)
    x <- as.matrix(x)
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    yy <- ftinv(res, x, xmemb)
    expect_equal(yy, 20:50)
})


test_that('ft extrapolation 2', {
    y <- c(1:10, 101:110, 201:210)
    x <- data.frame(a = 1:30)
    x <- as.matrix(x)
    xbreaks <- equidist(x[, 'a'], 9, left='infinity', right='infinity')
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    res <- ft(x, xmemb, y, order = 1)

    x <- data.frame(a = 21:50)
    x <- as.matrix(x)
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    yy <- ftinv(res, x, xmemb)
    expect_equal(yy[11:30], 211:230)
})


test_that('ft extrapolation to NaN', {
    y <- (1:30)
    x <- data.frame(a = 1:30)
    x <- as.matrix(x)
    xbreaks <- equidist(x[, 'a'], 3, left='same', right='same')
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    res <- ft(x, xmemb, y, order = 1)

    x <- data.frame(a = 20:50)
    x <- as.matrix(x)
    xmemb <- fcut(x, breaks = list(a = xbreaks))

    yy <- ftinv(res, x, xmemb)
    expect_equal(yy, c(20:30, rep(NaN, 20)))


})


test_that('ft order 0', {
    y <- rep(10, 30)
    x <- data.frame(a = 1:30, b = 30:1)
    x <- as.matrix(x)

    xmemb <- fcut(x,
                  breaks = list(a = equidist(x[, 'a'], 3),
                                b = equidist(x[, 'b'], 3)))

    res <- ft(x, xmemb, y, order = 0)
    expect_true(is.ft(res))
    expect_true(is.list(res))
    expect_equal(res$inputs, c('a', 'b'))
    expect_equal(res$partitions,
                 list(c('a=1', 'a=2', 'a=3'), c('b=1', 'b=2', 'b=3')))
    expect_equal(res$order, 0)
    expect_equal(nrow(res$antecedents), 9)
    expect_equal(ncol(res$antecedents), 2)
    expect_equal(nrow(res$consequents), 1)
    expect_equal(ncol(res$consequents), 9)

    yy <- ftinv(res, x, xmemb)
    expect_equal(yy, y)
})

