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

    yy <- predict(res, x, xmemb)
    expect_equal(yy, y)
})
