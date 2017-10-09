test_that('smape', {
    f <- 1:10 + 0.2
    r <- 1:10

    expect_equal(smape(f, r), mean(0.2 / ((f+r) / 2)))
    expect_error(smape(1:10, 1:9))
    expect_error(smape(1, 2))
    expect_error(smape(c(1:5, NA), 1:6))
})
