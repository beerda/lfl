test_that('rmse', {
    f <- 1:10 + 0.2
    r <- 1:10

    expect_equal(rmse(f, r), 0.2)
    expect_error(rmse(1:10, 1:9))
    expect_error(rmse(1, 2))
    expect_error(rmse(c(1:5, NA), 1:6))
})
