test_that('hedge', {
    expect_that(hedge('-')(c(-Inf, -1, 0, 1, 10, Inf, NA, NaN)),
                equals(c(0, 0, 0, 1, 1, 1, NA, NaN)))
})

