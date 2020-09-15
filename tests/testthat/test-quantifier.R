test_that("quantifier: all", {
    q <- quantifier('all', weighting=TRUE, alg='lukas')

    expect_equal(q(c(0, 0, 0), 1), 0)
    expect_equal(q(c(0, 0.1, 0), 1), 0)
    expect_equal(q(c(1, 0.9, 1), 1), 0.9)
    expect_equal(q(c(0.5, 1, 0.8), 1), 0.5)

    expect_equal(q(c(0.1, 0.3, 1), c(0, 0.9, 0.5)), 0.3)
    expect_equal(q(c(0.1, 0.3, 1), c(0.01, 0.9, 0.5)), 0.1)
})
