test_that("sugeno", {
    q <- sugeno(identity, relative=TRUE, strong=FALSE, alg='lukasiewicz')

    expect_equal(q(c(1, 1, 1, 1)), 1)
    expect_equal(q(c(1, 1, 1, 0)), 0.75)
    expect_equal(q(c(0, 0, 0, 0)), 0)
    expect_equal(q(c(1, 0.6, 1, 0)), 0.6)
    expect_equal(q(c(1, 0.4, 1, 0)), 0.5)
})
