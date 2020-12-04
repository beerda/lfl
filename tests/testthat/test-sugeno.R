test_that("sugeno bochvar", {
    a <- algebra('lukasiewicz')
    q <- sugeno(identity, relative=TRUE, strong=FALSE, alg=a)

    expect_equal(q(c(1, 1, 1, 1)), 1)
    expect_equal(q(c(1, 1, 1, 0)), 0.75)
    expect_equal(q(c(0, 0, 0, 0)), 0)
    expect_equal(q(c(1, 0.6, 1, 0)), 0.6)
    expect_equal(q(c(1, 0.4, 1, 0)), 0.5)

    expect_equal(q(c(1, 1, NA, 1)), NA_real_)
    expect_equal(q(c(0, 0, NA, 0)), NA_real_)
})


test_that("sugeno sobocinski", {
    a <- sobocinski(algebra('lukasiewicz'))
    q <- sugeno(identity, relative=TRUE, strong=FALSE, alg=a)

    expect_equal(q(c(1, 1, 1, 1)), 1)
    expect_equal(q(c(1, 1, 1, 0)), 0.75)
    expect_equal(q(c(0, 0, 0, 0)), 0)
    expect_equal(q(c(1, 0.6, 1, 0)), 0.6)
    expect_equal(q(c(1, 0.4, 1, 0)), 0.5)

    expect_equal(q(c(1, NA, 1, 1, 1)), 1)
    expect_equal(q(c(1, 1, 1, 0, NA)), 0.75)
    expect_equal(q(c(0, 0, 0, NA, 0)), 0)
    expect_equal(q(c(1, NA, NA, 0.6, 1, 0)), 0.6)
    expect_equal(q(c(NA, 1, 0.4, 1, 0)), 0.5)
})
