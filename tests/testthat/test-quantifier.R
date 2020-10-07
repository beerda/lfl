test_that("quantifier: all", {
    q <- quantifier('all', alg='lukas')

    expect_equal(q(0, 1), 0)
    expect_equal(q(0.5, 1), 0.5)
    expect_equal(q(1, 1), 1)

    expect_equal(q(c(0, 0, 0), 1), 0)
    expect_equal(q(c(0, 0.1, 0), 1), 0)
    expect_equal(q(c(1, 0.9, 1), 1), 0.9)
    expect_equal(q(c(0.5, 1, 0.8), 1), 0.5)

    expect_equal(q(c(0.1, 0.3, 1), c(0, 0.9, 0.5)), 0.3)
    expect_equal(q(c(0.1, 0.3, 1), c(0.01, 0.9, 0.5)), 0.1)
})


test_that("quantifier: some", {
    q <- quantifier('some', alg='lukas')

    expect_equal(q(0, 1), 0)
    expect_equal(q(0.5, 1), 0.5)
    expect_equal(q(1, 1), 1)

    expect_equal(q(c(0, 0, 0), 1), 0)
    expect_equal(q(c(0, 0.1, 0), 1), 0.1)
    expect_equal(q(c(0.5, 0.3, 0.8), 1), 0.8)
    expect_equal(q(c(0.5, 1, 0.8), 1), 1)

    expect_equal(q(c(1, 0.3, 0.2), c(0, 0.9, 0.5)), 0.3)
    expect_equal(q(c(1, 0.3, 0.2), c(0.01, 0.9, 0.5)), 1)
})


test_that("quantifier: at.least", {
    q <- quantifier('at.least', n=2, alg='lukas')

    expect_equal(q(0, 1), 0)
    expect_equal(q(0.5, 1), 0)
    expect_equal(q(1, 1), 0)

    expect_equal(q(c(0, 0, 0), 1), 0)
    expect_equal(q(c(0, 0.1, 0), 1), 0)
    expect_equal(q(c(0.5, 0.1, 0), 1), 0.1)
    expect_equal(q(c(0.5, 0.3, 0.8), 1), 0.5)
    expect_equal(q(c(0.5, 1, 0.8), 1), 0.8)

    expect_equal(q(c(1, 0.3, 0.2), c(0, 0.9, 0.5)), 0)
    expect_equal(q(c(1, 0.3, 0.2, 0.9), c(0, 0.9, 0.5, 1)), 0.2)
    expect_equal(q(c(1, 0.3, 0.2, 0.9), c(0.11, 0.9, 0.5, 1)), 0.3)
})

test_that("bochvar quantifier", {
    for (quant in c('all', 'almost.all', 'most', 'many', 'some')) {
        for (alg in c('lukas', 'goedel', 'goguen')) {
            q <- quantifier('all', alg='lukas')
            expect_equal(q(NA_real_), NA_real_)
            expect_equal(q(NA_real_, 1), NA_real_)
            expect_equal(q(NA_real_, 0.3), NA_real_)
            expect_equal(q(NA_real_, 0), NA_real_)
            expect_equal(q(c(0.1, NA, 1), 1), NA_real_)
            expect_equal(q(c(0.1, NA, 1), c(0.01, 0.9, 0.5)), NA_real_)
            expect_error(q(1, NA))
        }
    }
})
