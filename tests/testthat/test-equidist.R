test_that('equidist', {
    expect_equal(equidist(10:20, 2, left = 'none', right = 'none'),
                 c(10, 20))

    expect_equal(equidist(10:20, 3, left = 'none', right = 'none'),
                 c(10, 15, 20))
    expect_equal(equidist(10:20, 3, left = 'infinity', right = 'none'),
                 c(-Inf, 10, 15, 20))
    expect_equal(equidist(10:20, 3, left = 'same', right = 'none'),
                 c(10, 10, 15, 20))
    expect_equal(equidist(10:20, 3, left = 'none', right = 'infinity'),
                 c(10, 15, 20, Inf))
    expect_equal(equidist(10:20, 3, left = 'none', right = 'same'),
                 c(10, 15, 20, 20))
    expect_equal(equidist(10:20, 3, left = 'infinity', right = 'infinity'),
                 c(-Inf, 10, 15, 20, Inf))
    expect_equal(equidist(10:20, 3, left = 'same', right = 'same'),
                 c(10, 10, 15, 20, 20))

    expect_equal(equidist(10:20, 11, left = 'none', right = 'none'),
                 10:20)
})


test_that('equidist 2', {
    expect_equal(equidist(c(10, 18, 20), 2, left = 'none', right = 'none'),
                 c(10, 20))

    expect_equal(equidist(c(10, 18, 20), 3, left = 'none', right = 'none'),
                 c(10, 15, 20))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'infinity', right = 'none'),
                 c(-Inf, 10, 15, 20))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'same', right = 'none'),
                 c(10, 10, 15, 20))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'none', right = 'infinity'),
                 c(10, 15, 20, Inf))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'none', right = 'same'),
                 c(10, 15, 20, 20))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'infinity', right = 'infinity'),
                 c(-Inf, 10, 15, 20, Inf))
    expect_equal(equidist(c(10, 18, 20), 3, left = 'same', right = 'same'),
                 c(10, 10, 15, 20, 20))

    expect_equal(equidist(c(10, 18, 20), 11, left = 'none', right = 'none'),
                 10:20)
})
