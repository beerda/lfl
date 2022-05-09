test_that('equifreq', {
    expect_equal(equifreq(10:20, 2, left = 'none', right = 'none'),
                 c(10, 20))

    expect_equal(equifreq(10:20, 3, left = 'none', right = 'none'),
                 c(10, 15, 20))
    expect_equal(equifreq(10:20, 3, left = 'infinity', right = 'none'),
                 c(-Inf, 10, 15, 20))
    expect_equal(equifreq(10:20, 3, left = 'same', right = 'none'),
                 c(10, 10, 15, 20))
    expect_equal(equifreq(10:20, 3, left = 'none', right = 'infinity'),
                 c(10, 15, 20, Inf))
    expect_equal(equifreq(10:20, 3, left = 'none', right = 'same'),
                 c(10, 15, 20, 20))
    expect_equal(equifreq(10:20, 3, left = 'infinity', right = 'infinity'),
                 c(-Inf, 10, 15, 20, Inf))
    expect_equal(equifreq(10:20, 3, left = 'same', right = 'same'),
                 c(10, 10, 15, 20, 20))

    expect_equal(equifreq(10:20, 11, left = 'none', right = 'none'),
                 10:20)
})


test_that('equifreq 2', {
    expect_equal(equifreq(c(10, 18, 20), 2, left = 'none', right = 'none'),
                 c(10, 20))

    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'none', right = 'none'),
                 c(10, 18, 20))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'infinity', right = 'none'),
                 c(-Inf, 10, 18, 20))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'same', right = 'none'),
                 c(10, 10, 18, 20))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'none', right = 'infinity'),
                 c(10, 18, 20, Inf))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'none', right = 'same'),
                 c(10, 18, 20, 20))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'infinity', right = 'infinity'),
                 c(-Inf, 10, 18, 20, Inf))
    expect_equal(equifreq(c(10, 11, 18, 19, 20), 3, left = 'same', right = 'same'),
                 c(10, 10, 18, 20, 20))

    expect_equal(equifreq(c(10, 18, 20), 3, left = 'none', right = 'none'),
                 c(10, 18, 20))

})
