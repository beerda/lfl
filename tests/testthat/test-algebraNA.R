set.seed(34523)

test_that('sobocinski', {
    a <- sobocinski(algebra('goguen'))

    expect_that(a$t(), equals(NA_real_))
    expect_that(a$t(NA), equals(NA_real_))
    expect_that(a$t(NaN), equals(NA_real_))
    expect_that(a$t(0.5, NA, NaN, 0.8, NA), equals(0.4))
    expect_that(a$t(c(0.5, NA, NA, 0.8, NA)), equals(0.4))

    expect_true(is.null(a$pt()))
    expect_that(a$pt(NA, 0.8), equals(0.8))
    expect_that(a$pt(NaN, 0.8), equals(0.8))
    expect_that(a$pt(c(0.5, NA, 0.4), c(NA, 0.4, 0.5)), equals(c(0.5, 0.4, 0.2)))


    expect_that(a$c(), equals(NA_real_))
    expect_that(a$c(NA), equals(NA_real_))
    expect_that(a$c(NaN), equals(NA_real_))
    expect_that(a$c(0.5, NA, NaN, 0.8, NA), equals(0.9))
    expect_that(a$c(c(0.5, NA, NA, 0.8, NA)), equals(0.9))

    expect_true(is.null(a$pc()))
    expect_that(a$pc(NA, 0.8), equals(0.8))
    expect_that(a$pc(NaN, 0.8), equals(0.8))
    expect_that(a$pc(c(0.5, NA, 0.4), c(NA, 0.4, 0.5)), equals(c(0.5, 0.4, 0.7)))

    expect_that(a$r(c(0.5, 0.8,    NA, 0.3, NA),
                    c(0.8, 0.5,   0.4,  NA, NA)),
             equals(c(1.0, 0.625, 0.4,   0, NA_real_)))

    expect_that(a$b(c(0.5,   0.8,    NA,  0.4,  NA),
                    c(0.8,   0.5,   0.4,   NA,  NA)),
             equals(c(0.625, 0.625,   0,    0, NA_real_)))

    expect_that(a$i(), equals(NA_real_))
    expect_that(a$i(NA), equals(NA_real_))
    expect_that(a$i(NaN), equals(NA_real_))
    expect_that(a$i(0.5, NA, NaN, 0.8, NA), equals(0.5))
    expect_that(a$i(c(0.5, NA, NA, 0.8, NA)), equals(0.5))

    expect_true(is.null(a$pi()))
    expect_that(a$pi(NA, 0.8), equals(0.8))
    expect_that(a$pi(NaN, 0.8), equals(0.8))
    expect_that(a$pi(c(0.5, NA, 0.4), c(NA, 0.4, 0.5)), equals(c(0.5, 0.4, 0.4)))

    expect_that(a$s(), equals(NA_real_))
    expect_that(a$s(NA), equals(NA_real_))
    expect_that(a$s(NaN), equals(NA_real_))
    expect_that(a$s(0.5, NA, NaN, 0.8, NA), equals(0.8))
    expect_that(a$s(c(0.5, NA, NA, 0.8, NA)), equals(0.8))

    expect_true(is.null(a$ps()))
    expect_that(a$ps(NA, 0.8), equals(0.8))
    expect_that(a$ps(NaN, 0.8), equals(0.8))
    expect_that(a$ps(c(0.5, NA, 0.4), c(NA, 0.4, 0.5)), equals(c(0.5, 0.4, 0.5)))

    expect_that(a$n(NA), equals(a$r(NA, 0)))
    expect_that(a$n(1), equals(a$r(1, 0)))
    expect_that(a$n(0), equals(a$r(0, 0)))
    expect_that(a$n(0.8), equals(a$r(0.8, 0)))
})


test_that('kleene', {
    a <- kleene(algebra('goguen'))

    expect_that(a$t(), equals(NA_real_))
    expect_that(a$t(NA), equals(NA_real_))
    expect_that(a$t(NaN), equals(NA_real_))
    expect_that(a$t(0.5, NA), equals(NA_real_))
    expect_that(a$t(0.5, NaN), equals(NA_real_))
    expect_that(a$t(0, NA), equals(0))
    expect_that(a$t(1, NA), equals(NA_real_))
    expect_that(a$t(0.5, 0.4), equals(0.2))
    expect_that(a$t(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$t(c(0, 0.5, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pt()))
    expect_that(a$pt(NA, 0.8), equals(NA_real_))
    expect_that(a$pt(NaN, 0.8), equals(NA_real_))
    expect_that(a$pt(c(0.5, NA, 0.4, NA, NA),
                     c(NA, 0.4, 0.5, NA, 0)),
                equals(c(NA_real_, NA_real_, 0.2, NA_real_, 0)))

    expect_that(a$c(), equals(NA_real_))
    expect_that(a$c(NA), equals(NA_real_))
    expect_that(a$c(NaN), equals(NA_real_))
    expect_that(a$c(NA, 0), equals(NA_real_))
    expect_that(a$c(NA, 0.5), equals(NA_real_))
    expect_that(a$c(NA, 1), equals(1))
    expect_that(a$c(0.5, NA, NaN, 0.8, NA), equals(NA_real_))
    expect_that(a$c(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$c(c(1, 0.5, NA, NA, 0.8, NA)), equals(1))

    expect_true(is.null(a$pc()))
    expect_that(a$pc(NA, 0.8), equals(NA_real_))
    expect_that(a$pc(NaN, 0.8), equals(NA_real_))
    expect_that(a$pc(NA, 0), equals(NA_real_))
    expect_that(a$pc(NA, 1), equals(1))
    expect_that(a$pc(c(0.5, NA, 0.4, 1),
                     c(NA, 0.4, 0.5, NA)),
                equals(c(NA_real_, NA_real_, 0.7, 1)))

    expect_that(a$r(c(0.5, 0.8,   NA,      0.3,       NA,      0, NA),
                    c(0.8, 0.5,   0.4,      NA,       NA,      NA, 1)),
             equals(c(1.0, 0.625, NA_real_, NA_real_, NA_real_, 1, 1)))

    expect_that(a$b(c(0.5,   0.8,   NA,       0.4,      NA,       0,        1),
                    c(0.8,   0.5,   0.4,      NA,       NA,       NA,       NA)),
             equals(c(0.625, 0.625, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)))

    expect_that(a$i(), equals(NA_real_))
    expect_that(a$i(NA), equals(NA_real_))
    expect_that(a$i(NaN), equals(NA_real_))
    expect_that(a$i(0.5, NA, NaN, 0.8, NA), equals(NA_real_))
    expect_that(a$i(0, 0.5, NA, NaN, 0.8, NA), equals(0))
    expect_that(a$i(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$i(c(0.5, 0, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pi()))
    expect_that(a$pi(NA, 0.8), equals(NA_real_))
    expect_that(a$pi(NaN, 0.8), equals(NA_real_))
    expect_that(a$pi(NA, 1), equals(NA_real_))
    expect_that(a$pi(NA, 0), equals(0))
    expect_that(a$pi(c(0.5, NA, 0.4, 0, 1),
                     c(NA, 0.4, 0.5, NA, NA)),
                equals(c(NA_real_, NA_real_, 0.4, 0, NA_real_)))

    expect_that(a$s(), equals(NA_real_))
    expect_that(a$s(NA), equals(NA_real_))
    expect_that(a$s(NaN), equals(NA_real_))
    expect_that(a$s(0.5, NA, NaN, 0.8, NA), equals(NA_real_))
    expect_that(a$s(1, 0.5, NA, NaN, 0.8, NA), equals(1))
    expect_that(a$s(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$s(c(0.5, 1, NA, NA, 0.8, NA)), equals(1))

    expect_true(is.null(a$ps()))
    expect_that(a$ps(NA, 0.8), equals(NA_real_))
    expect_that(a$ps(NaN, 0.8), equals(NA_real_))
    expect_that(a$ps(NA, 0), equals(NA_real_))
    expect_that(a$ps(NA, 1), equals(1))
    expect_that(a$ps(c(0.5, NA, 0.4, NA, NA),
                     c(NA, 0.4, 0.5, 0, 1)),
                equals(c(NA_real_, NA_real_, 0.5, NA_real_, 1)))

    expect_that(a$n(NA), equals(a$r(NA, 0)))
    expect_that(a$n(1), equals(a$r(1, 0)))
    expect_that(a$n(0), equals(a$r(0, 0)))
    expect_that(a$n(0.8), equals(a$r(0.8, 0)))

})


test_that('dragonfly', {
    a <-dragonfly(algebra('goguen'))

    expect_that(a$t(), equals(NA_real_))
    expect_that(a$t(NA), equals(NA_real_))
    expect_that(a$t(NaN), equals(NA_real_))
    expect_that(a$t(0.5, NA), equals(NA_real_))
    expect_that(a$t(0.5, NaN), equals(NA_real_))
    expect_that(a$t(0, NA), equals(0))
    expect_that(a$t(1, NA), equals(NA_real_))
    expect_that(a$t(0.5, 0.4), equals(0.2))
    expect_that(a$t(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$t(c(0, 0.5, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pt()))
    expect_that(a$pt(NA, 0.8), equals(NA_real_))
    expect_that(a$pt(NaN, 0.8), equals(NA_real_))
    expect_that(a$pt(c(0.5, NA, 0.4, NA, NA),
                     c(NA, 0.4, 0.5, NA, 0)),
                equals(c(NA_real_, NA_real_, 0.2, NA_real_, 0)))

    expect_that(a$c(), equals(NA_real_))
    expect_that(a$c(NA), equals(NA_real_))
    expect_that(a$c(NaN), equals(NA_real_))
    expect_that(a$c(0.5, 0.8), equals(0.9))
    expect_that(a$c(NA, 0.5), equals(0.5))
    expect_that(a$c(NA, 1), equals(1))
    expect_that(a$c(NA, NA), equals(NA_real_))
    expect_that(a$c(NA, 0), equals(NA_real_))
    expect_that(a$c(0, NA), equals(NA_real_))
    expect_that(a$c(0, 0), equals(0))
    expect_that(a$c(0.5, NA, NaN, 0.8, NA), equals(0.9))
    expect_that(a$c(c(0.5, NA, NA, 0.8, NA)), equals(0.9))
    expect_that(a$c(c(1, 0.5, NA, NA, 0.8, NA)), equals(1))

    expect_true(is.null(a$pc()))
    expect_that(a$pc(NA, 0.8), equals(0.8))
    expect_that(a$pc(NaN, 0.8), equals(0.8))
    expect_that(a$pc(NA, 0), equals(NA_real_))
    expect_that(a$pc(NA, 1), equals(1))
    expect_that(a$pc(c(0, 0.5, NA, 0.4, 1),
                     c(NA, NA, 0.4, 0.5, NA)),
                equals(c(NA_real_, 0.5, 0.4, 0.7, 1)))

    expect_that(a$r(c(0.5, 0.8,   NA,      0.3,       NA,      0,    1,    NA, NA, 0,   1, 0.6),
                    c(0.8, 0.5,   0.4,      NA,       NA,     NA,   NA,     0, 1, 0.3, 0.3, 0)),
             equals(c(1.0, 0.625, 0.4,   NA_real_,     1,      1, NA_real_, NA_real_, 1,  1,  0.3, 0)))

    #expect_that(a$b(c(0.5,   0.8,   NA,       0.4,      NA,       0,        1),
                    #c(0.8,   0.5,   0.4,      NA,       NA,       NA,       NA)),
             #equals(c(0.625, 0.625, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)))
    expect_error(a$b(0.5, 0.3))

    expect_that(a$i(), equals(NA_real_))
    expect_that(a$i(NA), equals(NA_real_))
    expect_that(a$i(NaN), equals(NA_real_))
    expect_that(a$i(0.5, NA), equals(NA_real_))
    expect_that(a$i(0.5, NaN), equals(NA_real_))
    expect_that(a$i(0, NA), equals(0))
    expect_that(a$i(1, NA), equals(NA_real_))
    expect_that(a$i(0.5, 0.4), equals(0.4))
    expect_that(a$i(c(0.5, NA, NA, 0.8, NA)), equals(NA_real_))
    expect_that(a$i(c(0, 0.5, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pi()))
    expect_that(a$pi(NA, 0.8), equals(NA_real_))
    expect_that(a$pi(NaN, 0.8), equals(NA_real_))
    expect_that(a$pi(c(0.5, NA, 0.4, NA, NA),
                     c(NA, 0.4, 0.5, NA, 0)),
                equals(c(NA_real_, NA_real_, 0.4, NA_real_, 0)))


    expect_that(a$s(), equals(NA_real_))
    expect_that(a$s(NA), equals(NA_real_))
    expect_that(a$s(NaN), equals(NA_real_))
    expect_that(a$s(0.5, 0.8), equals(0.8))
    expect_that(a$s(NA, 0.5), equals(0.5))
    expect_that(a$s(NA, 1), equals(1))
    expect_that(a$s(NA, NA), equals(NA_real_))
    expect_that(a$s(NA, 0), equals(NA_real_))
    expect_that(a$s(0, NA), equals(NA_real_))
    expect_that(a$s(0, 0), equals(0))
    expect_that(a$s(0.5, NA, NaN, 0.8, NA), equals(0.8))
    expect_that(a$s(c(0.5, NA, NA, 0.8, NA)), equals(0.8))
    expect_that(a$s(c(1, 0.5, NA, NA, 0.8, NA)), equals(1))

    expect_true(is.null(a$ps()))
    expect_that(a$ps(NA, 0.8), equals(0.8))
    expect_that(a$ps(NaN, 0.8), equals(0.8))
    expect_that(a$ps(NA, 0), equals(NA_real_))
    expect_that(a$ps(NA, 1), equals(1))
    expect_that(a$ps(c(0, 0.5, NA, 0.4, 1),
                     c(NA, NA, 0.4, 0.5, NA)),
                equals(c(NA_real_, 0.5, 0.4, 0.5, 1)))

    expect_that(a$n(NA), equals(a$r(NA, 0)))
    expect_that(a$n(1), equals(a$r(1, 0)))
    expect_that(a$n(0), equals(a$r(0, 0)))
    expect_that(a$n(0.8), equals(a$r(0.8, 0)))

})
