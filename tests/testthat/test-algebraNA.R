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
             equals(c(1.0, 0.625, 0.4, 0.7, NA_real_)))

    expect_that(a$b(c(0.5,   0.8,    NA,  0.4,  NA),
                    c(0.8,   0.5,   0.4,   NA,  NA)),
             equals(c(0.625, 0.625, 0.24, 0.24, NA_real_)))

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

})
