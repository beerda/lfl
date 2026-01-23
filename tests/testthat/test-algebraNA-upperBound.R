set.seed(34523)

test_that('upperBound', {
    a <- upperBound(algebra('luk'))
    expect_that(a$algebratype, equals(c('lukasiewicz', 'upperBound')))


    a <-upperBound(algebra('gog'))

    expect_that(a$algebratype, equals(c('goguen', 'upperBound')))
    expect_that(a$t(), equals(NA_real_))
    expect_that(a$t(NA), equals(NA_real_))
    expect_that(a$t(NaN), equals(NA_real_))
    expect_that(a$t(0.5, NA), equals(0.5))
    expect_that(a$t(0.5, NaN), equals(0.5))
    expect_that(a$t(0, NA), equals(0))
    expect_that(a$t(1, NA), equals(NA_real_))
    expect_that(a$t(NA, 0), equals(0))
    expect_that(a$t(NA, 0.5), equals(0.5))
    expect_that(a$t(NA, 1), equals(NA_real_))
    expect_that(a$t(0.5, 0.4), equals(0.2))
    expect_that(a$t(c(0.5, NA, NA, 0.8, NA)), equals(0.4))
    expect_that(a$t(c(0, 0.5, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pt()))
    expect_that(a$pt(NA, 0.8), equals(0.8))
    expect_that(a$pt(NaN, 0.8), equals(0.8))
    expect_that(a$pt(c(0.5, NA, 0.4, NA, NA, 1, 1),
                     c(NA, 0.4, 0.5, NA, 0, NA, 0.3)),
                equals(c(0.5, 0.4, 0.2, NA_real_, 0, NA_real_, 0.3)))

    expect_that(a$c(), equals(NA_real_))
    expect_that(a$c(NA), equals(NA_real_))
    expect_that(a$c(NaN), equals(NA_real_))
    expect_that(a$c(NA, 0), equals(NA_real_))
    expect_that(a$c(NA, 0.5), equals(NA_real_))
    expect_that(a$c(NA, 1), equals(1))
    expect_that(a$c(0, NA), equals(NA_real_))
    expect_that(a$c(0.5, NA), equals(NA_real_))
    expect_that(a$c(1, NA), equals(1))
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

    expect_that(a$r(c(0.5,   0.8,  NA, 0.3, NA,        1, NA, NA,  0,   0, 1, 0.6),
                    c(0.8,   0.5, 0.4,  NA, NA,       NA,  1,  0, NA, 0.3, 0.3, 0)),
             equals(c(1.0, 0.625, 0.4,   1,  1, NA_real_,  1,  0,  1,   1, 0.3, 0)))

    expect_that(a$b(c(0.5,   0.8,   NA,  0.4, NA,  0,       1, NA, NA),
                    c(0.8,   0.5,   0.4,  NA, NA, NA,      NA,  0,  1)),
             equals(c(0.625, 0.625, 0.4, 0.4,  1,  0, NA_real_, 0, NA_real_)))

    expect_that(a$i(), equals(NA_real_))
    expect_that(a$i(NA), equals(NA_real_))
    expect_that(a$i(NaN), equals(NA_real_))
    expect_that(a$i(0.5, NA), equals(0.5))
    expect_that(a$i(0.5, NaN), equals(0.5))
    expect_that(a$i(0, NA), equals(0))
    expect_that(a$i(1, NA), equals(NA_real_))
    expect_that(a$i(NA, 0), equals(0))
    expect_that(a$i(NA, 0.5), equals(0.5))
    expect_that(a$i(NA, 1), equals(NA_real_))
    expect_that(a$i(0.5, 0.4), equals(0.4))
    expect_that(a$i(c(0.5, NA, NA, 0.8, NA)), equals(0.5))
    expect_that(a$i(c(0, 0.5, NA, NA, 0.8, NA)), equals(0))

    expect_true(is.null(a$pi()))
    expect_that(a$pi(NA, 0.8), equals(0.8))
    expect_that(a$pi(NaN, 0.8), equals(0.8))
    expect_that(a$pi(c(0.5, NA, 0.4, NA, NA),
                     c(NA, 0.4, 0.5, NA, 0)),
                equals(c(0.5, 0.4, 0.4, NA_real_, 0)))

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

    expect_that(a$n(NA), equals(0))
    expect_that(a$n(1), equals(0))
    expect_that(a$n(0), equals(1))
    expect_that(a$n(0.8), equals(a$r(0.8, 0)))

    expect_that(a$ni(NA), equals(NA_real_))
    expect_that(a$ni(1), equals(0))
    expect_that(a$ni(0), equals(1))
    expect_that(a$ni(0.8), equals(0.2))
})


test_that('upperBound order', {
    a <- upperBound(algebra('lukasiewicz'))

    expect_equal(a$order(c(NA), decreasing=FALSE), c(1))
    expect_equal(a$order(c(0), decreasing=FALSE), c(1))
    expect_equal(a$order(c(0.3), decreasing=FALSE), c(1))
    expect_equal(a$order(c(0.3, 0), decreasing=FALSE), c(2, 1))
    expect_equal(a$order(c(0.3, 0, 0.8), decreasing=FALSE), c(2, 1, 3))
    expect_equal(a$order(c(0.3, NA, 0, 1), decreasing=FALSE), c(3, 1, 2, 4))
    expect_equal(a$order(c(1, 0.3, NA, 0, 0.8), decreasing=FALSE), c(4, 2, 5, 3, 1))
    expect_equal(a$order(c(NA, 0), decreasing=FALSE), c(2, 1))
    expect_equal(a$order(c(NA, 0.5), decreasing=FALSE), c(2, 1))
    expect_equal(a$order(c(NA, 1), decreasing=FALSE), c(1, 2))

    expect_equal(a$order(c(NA), decreasing=TRUE), c(1))
    expect_equal(a$order(c(0), decreasing=TRUE), c(1))
    expect_equal(a$order(c(0.3), decreasing=TRUE), c(1))
    expect_equal(a$order(c(0.3, 0), decreasing=TRUE), c(1, 2))
    expect_equal(a$order(c(0.3, 0, 0.8), decreasing=TRUE), c(3, 1, 2))
    expect_equal(a$order(c(0.3, NA, 0, 1), decreasing=TRUE), c(4, 2, 1, 3))
    expect_equal(a$order(c(1, 0.3, NA, 0, 0.8), decreasing=TRUE), c(1, 3, 5, 2, 4))
    expect_equal(a$order(c(NA, 0), decreasing=TRUE), c(1, 2))
    expect_equal(a$order(c(NA, 0.5), decreasing=TRUE), c(1, 2))
    expect_equal(a$order(c(NA, 1), decreasing=TRUE), c(2, 1))
})

