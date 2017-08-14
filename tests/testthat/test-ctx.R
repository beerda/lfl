test_that("ctx3", {
    ex <- function(x1, x2, x3) {
        structure(c(low=x1, center=x2, high=x3),
                  class=c('ctx3', 'numeric'))
    }

    context <- ctx3()
    expect_true(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    context <- ctx3(1, 2, 4)
    expect_true(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    expect_equal(ctx3(),
                 ex(0, 0.5, 1))
    expect_equal(ctx3(high=5),
                 ex(0, 2.5, 5))
    expect_equal(ctx3(high=5, relCenter=0.8),
                 ex(0, 4, 5))
    expect_equal(ctx3(low=3, high=5),
                 ex(3, 4, 5))
    expect_equal(ctx3(low=3, high=5, relCenter=0.75),
                 ex(3, 4.5, 5))
    expect_equal(ctx3(low=3, center=3.1, high=5),
                 ex(3, 3.1, 5))

    expect_error(ctx3(0, 0, 1))
    expect_error(ctx3(0, 1, 1))
    expect_error(ctx3(2, 1, 0))
    expect_error(ctx3(low=-10, high=10, relCenter=0))
    expect_error(ctx3(low=-10, high=10, relCenter=1))
    expect_error(ctx3(low=-10, high=10, relCenter=-0.5))
    expect_error(ctx3(low=-10, high=10, relCenter=1.5))

    expect_equal(as.ctx3(ctx3(1, 2, 4)),
                 ex(1, 2, 4))
    expect_equal(as.ctx3(ctx5(1, 2, 4, 8, 16)),
                 ex(1, 4, 16))
    expect_equal(as.ctx3(ctx5bilat(1, 2, 4, 8, 16, 32, 64, 128, 256)),
                 ex(16, 64, 256))
    expect_equal(as.ctx3(ctx3bilat(1, 2, 4, 8, 16)),
                 ex(4, 8, 16))

    expect_equal(as.ctx3(c(1, 2, 3)),
                 ex(1, 2, 3))
    expect_error(as.ctx3(runif(100)))
})


test_that("ctx5", {
    ex <- function(x1, x2, x3, x4, x5) {
        structure(c(low=x1, lowerCenter=x2, center=x3, upperCenter=x4, high=x5),
                  class=c('ctx5', 'numeric'))
    }

    context <- ctx5()
    expect_false(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_true(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    context <- ctx5(1, 2, 4, 5, 8)
    expect_false(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_true(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    expect_equal(ctx5(),
                 ex(0, 0.25, 0.5, 0.75, 1))
    expect_equal(ctx5(high=5),
                 ex(0, 1.25, 2.5, 3.75, 5))
    expect_equal(ctx5(high=5, relCenter=0.8),
                 ex(0, 2, 4, 4.5, 5))
    expect_equal(ctx5(low=3, high=5),
                 ex(3, 3.5, 4, 4.5, 5))
    expect_equal(ctx5(low=3, high=5, relCenter=0.75),
                 ex(3, 3.75, 4.5, 4.75, 5))
    expect_equal(ctx5(low=3, center=3.1, high=5),
                 ex(3, 3.05, 3.1, 4.05, 5))
    expect_equal(ctx5(1, 2, 3, 4, 5),
                 ex(1, 2, 3, 4, 5))

    expect_error(ctx5(0, 0, 1, 2, 3))
    expect_error(ctx5(0, 1, 1, 2, 3))
    expect_error(ctx5(0, 1, 2, 2, 3))
    expect_error(ctx5(0, 1, 2, 3, 3))
    expect_error(ctx5(low=-10, high=10, relCenter=0))
    expect_error(ctx5(low=-10, high=10, relCenter=1))
    expect_error(ctx5(low=-10, high=10, relCenter=-0.5))
    expect_error(ctx5(low=-10, high=10, relCenter=1.5))

    expect_equal(as.ctx5(ctx5(1, 1.5, 2, 3, 4)),
                 ex(1, 1.5, 2, 3, 4))
    expect_equal(as.ctx5(ctx3(1, 2, 4)),
                 ex(1, 1.5, 2, 3, 4))
    expect_equal(as.ctx5(ctx5bilat(1, 2, 4, 8, 16, 32, 64, 128, 256)),
                 ex(16, 32, 64, 128, 256))
    expect_equal(as.ctx5(ctx3bilat(1, 2, 4, 8, 16)),
                 ex(4, 6, 8, 12, 16))

    expect_equal(as.ctx5(c(1, 2, 3, 4, 5)),
                 ex(1, 2, 3, 4, 5))
    expect_error(as.ctx3(runif(100)))
})


test_that("ctx3bilat", {
    ex <- function(x1, x2, x3, x4, x5) {
        structure(c(negMax=x1, negCenter=x2, origin=x3, center=x4, max=x5),
                  class=c('ctx3bilat', 'numeric'))
    }

    context <- ctx3bilat()
    expect_false(is.ctx3(context))
    expect_true(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    context <- ctx3bilat(1, 2, 4, 5, 8)
    expect_false(is.ctx3(context))
    expect_true(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_false(is.ctx5bilat(context))

    expect_equal(ctx3bilat(),
                 ex(-1, -0.5, 0, 0.5, 1))
    expect_equal(ctx3bilat(max=5),
                 ex(-1, -0.5, 0, 2.5, 5))
    expect_equal(ctx3bilat(negMax=-5),
                 ex(-5, -2.5, 0, 0.5, 1))
    expect_equal(ctx3bilat(max=5, relCenter=0.8),
                 ex(-1, -0.8, 0, 4, 5))
    expect_equal(ctx3bilat(negMax=-5, relCenter=0.8),
                 ex(-5, -4, 0, 0.8, 1))
    expect_equal(ctx3bilat(origin=3, max=5),
                 ex(-1, 1, 3, 4, 5))
    expect_equal(ctx3bilat(negMax=-5, origin=3, max=5),
                 ex(-5, -1, 3, 4, 5))
    expect_equal(ctx3bilat(negMax=3, negCenter=3.1, origin=4, center=4.1, max=5),
                 ex(3, 3.1, 4, 4.1, 5))

    expect_error(ctx3bilat(0, 0, 1, 2, 3))
    expect_error(ctx3bilat(0, 1, 1, 2, 3))
    expect_error(ctx3bilat(0, 1, 2, 2, 3))
    expect_error(ctx3bilat(0, 1, 2, 3, 3))
    expect_error(ctx3bilat(5, 1, 2, 3, 4))
    expect_error(ctx3bilat(relCenter=0))
    expect_error(ctx3bilat(relCenter=1))
    expect_error(ctx3bilat(relCenter=-0.5))
    expect_error(ctx3bilat(relCenter=1.5))

    expect_equal(as.ctx3bilat(ctx3bilat(-2, 0, 1, 2, 4)),
                 ex(-2, 0, 1, 2, 4))
    expect_equal(as.ctx3bilat(ctx3(1, 2, 4)),
                 ex(-2, 0, 1, 2, 4))
    expect_equal(as.ctx3bilat(ctx5bilat(1, 2, 4, 8, 16, 32, 64, 128, 256)),
                 ex(1, 4, 16, 64, 256))
    expect_equal(as.ctx3bilat(ctx5(16, 32, 64, 128, 256)),
                 ex(-224, -32, 16, 64, 256))

    expect_equal(as.ctx3bilat(c(1, 2, 3, 4, 5)),
                 ex(1, 2, 3, 4, 5))
    expect_error(as.ctx3bilat(runif(100)))
})


test_that("ctx5bilat", {
    ex <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9) {
        structure(c(negMax=x1, negUpperCenter=x2, negCenter=x3, negLowerCenter=x4, origin=x5,
                    lowerCenter=x6, center=x7, upperCenter=x8, max=x9),
                  class=c('ctx5bilat', 'numeric'))
    }

    context <- ctx5bilat()
    expect_false(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_true(is.ctx5bilat(context))

    context <- ctx5bilat(1, 2, 4, 5, 8, 9, 10, 11, 12)
    expect_false(is.ctx3(context))
    expect_false(is.ctx3bilat(context))
    expect_false(is.ctx5(context))
    expect_true(is.ctx5bilat(context))

    expect_equal(ctx5bilat(),
                 ex(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))
    expect_equal(ctx5bilat(max=5),
                 ex(-1, -0.75, -0.5, -0.25, 0, 1.25, 2.5, 3.75, 5))
    expect_equal(ctx5bilat(negMax=-5),
                 ex(-5, -3.75, -2.5, -1.25, 0, 0.25, 0.5, 0.75, 1))
    expect_equal(ctx5bilat(max=5, relCenter=0.8),
                 ex(-1, -0.9, -0.8, -0.4, 0, 2, 4, 4.5, 5))
    expect_equal(ctx5bilat(negMax=-5, relCenter=0.8),
                 ex(-5, -4.5, -4, -2, 0, 0.4, 0.8, 0.9, 1))
    expect_equal(ctx5bilat(origin=3, max=5),
                 ex(-1, 0, 1, 2, 3, 3.5, 4, 4.5, 5))
    expect_equal(ctx5bilat(negMax=-5, origin=3, max=5),
                 ex(-5, -3, -1, 1, 3, 3.5, 4, 4.5, 5))
    expect_equal(ctx5bilat(negMax=1, negUpperCenter=1.5, negCenter=2,
                        negLowerCenter=2.5, origin=4, lowerCenter=4.2,
                        center=4.5, upperCenter=4.8, max=5),
                 ex(1, 1.5, 2, 2.5, 4, 4.2, 4.5, 4.8, 5))

    expect_error(ctx5bilat(0, 0, 1, 2, 3, 4, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 1, 2, 3, 4, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 2, 3, 4, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 3, 3, 4, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 3, 4, 4, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 3, 4, 5, 5, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 3, 4, 5, 6, 6, 7))
    expect_error(ctx5bilat(0, 1, 2, 3, 4, 5, 6, 7, 7))
    expect_error(ctx5bilat(relCenter=0))
    expect_error(ctx5bilat(relCenter=1))
    expect_error(ctx5bilat(relCenter=-0.5))
    expect_error(ctx5bilat(relCenter=1.5))

    expect_equal(as.ctx5bilat(ctx5bilat(1, 2, 4, 8, 16, 32, 64, 128, 256)),
                 ex(1, 2, 4, 8, 16, 32, 64, 128, 256))
    expect_equal(as.ctx5bilat(ctx3(1, 2, 4)),
                 ex(-2, -1, 0, 0.5, 1, 1.5, 2, 3, 4))
    expect_equal(as.ctx5bilat(ctx3bilat(1, 2, 4, 8, 16)),
                 ex(1, 1.5, 2, 3, 4, 6, 8, 12, 16))
    expect_equal(as.ctx5bilat(ctx5(16, 32, 64, 128, 256)),
                 ex(-224, -96, -32, 0, 16, 32, 64, 128, 256))

    expect_equal(as.ctx5bilat(c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
                 ex(1, 2, 3, 4, 5, 6, 7, 8, 9))
    expect_error(as.ctx5bilat(runif(100)))
})

