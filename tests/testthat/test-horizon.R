test_that('horizon ctx3', {
    ctx <- ctx3(0, 0.5, 1)
    expect_equal(horizon(ctx, 'sm')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(1, 1, 1, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'me')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'bi')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 1, 1, 1, NA, NaN))

    expect_equal(horizon(ctx, 'sm')(seq(0, 1, length.out=11)),
                 c(seq(1, 0, length.out=6), rep(0, 5)))
    expect_equal(horizon(ctx, 'me')(seq(0, 1, length.out=11)),
                 c(seq(0, 1, length.out=6), seq(0.8, 0, length.out=5)))
    expect_equal(horizon(ctx, 'bi')(seq(0, 1, length.out=11)),
                 c(rep(0, 5), seq(0, 1, length.out=6)))

    expect_error(horizon(ctx, 'lm'))
    expect_error(horizon(ctx, 'um'))
    expect_error(horizon(ctx, 'neg.sm'))
    expect_error(horizon(ctx, 'neg.lm'))
    expect_error(horizon(ctx, 'neg.um'))
    expect_error(horizon(ctx, 'neg.me'))
    expect_error(horizon(ctx, 'neg.bi'))
    expect_error(horizon(ctx, 'neg.lm'))
    expect_error(horizon(ctx, 'neg.um'))
    expect_error(horizon(ctx, 'ze'))
})


test_that('horizon ctx5', {
    ctx <- ctx5(0, 0.25, 0.5, 0.75, 1)
    expect_equal(horizon(ctx, 'sm')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(1, 1, 1, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'lm')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'me')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'um')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'bi')(c(-Inf, -10, 0, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 1, 1, 1, NA, NaN))

    expect_equal(horizon(ctx, 'sm')(seq(0, 1, length.out=21)),
                 c(seq(1, 0, length.out=11), rep(0, 10)))
    expect_equal(horizon(ctx, 'lm')(seq(0, 1, length.out=21)),
                 c(seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 10)))
    expect_equal(horizon(ctx, 'me')(seq(0, 1, length.out=21)),
                 c(seq(0, 1, length.out=11), seq(0.9, 0, length.out=10)))
    expect_equal(horizon(ctx, 'um')(seq(0, 1, length.out=21)),
                 c(rep(0, 10), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5)))
    expect_equal(horizon(ctx, 'bi')(seq(0, 1, length.out=21)),
                 c(rep(0, 10), seq(0, 1, length.out=11)))

    expect_error(horizon(ctx, 'neg.sm'))
    expect_error(horizon(ctx, 'neg.lm'))
    expect_error(horizon(ctx, 'neg.um'))
    expect_error(horizon(ctx, 'neg.me'))
    expect_error(horizon(ctx, 'neg.bi'))
    expect_error(horizon(ctx, 'ze'))
})


test_that('horizon ctx3bilat', {
    ctx <- ctx3bilat(-1, -0.5, 0, 0.5, 1)
    expect_equal(horizon(ctx, 'neg.bi')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(1, 1, 1, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'neg.me')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'neg.sm')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'ze')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'sm')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'me')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'bi')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 1, 1, 1, NA, NaN))

    expect_equal(horizon(ctx, 'neg.bi')(seq(-1, 1, length.out=21)),
                 c(seq(1, 0, length.out=6), rep(0, 15)))
    expect_equal(horizon(ctx, 'neg.me')(seq(-1, 1, length.out=21)),
                 c(seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 10)))
    expect_equal(horizon(ctx, 'neg.sm')(seq(-1, 1, length.out=21)),
                 c(rep(0, 5), seq(0, 1, length.out=6), rep(0, 10)))
    expect_equal(horizon(ctx, 'ze')(seq(-1, 1, length.out=21)),
                 c(rep(0, 5), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 5)))
    expect_equal(horizon(ctx, 'sm')(seq(-1, 1, length.out=21)),
                 c(rep(0, 10), seq(1, 0, length.out=6), rep(0, 5)))
    expect_equal(horizon(ctx, 'me')(seq(-1, 1, length.out=21)),
                 c(rep(0, 10), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5)))
    expect_equal(horizon(ctx, 'bi')(seq(-1, 1, length.out=21)),
                 c(rep(0, 15), seq(0, 1, length.out=6)))

    expect_error(horizon(ctx, 'lm'))
    expect_error(horizon(ctx, 'um'))
    expect_error(horizon(ctx, 'neg.lm'))
    expect_error(horizon(ctx, 'neg.um'))
})


test_that('horizon ctx5bilat', {
    ctx <- ctx5bilat(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    expect_equal(horizon(ctx, 'neg.bi')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(1, 1, 1, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'neg.me')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'neg.sm')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'ze')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'sm')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'me')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 0, 0, 0, NA, NaN))
    expect_equal(horizon(ctx, 'bi')(c(-Inf, -10, -1, 1, 10, Inf, NA, NaN)), 
                 c(0, 0, 0, 1, 1, 1, NA, NaN))

    expect_equal(horizon(ctx, 'neg.bi')(seq(-1, 1, length.out=41)),
                 c(seq(1, 0, length.out=11), rep(0, 30)))
    expect_equal(horizon(ctx, 'neg.um')(seq(-1, 1, length.out=41)),
                 c(seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 30)))
    expect_equal(horizon(ctx, 'neg.me')(seq(-1, 1, length.out=41)),
                 c(seq(0, 1, length.out=11), seq(0.9, 0, length.out=10), rep(0, 20)))
    expect_equal(horizon(ctx, 'neg.lm')(seq(-1, 1, length.out=41)),
                 c(rep(0, 10), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 20)))
    expect_equal(horizon(ctx, 'neg.sm')(seq(-1, 1, length.out=41)),
                 c(rep(0, 10), seq(0, 1, length.out=11), rep(0, 20)))
    expect_equal(horizon(ctx, 'ze')(seq(-1, 1, length.out=41)),
                 c(rep(0, 10), seq(0, 1, length.out=11), seq(0.9, 0, length.out=10), rep(0, 10)))
    expect_equal(horizon(ctx, 'sm')(seq(-1, 1, length.out=41)),
                 c(rep(0, 20), seq(1, 0, length.out=11), rep(0, 10)))
    expect_equal(horizon(ctx, 'lm')(seq(-1, 1, length.out=41)),
                 c(rep(0, 20), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5), rep(0, 10)))
    expect_equal(horizon(ctx, 'me')(seq(-1, 1, length.out=41)),
                 c(rep(0, 20), seq(0, 1, length.out=11), seq(0.9, 0, length.out=10)))
    expect_equal(horizon(ctx, 'um')(seq(-1, 1, length.out=41)),
                 c(rep(0, 30), seq(0, 1, length.out=6), seq(0.8, 0, length.out=5)))
    expect_equal(horizon(ctx, 'bi')(seq(-1, 1, length.out=41)),
                 c(rep(0, 30), seq(0, 1, length.out=11)))
})
