test_that('perceive global', {
    f <- lcut(data.frame(a=0:1, b=0:1, c=0:1, d=0:1))

    runPerceive <- function(...) {
        ret <- perceive(list(...), fsets=f)
        return(ret)
    }

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'bi.c'),
                             c('sm.d', 'sm.b', 'sm.c')),
                 c(F, T, T))

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'sm.c'),
                             c('sm.d', 'sm.b', 'sm.c')),
                 c(T, T, T))

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('bi.d', 've.sm.a', 'bi.c'),
                             c('sm.d', 'sm.b', 'sm.c')),
                 c(F, T, T))
})


test_that('perceive local', {
    f <- lcut(data.frame(a=0:1, b=0:1, c=0:1, d=0:1))

    runPerceive <- function(..., fired) {
        return(perceive(list(...), fsets=f, type='local', fired=fired))
    }

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'bi.c'),
                             fired=c(0.3, 0.4)),
                 c(F, T))

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'bi.c'),
                             fired=c(0.4, 0.3)),
                 c(T, F))

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'bi.c'),
                             fired=c(0.4, 0.4)),
                 c(F, T))

    expect_equal(runPerceive(c('sm.d', 'sm.a', 'bi.c'),
                             c('sm.d', 've.sm.a', 'bi.c'),
                             fired=c(1, 1)),
                 c(F, T))
})
