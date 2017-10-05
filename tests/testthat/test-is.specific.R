test_that('is.specific', {
    is.specifi2 <- function(x, y, f) {
        is.specific(rev(x), y, f)
    }

    is.specifi3 <- function(x, y, f) {
        is.specific(x, rev(y), f)
    }

    is.specifi4 <- function(x, y, f) {
        is.specific(rev(x), rev(y), f)
    }

    v <- c(rep('a', 3), rep('b', 3), rep('c', 3), rep('d', 3))
    s <- matrix(c(0,1,0, 0,0,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,

                  0,0,0, 0,1,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,

                  0,0,0, 0,0,0, 0,1,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,

                  0,0,0, 0,0,0, 0,0,0, 0,1,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0,
                  0,0,0, 0,0,0, 0,0,0, 0,0,0),
                byrow=TRUE,
                ncol=12)
    m <- matrix(0, nrow=1, ncol=12)
    colnames(m) <- paste(rep(c('VeSm', 'Sm', 'Bi'), times=4),
                         rep(c('a', 'b', 'c', 'd'), each=3),
                         sep='.')
    f <- fsets(m, v, s)

    # equal rules
    expect_true(is.specific(c('VeSm.a', 'Bi.c'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi2(c('VeSm.a', 'Bi.c'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi3(c('VeSm.a', 'Bi.c'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi4(c('VeSm.a', 'Bi.c'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    # the same but single item that is more specific
    expect_true(is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c', 'Sm.d'),
                            f))

    expect_true(is.specifi2(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c', 'Sm.d'),
                            f))

    expect_true(is.specifi3(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c', 'Sm.d'),
                            f))

    expect_true(is.specifi4(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c', 'Sm.d'),
                            f))

    # "any" is less specific
    expect_true(is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi2(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi3(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi4(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('VeSm.a', 'Bi.c'),
                            f))

    # "any" + other
    expect_true(is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi2(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi3(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c'),
                            f))

    expect_true(is.specifi4(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            c('Sm.a', 'Bi.c'),
                            f))

    # everything is more specific than empty rule
    expect_true(is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
                            character(),
                            f))

    # null rules are <=
    expect_true(is.specific(character(),
                            character(),
                            f))

    # the same but single item that is more specific
    expect_false(is.specific(c('Sm.a', 'Bi.c', 'Sm.d'),
                             c('VeSm.a', 'Bi.c', 'Sm.d'),
                             f))

    # "any" is less specific
    expect_false(is.specific(c('VeSm.a', 'Bi.c'),
                             c('VeSm.a', 'Bi.c', 'Sm.d'),
                             f))

    # "any" + other
    expect_false(is.specific(c('Sm.a', 'Bi.c'),
                             c('VeSm.a', 'Bi.c', 'Sm.d'),
                             f))

    # everything is more specific than empty rule
    expect_false(is.specific(character(),
                             c('VeSm.a', 'Bi.c', 'Sm.d'),
                             f))

    # different .vars are incomparable
    expect_false(is.specific(c('Sm.a'),
                             c('Bi.c'),
                             f))
    expect_false(is.specific(c('Bi.c'),
                             c('Sm.a'),
                             f))



    expect_false(is.specific(c('VeSm.a', 'Sm.c'),
                             c('Sm.a', 'Bi.c'),
                             f))
    expect_false(is.specific(c('Sm.b', 'Sm.d'),
                             c('Sm.a', 'Bi.c'),
                             f))
})
