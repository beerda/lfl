test_that('pbld', {
    # init fsets
    .vars <- c(rep('b', 3),
               rep('c', 3))
    names(.vars) <- paste(rep(c('ve.sm', 'sm', 'bi'), times=2),
                          rep(c('b', 'c'), each=3),
                          sep='.')


    .specs <- matrix(c(0,1,0, 0,0,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0,

                       0,0,0, 0,1,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0),
                     byrow=TRUE,
                     ncol=6)
    colnames(.specs) <- names(.vars)
    rownames(.specs) <- names(.vars)

    x <- matrix(runif(18), ncol=6, nrow=3)
    colnames(x) <- names(.vars)

    x <- fsets(x, vars=.vars, specs=.specs)

    # init rules
    rules <- list(c('sm.b', 've.sm.c'),
                  c('sm.b', 'sm.c'),
                  c('bi.b', 'bi.b'),
                  c('bi.b', 'sm.c', 'sm.b'))

    # init values
    values <- 0:10 / 10

    # init partition
    partition <- lcut(data.frame(b=values))


    res <- pbld(x, rules, partition, values)
    expect_equal(nrow(x), length(res))
    #expect_true(FALSE)
})


test_that('pbld with empty rulebase', {
    # init fsets
    .vars <- c(rep('b', 3),
               rep('c', 3))
    names(.vars) <- paste(rep(c('ve.sm', 'sm', 'bi'), times=2),
                          rep(c('b', 'c'), each=3),
                          sep='.')


    .specs <- matrix(c(0,1,0, 0,0,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0,

                       0,0,0, 0,1,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0),
                     byrow=TRUE,
                     ncol=6)
    colnames(.specs) <- names(.vars)
    rownames(.specs) <- names(.vars)

    x <- matrix(runif(18), ncol=6, nrow=3)
    colnames(x) <- names(.vars)

    x <- fsets(x, vars=.vars, specs=.specs)

    # init rules
    rules <- list()

    # init values
    values <- 0:10 / 10

    # init partition
    partition <- lcut(data.frame(b=values))


    res <- pbld(x, rules, partition, values)
    expect_equal(nrow(x), length(res))
})


test_that('pbld complete test', {
    x <- matrix(seq(0, 0.4, 0.025), nrow=17, ncol=1)
    colnames(x) <- 'x'
    inputContext <- ctx3(0, 0.4, 1)
    input <- lcut(x, context=inputContext)

    rules <- list(c('bi.y', 'sm.x'),
                c('me.y', 'ro.sm.x'),
                c('ve.sm.y', 'ex.sm.x'))

    outputContext <- ctx3(0, 0.4, 1)
    v <- seq(outputContext[1], outputContext[3], length.out=1000)
    p <- lcut(v, name='y', context=outputContext)

    expect_equal(round(pbld(input, rules, p, v), 2),
                 c(0.03, 0.91, 0.91, 0.41, 0.41, 0.41, 0.41, 0.42, 0.44, 0.45, rep(0, 7)))
})
