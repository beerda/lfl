test_that('fire rules on data vector', {
    x <- 1:10 / 10
    names(x) <- letters[1:10]
    rules <- list(c('c', 'e'),
                  character(),
                  c('a'),
                  c('a', 'b'))

    res <- fire(x, rules, pgoguen.tnorm, onlyAnte=FALSE)
    expect_equal(res, list(0.15, 1, 0.1, 0.02))

    res <- fire(x, rules, 'goguen', onlyAnte=FALSE)
    expect_equal(res, list(0.15, 1, 0.1, 0.02))
})


test_that('fire single rule on data matrix', {
    x <- matrix(1:20 / 20, nrow=2)
    colnames(x) <- letters[1:10]
    rules <- c('c', 'e')

    res <- fire(x, rules, pgoguen.tnorm, onlyAnte=FALSE)
    expect_equal(res, list(c(0.1125, 0.15)))

    res <- fire(x, rules, 'goguen', onlyAnte=FALSE)
    expect_equal(res, list(c(0.1125, 0.15)))
})


test_that('fire rules on data matrix', {
    x <- matrix(1:20 / 20, nrow=2)
    colnames(x) <- letters[1:10]
    rules <- list(c('c', 'e'),
                  character(),
                  c('a'),
                  c('a', 'b'))

    res <- fire(x, rules, 'goguen', onlyAnte=FALSE)
    expect_equal(res, list(c(0.1125, 0.15),
                           c(1, 1),
                           c(0.05, 0.1),
                           c(0.0075, 0.02)))
})


test_that('fire on farules', {
    x <- matrix(1:20 / 20, nrow=2)
    colnames(x) <- letters[1:10]
    rules <- list(c('a', 'c', 'e'),
                  c('b'),
                  c('d', 'a'),
                  c('c', 'a', 'b'))
    frules <- list(rules=rules, statistics=matrix(0, ncol=1, nrow=1))
    class(frules) <- c('farules', 'list')
    expect_true(is.farules(frules))

    res <- fire(x, frules, 'goguen', onlyAnte=TRUE)
    expect_equal(res, list(c(0.1125, 0.15),
                           c(1, 1),
                           c(0.05, 0.1),
                           c(0.0075, 0.02)))
})


test_that('fire rules on NA data matrix', {
    x <- matrix(1:20 / 20, nrow=2)
    colnames(x) <- letters[1:10]
    x[2, 1] <- NA

    rules <- list(c('c', 'e'),
                  character(),
                  c('a'),
                  c('a', 'b'))

    res <- fire(x, rules, 'goguen', onlyAnte=FALSE)
    expect_equal(res, list(c(0.1125, 0.15),
                           c(1, 1),
                           c(0.05, NA),
                           c(0.0075, NA)))
})

