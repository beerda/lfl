test_that('fcut for factor', {
    x <- factor(c('a', 'b', 'a', 'c', 'c', 'b', 'c'))
    res <- fcut(x)

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 3)
    expect_equal(nrow(res), 7)
    expect_equal(colnames(res), c('x=a', 'x=b', 'x=c'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 3))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(rep(0, 3*3),
                                     nrow=3,
                                     ncol=3))
    expect_true(is.fsets(res))

    expect_equivalent(as.matrix(res)[1, 1], 1)
    expect_equivalent(as.matrix(res)[1, 2], 0)
    expect_equivalent(as.matrix(res)[1, 3], 0)

    expect_equivalent(as.matrix(res)[2, 1], 0)
    expect_equivalent(as.matrix(res)[2, 2], 1)
    expect_equivalent(as.matrix(res)[2, 3], 0)

    expect_equivalent(as.matrix(res)[3, 1], 1)
    expect_equivalent(as.matrix(res)[3, 2], 0)
    expect_equivalent(as.matrix(res)[3, 3], 0)

    expect_equivalent(as.matrix(res)[4, 1], 0)
    expect_equivalent(as.matrix(res)[4, 2], 0)
    expect_equivalent(as.matrix(res)[4, 3], 1)

    expect_equivalent(as.matrix(res)[5, 1], 0)
    expect_equivalent(as.matrix(res)[5, 2], 0)
    expect_equivalent(as.matrix(res)[5, 3], 1)

    expect_equivalent(as.matrix(res)[6, 1], 0)
    expect_equivalent(as.matrix(res)[6, 2], 1)
    expect_equivalent(as.matrix(res)[6, 3], 0)

    expect_equivalent(as.matrix(res)[7, 1], 0)
    expect_equivalent(as.matrix(res)[7, 2], 0)
    expect_equivalent(as.matrix(res)[7, 3], 1)
})


test_that('fcut for logical', {
    x <- c(TRUE, FALSE, FALSE, TRUE)
    res <- fcut(x)

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 2)
    expect_equal(nrow(res), 4)
    expect_equal(colnames(res), c('x', 'not.x'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 2))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0, nrow=2, ncol=2))
    expect_true(is.fsets(res))

    expect_equivalent(as.matrix(res)[1, 1], 1)
    expect_equivalent(as.matrix(res)[1, 2], 0)

    expect_equivalent(as.matrix(res)[2, 1], 0)
    expect_equivalent(as.matrix(res)[2, 2], 1)

    expect_equivalent(as.matrix(res)[3, 1], 0)
    expect_equivalent(as.matrix(res)[3, 2], 1)

    expect_equivalent(as.matrix(res)[4, 1], 1)
    expect_equivalent(as.matrix(res)[4, 2], 0)
})


test_that('fcut of numeric by single triangle', {
    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 50, 100),
                type='triangle')

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 1)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), 'x=1')
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), 'x')
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0,
                                    nrow=1,
                                    ncol=1))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[26, 1], 0.5)
    expect_equivalent(as.matrix(res)[51, 1], 1)
    expect_equivalent(as.matrix(res)[76, 1], 0.5)
    expect_equivalent(as.matrix(res)[101, 1], 0)
    expect_true(is.fsets(res))
})


test_that('fcut of numeric by multiple triangles', {
    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 25, 50, 75, 100),
                type='triangle')

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 3)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), c('x=1', 'x=2', 'x=3'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 3))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(rep(0, 3*3),
                                    nrow=3,
                                    ncol=3))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[26, 1], 1)
    expect_equivalent(as.matrix(res)[51, 1], 0)
    expect_equivalent(as.matrix(res)[76, 1], 0)
    expect_equivalent(as.matrix(res)[101, 1], 0)

    expect_equivalent(as.matrix(res)[1, 2], 0)
    expect_equivalent(as.matrix(res)[26, 2], 0)
    expect_equivalent(as.matrix(res)[51, 2], 1)
    expect_equivalent(as.matrix(res)[76, 2], 0)
    expect_equivalent(as.matrix(res)[101, 2], 0)

    expect_equivalent(as.matrix(res)[1, 3], 0)
    expect_equivalent(as.matrix(res)[26, 3], 0)
    expect_equivalent(as.matrix(res)[51, 3], 0)
    expect_equivalent(as.matrix(res)[76, 3], 1)
    expect_equivalent(as.matrix(res)[101, 3], 0)
    expect_true(is.fsets(res))
})


test_that('fcut of numeric with merge 1:3', {
    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 25, 50, 75, 100),
                type='triangle',
                merge=1:3)

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 6)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), c('x=1', 'x=2', 'x=3',
                                  'x=1|x=2', 'x=2|x=3',
                                  'x=1|x=2|x=3'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 6))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(c(0,0,0,1,0,1,
                                      0,0,0,1,1,1,
                                      0,0,0,0,1,1,
                                      0,0,0,0,0,1,
                                      0,0,0,0,0,1,
                                      0,0,0,0,0,0),
                                    byrow=TRUE,
                                    nrow=6))
    expect_true(is.fsets(res))
})


test_that('fcut of numeric with merge 2', {
    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 25, 50, 75, 100),
                type='triangle',
                merge=2)

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 2)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), c('x=1|x=2', 'x=2|x=3'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 2))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(rep(0, 2*2),
                                    nrow=2,
                                    ncol=2))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[26, 1], 1)
    expect_equivalent(as.matrix(res)[30, 1], 1)
    expect_equivalent(as.matrix(res)[51, 1], 1)
    expect_equivalent(as.matrix(res)[76, 1], 0)
    expect_equivalent(as.matrix(res)[101, 1], 0)

    expect_equivalent(as.matrix(res)[1, 2], 0)
    expect_equivalent(as.matrix(res)[26, 2], 0)
    expect_equivalent(as.matrix(res)[51, 2], 1)
    expect_equivalent(as.matrix(res)[60, 2], 1)
    expect_equivalent(as.matrix(res)[76, 2], 1)
    expect_equivalent(as.matrix(res)[101, 2], 0)
    expect_true(is.fsets(res))
})


test_that('fcut of numeric with merge 1,3', {
    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 25, 50, 75, 100),
                type='triangle',
                merge=c(1,3))

    expect_true(is.matrix(res))
    expect_equal(ncol(res), 4)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), c('x=1', 'x=2', 'x=3',
                                  'x=1|x=2|x=3'))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), rep('x', 4))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(c(0,0,0,1,
                                      0,0,0,1,
                                      0,0,0,1,
                                      0,0,0,0),
                                    byrow=TRUE,
                                    nrow=4))
    expect_true(is.fsets(res))
})


test_that('fcut of matrix', {
    x <- matrix(1:100, byrow=TRUE, ncol=4)
    colnames(x) <- letters[1:4]

    res <- fcut(x,
                breaks=c(1, 30, 60, 100),
                type='triangle')

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 8)
    expect_equal(nrow(res), 25)
    expect_equal(colnames(res), c('a=1', 'a=2', 'b=1', 'b=2', 'c=1', 'c=2', 'd=1', 'd=2'))
    expect_equal(vars(res), c(rep('a', 2), rep('b', 2), rep('c', 2), rep('d', 2)))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0,
                                    nrow=8,
                                    ncol=8))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[8, 3], 1)
    expect_equivalent(as.matrix(res)[15, 7], 0)
})



test_that('fcut of data frame', {
    x <- matrix(1:100, byrow=TRUE, ncol=4)
    colnames(x) <- letters[1:4]

    res <- fcut(as.data.frame(x),
                breaks=c(1, 30, 60, 100),
                type='triangle')

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 8)
    expect_equal(nrow(res), 25)
    expect_equal(colnames(res), c('a=1', 'a=2', 'b=1', 'b=2', 'c=1', 'c=2', 'd=1', 'd=2'))
    expect_equal(vars(res), c(rep('a', 2), rep('b', 2), rep('c', 2), rep('d', 2)))
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0,
                                    nrow=8,
                                    ncol=8))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[8, 3], 1)
    expect_equivalent(as.matrix(res)[15, 7], 0)
})


test_that('fcut for custom function factory', {
    func <- function(a, b, c) {
        f <- triangular(a, b, c)
        return(function(x) f(x)^2)
    }

    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 50, 100),
                type=func)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 1)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), 'x=1')
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), 'x')
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0,
                                    nrow=1,
                                    ncol=1))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[26, 1], 0.25)
    expect_equivalent(as.matrix(res)[51, 1], 1)
    expect_equivalent(as.matrix(res)[76, 1], 0.25)
    expect_equivalent(as.matrix(res)[101, 1], 0)
    expect_true(is.fsets(res))
})


test_that('fcut for custom function', {
    func <- function(x, a, b, c) {
        f <- triangular(a, b, c)
        return(f(x)^2)
    }

    x <- 0:100
    res <- fcut(x,
                breaks=c(0, 50, 100),
                type=func)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 1)
    expect_equal(nrow(res), 101)
    expect_equal(colnames(res), 'x=1')
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), 'x')
    expect_equal(names(vars(res)), NULL)
    expect_equal(specs(res), matrix(0,
                                    nrow=1,
                                    ncol=1))

    expect_equivalent(as.matrix(res)[1, 1], 0)
    expect_equivalent(as.matrix(res)[26, 1], 0.25)
    expect_equivalent(as.matrix(res)[51, 1], 1)
    expect_equivalent(as.matrix(res)[76, 1], 0.25)
    expect_equivalent(as.matrix(res)[101, 1], 0)
    expect_true(is.fsets(res))
})

