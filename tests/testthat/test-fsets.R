test_that('fsets type conversions', {
    set.seed(35423)
    m <- matrix(runif(12), ncol=3)
    s <- matrix(0, nrow=3, ncol=3)
    diag(s) <- 1

    f <- fsets(m, vars=letters[1:3], specs=s)
    expect_true(is.fsets(f))
    expect_that(class(f), equals(c('fsets', 'matrix')))
    expect_that(vars(f), equals(letters[1:3]))
    expect_that(specs(f), equals(s))

    mat <- as.matrix(f)
    expect_true(is.matrix(mat))
    expect_that(mat, equals(m))
    expect_that(vars(mat), equals(NULL))
    expect_that(specs(mat), equals(NULL))

    df <- as.data.frame(f)
    expect_true(is.data.frame(df))
    expect_that(ncol(df), equals(3))
    expect_that(df[, 1], equals(m[, 1]))
    expect_that(df[, 2], equals(m[, 2]))
    expect_that(df[, 3], equals(m[, 3]))
    expect_that(vars(df), equals(NULL))
    expect_that(specs(df), equals(NULL))
})
