test_that('fsets without colnames', {
    set.seed(35423)
    m <- matrix(runif(12), ncol=3)
    v <- letters[1:3]
    s <- matrix(0, nrow=3, ncol=3)
    diag(s) <- 1

    f <- fsets(m, vars=v, specs=s)
    expect_true(is.fsets(f))
    expect_that(class(f), equals(c('fsets', 'matrix')))
    expect_that(vars(f), equals(letters[1:3]))
    expect_that(specs(f), equals(s))

    mat <- as.matrix(f)
    expect_true(is.matrix(mat))
    expect_that(mat, equals(m))
    expect_error(vars(mat))
    expect_error(specs(mat))

    df <- as.data.frame(f)
    expect_true(is.data.frame(df))
    expect_that(ncol(df), equals(3))
    expect_that(nrow(df), equals(4))
    expect_that(df[, 1], equals(m[, 1]))
    expect_that(df[, 2], equals(m[, 2]))
    expect_that(df[, 3], equals(m[, 3]))
    expect_error(vars(df))
    expect_error(specs(df))

    #---------
    v2 <- v
    names(v2) <- LETTERS[1:3]
    s2 <- s
    colnames(s2) <- LETTERS[1:3]
    rownames(s2) <- LETTERS[1:3]

    f2 <- f
    vars(f2) <- v
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    specs(f2) <- s
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    vars(f2) <- v2
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    specs(f2) <- s2
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- fsets(m, vars=v2, specs=s)
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- fsets(m, vars=v, specs=s2)
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))
})

test_that('fsets with colnames', {
    set.seed(35423)
    m <- matrix(runif(12), ncol=3)
    colnames(m) <- LETTERS[1:3]
    v <- letters[1:3]
    s <- matrix(0, nrow=3, ncol=3)
    diag(s) <- 1

    f <- fsets(m, vars=v, specs=s)
    expect_true(is.fsets(f))
    expect_that(colnames(f), equals(LETTERS[1:3]))
    expect_that(class(f), equals(c('fsets', 'matrix')))
    expect_that(vars(f), equals(letters[1:3]))
    expect_that(specs(f), equals(s))

    mat <- as.matrix(f)
    expect_true(is.matrix(mat))
    expect_that(mat, equals(m))
    expect_that(colnames(mat), equals(LETTERS[1:3]))
    expect_error(vars(mat))
    expect_error(specs(mat))

    df <- as.data.frame(f)
    expect_true(is.data.frame(df))
    expect_that(ncol(df), equals(3))
    expect_that(nrow(df), equals(4))
    expect_that(colnames(df), equals(LETTERS[1:3]))
    expect_that(df[, 1], equals(m[, 1]))
    expect_that(df[, 2], equals(m[, 2]))
    expect_that(df[, 3], equals(m[, 3]))
    expect_error(vars(df))
    expect_error(specs(df))

    #---------
    v2 <- v
    names(v2) <- LETTERS[1:3]
    s2 <- s
    colnames(s2) <- LETTERS[1:3]
    rownames(s2) <- LETTERS[1:3]

    f2 <- f
    vars(f2) <- v
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    specs(f2) <- s
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    vars(f2) <- v2
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- f
    specs(f2) <- s2
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- fsets(m, vars=v2, specs=s)
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))

    f2 <- fsets(m, vars=v, specs=s2)
    expect_true(is.fsets(f2))
    expect_that(class(f2), equals(c('fsets', 'matrix')))
    expect_that(vars(f2), equals(letters[1:3]))
    expect_that(specs(f2), equals(s))
    expect_that(colnames(f2), equals(LETTERS[1:3]))
    expect_null(names(vars(f2)))
    expect_null(colnames(specs(f2)))
    expect_null(rownames(specs(f2)))
})

test_that('non fsets', {
    expect_false(is.fsets(matrix(3, ncol=3, nrow=3)))
})
