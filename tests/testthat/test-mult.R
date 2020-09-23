test_that('mult', {
    set.seed(4523)
    x <- matrix(runif(24, -100, 100), ncol=6)
    y <- matrix(runif(18, -100, 100), nrow=6)
    res <- mult(x, y, function(xx, yy) sum(xx * yy))
    expect_true(is.numeric(res))
    expect_true(is.matrix(res))
    expect_equal(res, x %*% y)

    rownames(x) <- rev(LETTERS[seq_len(nrow(x))])
    colnames(x) <- letters[seq_len(ncol(x))]
    rownames(y) <- letters[seq_len(nrow(y))]
    colnames(y) <- LETTERS[seq_len(ncol(y))]
    res <- mult(x, y, function(xx, yy) sum(xx * yy))
    expect_equal(colnames(res), colnames(y))
    expect_equal(rownames(res), rownames(x))

    colnames(x) <- rev(letters[seq_len(ncol(x))])
    expect_error(mult(x, y, function(xx, yy) sum(xx * yy)))
})

