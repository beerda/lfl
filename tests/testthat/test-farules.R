test_that("farules", {
    r <- list(letters[1:3], letters[1:4])
    m <-  matrix(1:6, nrow=2)
    f <- farules(r, m)

    expect_true(is.farules(f))
    expect_true(inherits(f, 'farules'))
    expect_true(is.list(f))
    expect_true(is.list(f$rules))
    expect_true(is.matrix(f$statistics))
    expect_equal(f$rules, r)
    expect_equal(f$statistics, m)
})


test_that("as.data.frame.farules", {
    stats <- matrix(1:9, nrow=3);
    colnames(stats) <- c('support', 'confidence', 'lift')
    res <- data.frame(stats)
    rownames(res) <- c('a & b => c', 'x & y & z => d', ' => e')

    f <- farules(list(c('c', 'a', 'b'),
                      c('d', 'x', 'y', 'z'),
                      c('e')),
                 stats)
    expect_true(is.farules(f))

    d <- as.data.frame(f)
    expect_equal(d, res)
    expect_false(is.farules(d))
})


test_that('c.farules', {
    set.seed(3345)
    ori1 <- farules(rules=list(letters[1:3],
                               letters[2:5]),
                    statistics=matrix(runif(16), nrow=2))
    ori2 <- farules(rules=list(letters[4],
                               letters[3:8]),
                    statistics=matrix(runif(16), nrow=2))

    res <- c(ori1, ori2)
    expect_true(is.farules(res))
    expect_true(is.list(res))
    expect_true(is.list(res$rules))
    expect_true(is.matrix(res$statistics))
    expect_equal(res$rules, c(ori1$rules, ori2$rules))
    expect_equal(res$statistics, rbind(ori1$statistics, ori2$statistics))
})
