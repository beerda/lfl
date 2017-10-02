test_that("as.data.frame.farules", {
    stats <- matrix(1:9, nrow=3);
    colnames(stats) <- c('support', 'confidence', 'lift')
    res <- data.frame(stats)
    rownames(res) <- c('a & b => c', 'x & y & z => d', ' => e')

    f <- farules(list(c('c', 'a', 'b'),
                      c('d', 'x', 'y', 'z'),
                      c('e')),
                 stats)
    expect_equal(as.data.frame(f), res)
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
    expect_equal(res$rules, c(ori1$rules, ori2$rules))
    expect_equal(res$statistics, rbind(ori1$statistics, ori2$statistics))
})
