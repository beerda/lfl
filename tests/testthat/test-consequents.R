test_that('consequents', {
    expect_equal(consequents(list(c('a', 'b', 'c'),
                                  c('d'),
                                  c('a', 'e'))),
                 list('a', 'd', 'a'))
    expect_error(consequents(list(c(1, 2, 3))))
    expect_error(consequents(list(character())))
})


test_that('consequents (single rule)', {
    expect_equal(consequents(list(c('a', 'b', 'c'))),
                 list('a'))
})

test_that('consequents (empty list)', {
    expect_equal(consequents(list()),
                 list())
})


test_that('consequents (farules)', {
    f <- farules(list(letters[1:3], letters[2:5]),
                 matrix(0, nrow=2, ncol=3))
    expect_equal(consequents(f),
                 list(letters[1], letters[2]))
})
