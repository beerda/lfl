test_that('antecedents', {
    expect_equal(antecedents(list(c('a', 'b', 'c'),
                                  c('d'),
                                  c('a', 'e'))),
                 list(c('b', 'c'),
                      character(0),
                      c('e')))
    expect_error(antecedents(list(c(1, 2, 3))))
    expect_error(antecedents(list(character())))
})

test_that('antecedents (single rule)', {
    expect_equal(antecedents(list(c('a', 'b', 'c'))),
                 list(c('b', 'c')))
})

test_that('antecedents (empty list)', {
    expect_equal(antecedents(list()),
                 list())
})


test_that('antecedents (farules)', {
    f <- farules(list(letters[1:3], letters[2:5]),
                 matrix(0, nrow=2, ncol=3))
    expect_equal(antecedents(f),
                 list(letters[2:3], letters[3:5]))
})
