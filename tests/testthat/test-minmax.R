test_that("minmax ctx3", {
    r <- minmax(0:100)
    expect_true(is.ctx3(r))
    expect_equal(as.vector(r), c(0, 50, 100))
    expect_equal(names(r), c('low', 'center', 'high'))
})


test_that("minmax ctx3 with custom value", {
    r <- minmax(0:100, high=80)
    expect_true(is.ctx3(r))
    expect_equal(as.vector(r), c(0, 40, 80))
    expect_equal(names(r), c('low', 'center', 'high'))
})


test_that("minmax ctx3 with custom value 2", {
    r <- minmax(0:100, relCenter=0.4)
    expect_true(is.ctx3(r))
    expect_equal(as.vector(r), c(0, 40, 100))
    expect_equal(names(r), c('low', 'center', 'high'))
})


test_that("minmax ctx3bilat", {
    r <- minmax(0:100, type='ctx3bilat')
    expect_true(is.ctx3bilat(r))
    expect_equal(as.vector(r), c(0, 25, 50, 75, 100))
    expect_equal(names(r), c('negMax', 'negCenter', 'origin', 'center', 'max'))
})


test_that("minmax ctx5", {
    r <- minmax(0:100, type='ctx5')
    expect_true(is.ctx5(r))
    expect_equal(as.vector(r), c(0, 25, 50, 75, 100))
    expect_equal(names(r), c('low', 'lowerCenter', 'center', 'upperCenter', 'high'))
})


test_that("minmax ctx5bilat", {
    r <- minmax(0:100, type='ctx5bilat')
    expect_true(is.ctx5bilat(r))
    expect_equal(as.vector(r), c(0, 12.5, 25, 37.5, 50, 62.5, 75, 87.5, 100))
    expect_equal(names(r), c('negMax', 'negUpperCenter', 'negCenter', 'negLowerCenter', 'origin', 'lowerCenter', 'center', 'upperCenter', 'max'))
})

