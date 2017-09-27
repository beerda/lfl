test_that("mustBeNumericScalar", {
  expect_null(.mustBeNumericScalar(3))
  expect_null(.mustBeNumericScalar(0))
  expect_null(.mustBeNumericScalar(1L))
  expect_null(.mustBeNumericScalar(0.8))
  expect_error(.mustBeNumericScalar('c'))
  expect_error(.mustBeNumericScalar(1:4))
  expect_error(.mustBeNumericScalar(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeNumericScalar(NULL))
  expect_error(.mustBeNumericScalar(TRUE))
})

test_that("mustBeFactor", {
  expect_null(.mustBeFactor(factor(letters)))
  expect_error(.mustBeFactor(3))
  expect_error(.mustBeFactor(0))
  expect_error(.mustBeFactor(0.8))
  expect_error(.mustBeFactor('c'))
  expect_error(.mustBeFactor(1:4))
  expect_error(.mustBeFactor(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeFactor(NULL))
  expect_error(.mustBeFactor(TRUE))
})

test_that("mustBeIntegerScalar", {
  expect_null(.mustBeIntegerScalar(3L))
  expect_null(.mustBeIntegerScalar(0L))
  expect_error(.mustBeIntegerScalar(0.8))
  expect_error(.mustBeIntegerScalar('c'))
  expect_error(.mustBeIntegerScalar(1:4))
  expect_error(.mustBeIntegerScalar(c(1L, 2L)))
  expect_error(.mustBeIntegerScalar(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeIntegerScalar(NULL))
  expect_error(.mustBeIntegerScalar(TRUE))
})

test_that("mustBeCharacterScalar", {
  expect_null(.mustBeCharacterScalar('c'))
  expect_null(.mustBeCharacterScalar('hello'))
  expect_error(.mustBeCharacterScalar(1))
  expect_error(.mustBeCharacterScalar(c('a', 'b')))
  expect_error(.mustBeCharacterScalar(matrix('c', nrow=1, ncol=1)))
  expect_error(.mustBeCharacterScalar(NULL))
  expect_error(.mustBeCharacterScalar(TRUE))
})

test_that("mustBeNumericVector", {
  expect_null(.mustBeNumericVector(3))
  expect_null(.mustBeNumericVector(0))
  expect_null(.mustBeNumericVector(0.8))
  expect_null(.mustBeNumericVector(1:4))
  expect_error(.mustBeNumericVector('c'))
  expect_error(.mustBeNumericVector(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeNumericVector(NULL))
  expect_error(.mustBeNumericVector(TRUE))
  expect_error(.mustBeNumericVector(FALSE))
})

test_that("mustBeLogicalVector", {
  expect_error(.mustBeLogicalVector(3))
  expect_error(.mustBeLogicalVector(0))
  expect_error(.mustBeLogicalVector(0.8))
  expect_error(.mustBeLogicalVector(1:4))
  expect_error(.mustBeLogicalVector('c'))
  expect_error(.mustBeLogicalVector(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeLogicalVector(NULL))
  expect_null(.mustBeLogicalVector(TRUE))
  expect_null(.mustBeLogicalVector(FALSE))
  expect_null(.mustBeLogicalVector(c(T,F,F)))
})

test_that("mustBeCharacterVector", {
  expect_null(.mustBeCharacterVector('c'))
  expect_null(.mustBeCharacterVector('hello'))
  expect_null(.mustBeCharacterVector(c('a', 'b')))
  expect_error(.mustBeCharacterVector(1))
  expect_error(.mustBeCharacterVector(matrix('c', nrow=1, ncol=1)))
  expect_error(.mustBeCharacterVector(NULL))
  expect_error(.mustBeCharacterVector(TRUE))
})

test_that("mustBeList", {
  expect_null(.mustBeList(list()))
  expect_null(.mustBeList(list(a=1, b=2)))
  expect_error(.mustBeList(1))
  expect_error(.mustBeList('a'))
  expect_error(.mustBeList(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeList(NULL))
  expect_error(.mustBeList(TRUE))
  expect_error(.mustBeList(NA))
})

test_that("mustBeMatrix", {
  expect_null(.mustBeMatrix(matrix(1, nrow=1, ncol=1)))
  expect_null(.mustBeMatrix(matrix(1, nrow=3, ncol=5)))
  expect_null(.mustBeMatrix(matrix('a', nrow=1, ncol=1)))
  expect_null(.mustBeMatrix(matrix('a', nrow=3, ncol=5)))
  expect_null(.mustBeMatrix(matrix(TRUE, nrow=1, ncol=1)))

  expect_error(.mustBeMatrix(list(a=3)))
  expect_error(.mustBeMatrix(1:5))
  expect_error(.mustBeMatrix(TRUE))
  expect_error(.mustBeMatrix(NULL))
  expect_error(.mustBeMatrix(letters[1:5]))
})

test_that("mustBeNumericMatrix", {
  expect_null(.mustBeNumericMatrix(matrix(1, nrow=1, ncol=1)))
  expect_null(.mustBeNumericMatrix(matrix(1, nrow=3, ncol=5)))

  expect_error(.mustBeNumericMatrix(matrix('a', nrow=1, ncol=1)))
  expect_error(.mustBeNumericMatrix(matrix('a', nrow=3, ncol=5)))
  expect_error(.mustBeNumericMatrix(matrix(TRUE, nrow=1, ncol=1)))
  expect_error(.mustBeNumericMatrix(list(a=3)))
  expect_error(.mustBeNumericMatrix(1:5))
  expect_error(.mustBeNumericMatrix(TRUE))
  expect_error(.mustBeNumericMatrix(NULL))
  expect_error(.mustBeNumericMatrix(letters[1:5]))
})

test_that("mustBeDataFrame", {
  expect_null(.mustBeDataFrame(data.frame(a=1:5, b=1:5)))

  expect_error(.mustBeDataFrame(matrix(1, nrow=1, ncol=1)))
  expect_error(.mustBeDataFrame(matrix(1, nrow=3, ncol=5)))
  expect_error(.mustBeDataFrame(matrix('a', nrow=1, ncol=1)))
  expect_error(.mustBeDataFrame(matrix('a', nrow=3, ncol=5)))
  expect_error(.mustBeDataFrame(matrix(TRUE, nrow=1, ncol=1)))
  expect_error(.mustBeDataFrame(list(a=3)))
  expect_error(.mustBeDataFrame(1:5))
  expect_error(.mustBeDataFrame(TRUE))
  expect_error(.mustBeDataFrame(NULL))
  expect_error(.mustBeDataFrame(letters[1:5]))
})

test_that("mustBeTs", {
  expect_null(.mustBeTs(stats::ts(1:5)))

  expect_error(.mustBeTs(matrix(1, nrow=3, ncol=5)))
  expect_error(.mustBeTs(matrix('a', nrow=3, ncol=5)))
  expect_error(.mustBeTs(matrix(TRUE, nrow=1, ncol=1)))
  expect_error(.mustBeTs(list(a=3)))
  expect_error(.mustBeTs(1:5))
  expect_error(.mustBeTs(TRUE))
  expect_error(.mustBeTs(NULL))
  expect_error(.mustBeTs(letters[1:5]))
})

test_that("mustBeFunction", {
  expect_null(.mustBeFunction(function(x) {}))
  expect_null(.mustBeFunction(identity))

  expect_error(.mustBeFunction(matrix(1, nrow=3, ncol=5)))
  expect_error(.mustBeFunction(list(a=3)))
  expect_error(.mustBeFunction(1:5))
  expect_error(.mustBeFunction(TRUE))
  expect_error(.mustBeFunction(NULL))
})

test_that("mustNotBeNull", {
  expect_null(.mustNotBeNull(matrix(1, nrow=3, ncol=5)))
  expect_null(.mustNotBeNull(list(a=3)))
  expect_null(.mustNotBeNull(1:5))
  expect_null(.mustNotBeNull(TRUE))

  expect_error(.mustNotBeNull(NULL))
})

test_that("mustNotHaveNA", {
  expect_null(.mustNotHaveNA(matrix(1, nrow=3, ncol=5)))
  expect_null(.mustNotHaveNA(list(a=3)))
  expect_null(.mustNotHaveNA(1:5))
  expect_null(.mustNotHaveNA(TRUE))
  expect_null(.mustNotHaveNA(NULL))

  expect_error(.mustNotHaveNA(NA))
  expect_error(.mustNotHaveNA(c(1:5, NA)))
  expect_error(.mustNotHaveNA(matrix(NA, nrow=3, ncol=5)))
})

test_that("mustNotBeZeroLength", {
  expect_null(.mustNotBeZeroLength(1))
  expect_null(.mustNotBeZeroLength(letters))
  expect_error(.mustNotBeZeroLength(NULL))
})

test_that("stop", {
  expect_error(.stop())
})

test_that("mustBe", {
  expect_null(.mustBe(TRUE, "message"))
  expect_error(.mustBe(FALSE, "message"))
  expect_error(.mustBe(NA, "message"))
  expect_error(.mustBe(c(TRUE, TRUE), "message"))
})

test_that("mustBeOneOf", {
  expect_null(.mustBeOneOf('x', c(letters)))
  expect_error(.mustBeOneOf('X', c(letters)))
})
