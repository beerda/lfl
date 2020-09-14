set.seed(34523)

test_that('minimum (Goedel) t-norm', {
    expect_that(goedel.tnorm(0.2, 0.5, 0.1, 0.3), equals(0.1))
    expect_that(goedel.tnorm(0.4, 0.5, 0.3), equals(0.3))
    expect_that(goedel.tnorm(0.2, 0.5, 0.9), equals(0.2))
    expect_that(goedel.tnorm(0.2, 0.5, 0.0), equals(0))
    expect_that(goedel.tnorm(1, 1, 1, 1), equals(1))
    expect_that(goedel.tnorm(1, 0.9, 1, 1), equals(0.9))

    expect_that(goedel.tnorm(c(0.2, 0.5, 0.1, 0.3)), equals(0.1))
    expect_that(goedel.tnorm(c(0.4, 0.5, 0.3)), equals(0.3))
    expect_that(goedel.tnorm(c(0.2, 0.5, 0.9)), equals(0.2))
    expect_that(goedel.tnorm(c(0.2, 0.5, 0.0)), equals(0))
    expect_that(goedel.tnorm(c(1, 1, 1, 1)), equals(1))
    expect_that(goedel.tnorm(c(1, 0.9, 1, 1)), equals(0.9))
})


test_that('lukasiewicz t-norm', {
    expect_that(lukas.tnorm(0.2, 0.5, 0.1, 0.3), equals(0))
    expect_that(lukas.tnorm(0.8, 0.5, 0.9), equals(0.2))
    expect_that(lukas.tnorm(1, 1, 1, 1), equals(1))
    expect_that(lukas.tnorm(1, 0.9, 1, 1), equals(0.9))
    expect_that(lukas.tnorm(1, 0.9, 0.8, 1), equals(0.7))
    expect_that(lukas.tnorm(0.2, 0.5, 0.0), equals(0))

    expect_that(lukas.tnorm(c(0.2, 0.5, 0.1, 0.3)), equals(0))
    expect_that(lukas.tnorm(c(0.8, 0.5, 0.9)), equals(0.2))
    expect_that(lukas.tnorm(c(1, 1, 1, 1)), equals(1))
    expect_that(lukas.tnorm(c(1, 0.9, 1, 1)), equals(0.9))
    expect_that(lukas.tnorm(c(0.2, 0.5, 0.0)), equals(0))
})


test_that('product (goguen) t-norm', {
    expect_that(goguen.tnorm(0.2, 0.5, 0.1, 0.3), equals(0.2 * 0.5 * 0.1 * 0.3))
    expect_that(goguen.tnorm(0.8, 0.5, 0.9), equals(0.8 * 0.5 * 0.9))
    expect_that(goguen.tnorm(1, 1, 1, 1), equals(1))
    expect_that(goguen.tnorm(1, 0.9, 1, 1), equals(0.9))
    expect_that(goguen.tnorm(0.2, 0.5, 0.0), equals(0))

    expect_that(goguen.tnorm(c(0.2, 0.5, 0.1, 0.3)), equals(0.2 * 0.5 * 0.1 * 0.3))
    expect_that(goguen.tnorm(c(0.8, 0.5, 0.9)), equals(0.8 * 0.5 * 0.9))
    expect_that(goguen.tnorm(c(1, 1, 1, 1)), equals(1))
    expect_that(goguen.tnorm(c(1, 0.9, 1, 1)), equals(0.9))
    expect_that(goguen.tnorm(c(0.2, 0.5, 0.0)), equals(0))
})


for (ttt in names(.tnorms)) {
    test_that(paste(ttt, 't-norm borders'), {
        tnorm <- .tnorms[[ttt]]

        expect_that(tnorm(), equals(NA_real_))
        expect_that(tnorm(0.2, NA, 1), equals(NA_real_))
        expect_that(tnorm(0.2, NA, 0), equals(NA_real_))
        expect_that(tnorm(0.2, NaN, 1), equals(NA_real_))
        expect_that(tnorm(0.2, NaN, 0), equals(NA_real_))
        expect_that(tnorm(0.2, Inf, 0), throws_error('argument out of range 0..1'))
        expect_that(tnorm(0.2, -Inf, 0), throws_error('argument out of range 0..1'))
        expect_that(tnorm(0.2, 3, 0), throws_error('argument out of range 0..1'))
        expect_that(tnorm(0.2, -3, 0), throws_error('argument out of range 0..1'))

        expect_that(tnorm(c()), equals(NA_real_))
        expect_that(tnorm(c(0.2, NA, 1)), equals(NA_real_))
        expect_that(tnorm(c(0.2, NA, 0)), equals(NA_real_))
        expect_that(tnorm(c(0.2, NaN, 1)), equals(NA_real_))
        expect_that(tnorm(c(0.2, NaN, 0)), equals(NA_real_))
        expect_that(tnorm(c(0.2, Inf, 0)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, -Inf, 0)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, 3, 0)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, -3, 0)), throws_error('argument out of range 0..1'))
    })
}


test_that('parallel minimum t-norm', {
    expect_that(pgoedel.tnorm(c(0.2, 0.5, 0.4, 0.9, 1),
                    c(0.5, 0.9, 0.8, 1.0, 1),
                    c(0.6, 0.4, 0.0, 1.0, 1),
                    c(0.3, 0.7, 0.5, 1.0, 1)),
                equals(c(0.2, 0.4, 0.0, 0.9, 1)))

    expect_that(pgoedel.tnorm(0.2, 0.5), equals(0.2))
    expect_that(pgoedel.tnorm(0.2, 0.5, 0.0), equals(0))
    expect_that(pgoedel.tnorm(c(0.2, 0.5, 0.0)), equals(c(0.2, 0.5, 0.0)))
})


test_that('parallel lukasiewicz t-norm', {
    expect_that(plukas.tnorm(c(0.2, 0.8, 0.4, 0.9, 1),
                    c(0.5, 0.9, 0.8, 1.0, 1),
                    c(0.6, 0.5, 0.0, 1.0, 1),
                    c(0.3, 0.9, 0.5, 1.0, 1)),
                equals(c(0, 0.1, 0.0, 0.9, 1)))

    expect_that(plukas.tnorm(0.7, 0.8, 0.6), equals(0.1))
    expect_that(plukas.tnorm(0.2, 0.5, 0.0), equals(0))
    expect_that(plukas.tnorm(c(0.2, 0.5, 0.0)), equals(c(0.2, 0.5, 0.0)))
})


test_that('parallel product t-norm', {
    expect_that(pgoguen.tnorm(c(0.2, 0.5, 0.4, 0.9, 1),
                    c(0.5, 0.9, 0.8, 1.0, 1),
                    c(0.6, 0.4, 0.0, 1.0, 1),
                    c(0.3, 0.7, 0.5, 1.0, 1)),
                equals(c(0.2 * 0.5 * 0.6 * 0.3,
                        0.5 * 0.9 * 0.4 * 0.7,
                        0.0, 0.9, 1)))

    expect_that(pgoguen.tnorm(0.2, 0.5), equals(0.2 * 0.5))
    expect_that(pgoguen.tnorm(0.2, 0.5, 0.0), equals(0))
    expect_that(pgoguen.tnorm(c(0.2, 0.5, 0.0)), equals(c(0.2, 0.5, 0.0)))
})


for (ttt in names(.ptnorms)) {
    test_that(paste('parallel', ttt, 't-norm borders'), {
        tnorm <- .ptnorms[[ttt]]

        expect_true(is.null(tnorm()))
        expect_that(tnorm(c(0.2, NA, 1), c(0.8, 0.6, NA))[2:3], equals(as.numeric(c(NA, NA))))
        expect_that(tnorm(c(0.2, NA, 0), c(0.8, 0, NA))[2:3], equals(as.numeric(c(NA, NA))))
        expect_that(tnorm(c(0.2, NaN, 1), c(0.8, 0.6, NA))[2:3], equals(as.numeric(c(NA, NA))))
        expect_that(tnorm(c(0.2, NaN, 0), c(0.8, 0, NaN))[2:3], equals(as.numeric(c(NA, NA))))
        expect_that(tnorm(c(0.2, 0.9, 0), c(0.8, 0, Inf)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, 0.9, 0), c(0.8, 0, -Inf)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, 0.9, 0), c(0.8, 0, 3)), throws_error('argument out of range 0..1'))
        expect_that(tnorm(c(0.2, 0.9, 0), c(0.8, 0, -3)), throws_error('argument out of range 0..1'))

        mr <- matrix(runif(12), nrow=3, ncol=4)
        colnames(mr) <- LETTERS[1:4]
        rownames(mr) <- letters[1:3]

        m0 <- matrix(0, nrow=3, ncol=4)
        colnames(m0) <- colnames(mr)
        rownames(m0) <- rownames(mr)
        expect_that(tnorm(mr, 0), equals(m0))
        expect_that(tnorm(mr, m0), equals(m0))

        m1 <- matrix(1, nrow=3, ncol=4)
        expect_that(tnorm(mr, 1), equals(mr))
        expect_that(tnorm(mr, m1), equals(mr))

        mx <- matrix(tnorm(c(mr), c(mr)), nrow=3, ncol=4)
        colnames(mx) <- colnames(mr)
        rownames(mx) <- rownames(mr)
        expect_that(tnorm(mr, mr), equals(mx))
    })
}


test_that('goedel t-conorm', {
    expect_that(goedel.tconorm(0.2, 0.5, 0.1, 0.3), equals(0.5))
    expect_that(goedel.tconorm(0.4, 0.5, 0.8), equals(0.8))
    expect_that(goedel.tconorm(0.9, 0.5, 0.2), equals(0.9))
    expect_that(goedel.tconorm(0.2, 1, 0.0), equals(1))
    expect_that(goedel.tconorm(0, 0, 0, 0), equals(0))

    expect_that(goedel.tconorm(c(0.2, 0.5, 0.1, 0.3)), equals(0.5))
    expect_that(goedel.tconorm(c(0.4, 0.5, 0.8)), equals(0.8))
    expect_that(goedel.tconorm(c(0.9, 0.5, 0.2)), equals(0.9))
    expect_that(goedel.tconorm(c(0.2, 1, 0.0)), equals(1))
    expect_that(goedel.tconorm(c(0, 0, 0, 0)), equals(0))
})


test_that('lukasiewicz t-conorm', {
    expect_that(lukas.tconorm(0.2, 0.5, 0.1, 0.0), equals(0.8))
    expect_that(lukas.tconorm(0.4, 0.5, 0.8), equals(1))
    expect_that(lukas.tconorm(1, 1, 1), equals(1))
    expect_that(lukas.tconorm(0, 0, 0, 0), equals(0))

    expect_that(lukas.tconorm(c(0.2, 0.5, 0.1, 0.0)), equals(0.8))
    expect_that(lukas.tconorm(c(0.4, 0.5, 0.8)), equals(1))
    expect_that(lukas.tconorm(c(1, 1, 1)), equals(1))
    expect_that(lukas.tconorm(c(0, 0, 0, 0)), equals(0))
})


test_that('goguen t-conorm', {
    expect_that(goguen.tconorm(0.2, 0.5, 0.1, 0.3), equals(0.748))
    expect_that(goguen.tconorm(0.2, 1, 0.0), equals(1))
    expect_that(goguen.tconorm(0, 0, 0, 0), equals(0))

    expect_that(goguen.tconorm(c(0.2, 0.5, 0.1, 0.3)), equals(0.748))
    expect_that(goguen.tconorm(c(0.2, 1, 0.0)), equals(1))
    expect_that(goguen.tconorm(c(0, 0, 0, 0)), equals(0))
})


for (ttt in names(.tconorms)) {
    test_that(paste(ttt, 't-conorm borders'), {
        tconorm <- .tconorms[[ttt]]

        expect_that(tconorm(), equals(NA_real_))
        expect_that(tconorm(0.2, NA, 0), equals(NA_real_))
        expect_that(tconorm(0.2, NA, 1), equals(NA_real_))
        expect_that(tconorm(0.2, Inf, 0), throws_error('argument out of range 0..1'))
        expect_that(tconorm(0.2, -Inf, 0), throws_error('argument out of range 0..1'))
        expect_that(tconorm(0.2, 3, 0), throws_error('argument out of range 0..1'))
        expect_that(tconorm(0.2, -3, 0), throws_error('argument out of range 0..1'))

        expect_that(tconorm(c()), equals(NA_real_))
        expect_that(tconorm(c(0.2, NA, 0)), equals(NA_real_))
        expect_that(tconorm(c(0.2, NA, 1)), equals(NA_real_))
        expect_that(tconorm(c(0.2, Inf, 0)), throws_error('argument out of range 0..1'))
        expect_that(tconorm(c(0.2, -Inf, 0)), throws_error('argument out of range 0..1'))
        expect_that(tconorm(c(0.2, 3, 0)), throws_error('argument out of range 0..1'))
        expect_that(tconorm(c(0.2, -3, 0)), throws_error('argument out of range 0..1'))
    })
}


test_that('goedel residuum', {
    expect_that(goedel.residuum(c(0, 0.2, 0.8, 1), 1), equals(c(1, 1, 1, 1)))
    expect_that(goedel.residuum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0, 0, 0)))
    expect_that(goedel.residuum(c(0, 0.2, 0.8, 1), 0.5), equals(c(1, 1, 0.5, 0.5)))
    expect_that(goedel.residuum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(1, 1, 0.3, 0.9)))
})


test_that('lukasiewicz residuum', {
    expect_that(lukas.residuum(c(0, 0.2, 0.8, 1), 1), equals(c(1, 1, 1, 1)))
    expect_that(lukas.residuum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0.8, 0.2, 0)))
    expect_that(lukas.residuum(c(0, 0.2, 0.8, 1), 0.5), equals(c(1, 1, 0.7, 0.5)))
    expect_that(lukas.residuum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(1, 1, 0.5, 0.9)))
})


test_that('goguen residuum', {
    expect_that(goguen.residuum(c(0, 0.2, 0.8, 1), 1), equals(c(1, 1, 1, 1)))
    expect_that(goguen.residuum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0, 0, 0)))
    expect_that(goguen.residuum(c(0, 0.2, 0.8, 1), 0.5), equals(c(1, 1, 0.625, 0.5)))
    expect_that(goguen.residuum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(1, 1, 0.375, 0.9)))
})


test_that('goedel bi-residuum', {
    expect_that(goedel.biresiduum(c(0, 0.2, 0.8, 1), 1), equals(c(0, 0.2, 0.8, 1)))
    expect_that(goedel.biresiduum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0, 0, 0)))
    expect_that(goedel.biresiduum(c(0, 0.2, 0.8, 1), 0.5), equals(c(0, 0.2, 0.5, 0.5)))
    expect_that(goedel.biresiduum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(0, 0.2, 0.3, 0.9)))
})


test_that('lukasiewicz bi-residuum', {
    expect_that(lukas.biresiduum(c(0, 0.2, 0.8, 1), 1), equals(c(0, 0.2, 0.8, 1)))
    expect_that(lukas.biresiduum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0.8, 0.2, 0)))
    expect_that(lukas.biresiduum(c(0, 0.2, 0.8, 1), 0.5), equals(c(0.5, 0.7, 0.7, 0.5)))
    expect_that(lukas.biresiduum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(0.7, 0.3, 0.5, 0.9)))
})


test_that('goguen bi-residuum', {
    expect_that(goguen.biresiduum(c(0, 0.2, 0.8, 1), 1), equals(c(0, 0.2, 0.8, 1)))
    expect_that(goguen.biresiduum(c(0, 0.2, 0.8, 1), 0), equals(c(1, 0, 0, 0)))
    expect_that(goguen.biresiduum(c(0, 0.2, 0.8, 1), 0.5), equals(c(0, 2/5, 5/8, 0.5)))
    expect_that(goguen.biresiduum(c(0, 0.2, 0.8, 1), c(0.3, 0.9)), equals(c(0, 2/9, 3/8, 0.9)))
})


for (ttt in names(.residua)) {
    test_that(paste(ttt, 'residua borders'), {
        resid <- .residua[[ttt]]

        expect_that(resid(0, NA), equals(NA_real_))
        expect_that(resid(0.4, NA), equals(NA_real_))
        expect_that(resid(1, NA), equals(NA_real_))
        expect_that(resid(0.2, Inf), throws_error('argument out of range 0..1'))
        expect_that(resid(0.2, -Inf), throws_error('argument out of range 0..1'))
        expect_that(resid(0.2, 3), throws_error('argument out of range 0..1'))
        expect_that(resid(0.2, -3), throws_error('argument out of range 0..1'))

        expect_that(resid(NA, 0), equals(NA_real_))
        expect_that(resid(NA, 0.2), equals(NA_real_))
        expect_that(resid(NA, 1), equals(NA_real_))
        expect_that(resid(Inf, 0.2), throws_error('argument out of range 0..1'))
        expect_that(resid(-Inf, 0.2), throws_error('argument out of range 0..1'))
        expect_that(resid(3, 0.2), throws_error('argument out of range 0..1'))
        expect_that(resid(-3, 0.2), throws_error('argument out of range 0..1'))
    })
}


test_that('involutive negation', {
    expect_that(invol.neg(c(0, 0.2, NA, 0.8, 1, NaN)), equals(c(1, 0.8, NA, 0.2, 0, NA)))

    m <- matrix(c(0, 0.2, NA, 0.8, 1, 0.3), nrow=2)
    colnames(m) <- letters[1:3]
    rownames(m) <- letters[1:2]
    r <- matrix(c(1, 0.8, NA, 0.2, 0, 0.7), nrow=2)
    colnames(r) <- letters[1:3]
    rownames(r) <- letters[1:2]
    expect_that(invol.neg(m), equals(r))

    expect_that(invol.neg(c(-3, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(invol.neg(c(3, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(invol.neg(c(-Inf, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(invol.neg(c(Inf, 0.2)), throws_error('argument out of range 0..1'))
})


test_that('strict negation', {
    expect_that(strict.neg(c(0, 0.2, NA, 0.8, 1, NaN)), equals(c(1, 0, NA, 0, 0, NA)))

    m <- matrix(c(0, 0.2, NA, 0.8, 1, 0.3), nrow=2)
    colnames(m) <- letters[1:3]
    rownames(m) <- letters[1:2]
    r <- matrix(c(1, 0, NA, 0, 0, 0), nrow=2)
    colnames(r) <- letters[1:3]
    rownames(r) <- letters[1:2]
    expect_that(strict.neg(m), equals(r))

    expect_that(strict.neg(c(-3, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(strict.neg(c(3, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(strict.neg(c(-Inf, 0.2)), throws_error('argument out of range 0..1'))
    expect_that(strict.neg(c(Inf, 0.2)), throws_error('argument out of range 0..1'))
})


test_that('algebra', {
  o <- c(0.5, 1, 0, 0.8, NA, 0.3)
  oi <- c(3, 6, 1, 4, 2, 5)
  od <- c(2, 4, 1, 6, 3, 5)

  a <- algebra('goe')
  expect_true(inherits(a, 'algebra'))
  expect_true(inherits(a, 'list'))
  expect_true(is.algebra(a))
  expect_that(a$t, equals(goedel.tnorm))
  expect_that(a$pt, equals(pgoedel.tnorm))
  expect_that(a$c, equals(goedel.tconorm))
  expect_that(a$pc, equals(pgoedel.tconorm))
  expect_that(a$r, equals(goedel.residuum))
  expect_that(a$b, equals(goedel.biresiduum))
  expect_that(a$n, equals(strict.neg))
  expect_that(a$ni, equals(invol.neg))
  expect_that(a$s, equals(goedel.tconorm))
  expect_that(a$ps, equals(pgoedel.tconorm))
  expect_that(a$i, equals(goedel.tnorm))
  expect_that(a$pi, equals(pgoedel.tnorm))
  expect_that(a$algebratype, equals('goedel'))
  expect_equal(a$order(o), oi)
  expect_equal(a$order(o, decreasing=TRUE), od)

  a <- algebra('lukas')
  expect_true(inherits(a, 'algebra'))
  expect_true(inherits(a, 'list'))
  expect_true(is.algebra(a))
  expect_that(a$t, equals(lukas.tnorm))
  expect_that(a$pt, equals(plukas.tnorm))
  expect_that(a$c, equals(lukas.tconorm))
  expect_that(a$pc, equals(plukas.tconorm))
  expect_that(a$r, equals(lukas.residuum))
  expect_that(a$b, equals(lukas.biresiduum))
  expect_that(a$n, equals(invol.neg))
  expect_that(a$ni, equals(invol.neg))
  expect_that(a$s, equals(goedel.tconorm))
  expect_that(a$ps, equals(pgoedel.tconorm))
  expect_that(a$i, equals(goedel.tnorm))
  expect_that(a$pi, equals(pgoedel.tnorm))
  expect_that(a$algebratype, equals('lukasiewicz'))
  expect_equal(a$order(o), oi)
  expect_equal(a$order(o, decreasing=TRUE), od)

  a <- algebra('gog')
  expect_true(inherits(a, 'algebra'))
  expect_true(inherits(a, 'list'))
  expect_true(is.algebra(a))
  expect_that(a$t, equals(goguen.tnorm))
  expect_that(a$pt, equals(pgoguen.tnorm))
  expect_that(a$c, equals(goguen.tconorm))
  expect_that(a$pc, equals(pgoguen.tconorm))
  expect_that(a$r, equals(goguen.residuum))
  expect_that(a$b, equals(goguen.biresiduum))
  expect_that(a$n, equals(strict.neg))
  expect_that(a$ni, equals(invol.neg))
  expect_that(a$s, equals(goedel.tconorm))
  expect_that(a$ps, equals(pgoedel.tconorm))
  expect_that(a$i, equals(goedel.tnorm))
  expect_that(a$pi, equals(pgoedel.tnorm))
  expect_that(a$algebratype, equals('goguen'))
  expect_equal(a$order(o), oi)
  expect_equal(a$order(o, decreasing=TRUE), od)
})
