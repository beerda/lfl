set.seed(34523)

test_that('algebraic properties', {
    for (atype in c('goedel', 'goguen', 'lukasiewicz')) {
        a <- algebra(atype)
        alg <- list(bochvar=a,
                    sobocinski=sobocinski(a),
                    kleene=kleene(a),
                    dragonfly=dragonfly(a),
                    nelson=nelson(a),
                    lowerEst=lowerEst(a))

        for (amodif in names(alg)) {
            a <- alg[[amodif]]

            for (x in c(0, 0.5, 1, NA)) {
                # residual negation is derived from residuum
                expect_equal(a$n(x), a$r(x, 0), info=amodif)

                # involutiveness of the involutive negation
                expect_equal(a$ni(a$ni(x)), x, info=amodif)

                for (y in c(0, 0.3, 0.5, 0.8, 1, NA)) {
                    # commutativity
                    expect_equal(a$t(x, y), a$t(y, x), info=amodif)
                    expect_equal(a$pt(x, y), a$pt(y, x), info=amodif)
                    expect_equal(a$c(x, y), a$c(y, x), info=amodif)
                    expect_equal(a$pc(x, y), a$pc(y, x), info=amodif)
                    expect_equal(a$s(x, y), a$s(y, x), info=amodif)
                    expect_equal(a$ps(x, y), a$ps(y, x), info=amodif)
                    expect_equal(a$i(x, y), a$i(y, x), info=amodif)
                    expect_equal(a$pi(x, y), a$pi(y, x), info=amodif)

                    # bi-residuum
                    expect_equal(a$b(x, y), a$i(a$r(x, y), a$r(y, x)), info=amodif)

                    if (!(amodif %in% c('dragonfly', 'lowerEst'))) {
                        # De-Morgan's law
                        expect_equal(a$c(!!!x, !!!y), a$ni(a$t(a$ni(!!!x), a$ni(!!!y))), info=amodif)
                    }
                }
            }
        }
    }
})
