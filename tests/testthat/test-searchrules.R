test_that('searchrules', {
    n <- 100
    d <- data.frame(a=1:n, b=n:1, c=runif(n))
    d <- lcut3(d)

    rules <- searchrules(d)
    expect_true(is.farules(rules))
})


test_that('searchrules toy example', {
    expectRule <- function(d, i, rule, support, confidence) {
        expect_equal(rownames(d)[i], rule)
        expect_equal(d[i, 'support'], support, tolerance=1e-7)
        expect_equal(d[i, 'confidence'], confidence, tolerance=1e-7)
    }

    d <- data.frame(a=c(1, 0.9, 0.8, 0.2),
                    b=c(1, 0.7, 0, 0.3),
                    c=c(1, 0.6, 0, 0.4))
    f <- fsets(x=as.matrix(d), vars=letters[1:3])
    rules <- searchrules(f, lhs=1:3, rhs=1:3, minSupport=0, minConfidence=0, maxLength=100, tnorm='goedel', trie=FALSE)

    expect_true(is.farules(rules))
    expect_equal(length(rules$rules), 12)
    expect_equal(nrow(rules$statistics), 12)

    rulesOrder <- order(laply(rules$rules, function(r) paste(r, collapse='-')))
    drules <- as.data.frame(rules)
    drules <- drules[rulesOrder, ]

    expectRule(drules, 1, " => a", 0.725, 0.725)
    expectRule(drules, 2, "b => a", 0.475, 0.95)
    expectRule(drules, 3, "b & c => a", 0.45, 0.947368421)
    expectRule(drules, 4, "c => a", 0.45, 0.9)
    expectRule(drules, 5, " => b", 0.5, 0.5)
    expectRule(drules, 6, "a => b", 0.475, 0.655172414)
    expectRule(drules, 7, "a & c => b", 0.45, 1.0)
    expectRule(drules, 8, "c => b", 0.475, 0.95)
    expectRule(drules, 9, " => c", 0.5, 0.5)
    expectRule(drules, 10, "a => c", 0.45, 0.620689655)
    expectRule(drules, 11, "a & b => c", 0.45, 0.947368421)
    expectRule(drules, 12, "b => c", 0.475, 0.95)
})
