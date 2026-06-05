test_that("infer mamdani on fsets", {
    # init fsets
    .vars <- c(rep('b', 3),
               rep('c', 3))
    names(.vars) <- paste(rep(c('ve.sm', 'sm', 'bi'), times=2),
                          rep(c('b', 'c'), each=3),
                          sep='.')


    .specs <- matrix(c(0,1,0, 0,0,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0,

                       0,0,0, 0,1,0,
                       0,0,0, 0,0,0,
                       0,0,0, 0,0,0),
                     byrow=TRUE,
                     ncol=6)
    colnames(.specs) <- names(.vars)
    rownames(.specs) <- names(.vars)

    x <- matrix(runif(18), ncol=6, nrow=3)
    colnames(x) <- names(.vars)

    x <- fsets(x, vars=.vars, specs=.specs)

    # init rules
    rules <- list(c('sm.b', 've.sm.c'),
                  c('sm.b', 'sm.c'),
                  c('bi.b', 'bi.b'),
                  c('bi.b', 'sm.c', 'sm.b'))

    # init values
    values <- 0:10 / 10

    # init partition
    partition <- lcut(data.frame(b=values))

    res <- infer(x, rules, partition, alg = "goedel", type = "mamdani")
    expect_true(is.matrix(res))
    expect_equal(nrow(res), nrow(x))
    expect_equal(ncol(res), nrow(partition))
})


test_that("infer mamdani on custom matrix", {
    rules <- list(
        c("High",   "temp_high",   "pressure_high"),
        c("High",   "temp_high",   "humidity_low"),
        c("Medium", "temp_medium", "pressure_medium"),
        c("Low",    "temp_low"),
        c("Low",    "humidity_high", "pressure_low")
    )

    x <- matrix(
        c(
            0.9, 0.8, 0.7, 0.2, 0.3, 0.1, 0.2, 0.1,
            0.3, 0.2, 0.8, 0.7, 0.6, 0.2, 0.1, 0.4,
            0.1, 0.2, 0.2, 0.4, 0.5, 0.8, 0.7, 0.6,
            0.6, 0.5, 0.1, 0.5, 0.4, 0.3, 0.8, 0.7
        ),
        nrow = 4,
        byrow = TRUE
    )
    colnames(x) <- c("temp_high", "pressure_high", "humidity_low",
                     "temp_medium", "pressure_medium", "temp_low",
                     "humidity_high", "pressure_low")

    partition <- matrix(
        c(
            1.0, 0.2, 0.0,
            0.8, 0.5, 0.1,
            0.4, 1.0, 0.4,
            0.1, 0.5, 0.8,
            0.0, 0.1, 1.0
        ),
        nrow = 5,
        byrow = TRUE
    )
    colnames(partition) <- c("Low", "Medium", "High")
    rownames(partition) <- paste0("y", 1:5)

    expected <- matrix(
        c(
            0.2, 0.2, 0.4, 0.8, 0.8,
            0.2, 0.5, 0.6, 0.5, 0.3,
            0.8, 0.8, 0.4, 0.4, 0.1,
            0.7, 0.7, 0.4, 0.5, 0.5
        ),
        nrow = 4,
        byrow = TRUE
    )

    res <- infer(x, rules, partition, alg = "goedel", type = "mamdani")
    expect_equal(res, expected)
})


test_that("infer mamdani on empty rule set", {
    rules <- list()

    x <- matrix(
        c(
            0.9, 0.8, 0.7, 0.2, 0.3, 0.1, 0.2, 0.1,
            0.3, 0.2, 0.8, 0.7, 0.6, 0.2, 0.1, 0.4,
            0.1, 0.2, 0.2, 0.4, 0.5, 0.8, 0.7, 0.6,
            0.6, 0.5, 0.1, 0.5, 0.4, 0.3, 0.8, 0.7
        ),
        nrow = 4,
        byrow = TRUE
    )
    colnames(x) <- c("temp_high", "pressure_high", "humidity_low",
                     "temp_medium", "pressure_medium", "temp_low",
                     "humidity_high", "pressure_low")

    partition <- matrix(
        c(
            1.0, 0.2, 0.0,
            0.8, 0.5, 0.1,
            0.4, 1.0, 0.4,
            0.1, 0.5, 0.8,
            0.0, 0.1, 1.0
        ),
        nrow = 5,
        byrow = TRUE
    )
    colnames(partition) <- c("Low", "Medium", "High")
    rownames(partition) <- paste0("y", 1:5)

    expected <- matrix(
        c(
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0
        ),
        nrow = 4,
        byrow = TRUE
    )

    res <- infer(x, rules, partition, alg = "goedel", type = "mamdani")
    expect_equal(res, expected)
})


test_that("infer implicative on custom matrix", {
    rules <- list(
        c("High",   "temp_high",   "pressure_high"),
        c("High",   "temp_high",   "humidity_low"),
        c("Medium", "temp_medium", "pressure_medium"),
        c("Low",    "temp_low"),
        c("Low",    "humidity_high", "pressure_low")
    )

    x <- matrix(
        c(
            0.9, 0.8, 0.7, 0.2, 0.3, 0.1, 0.2, 0.1,
            0.3, 0.2, 0.8, 0.7, 0.6, 0.2, 0.1, 0.4,
            0.1, 0.2, 0.2, 0.4, 0.5, 0.8, 0.7, 0.6,
            0.6, 0.5, 0.1, 0.5, 0.4, 0.3, 0.8, 0.7
        ),
        nrow = 4,
        byrow = TRUE
    )
    colnames(x) <- c("temp_high", "pressure_high", "humidity_low",
                     "temp_medium", "pressure_medium", "temp_low",
                     "humidity_high", "pressure_low")

    partition <- matrix(
        c(
            1.0, 0.2, 0.0,
            0.8, 0.5, 0.1,
            0.4, 1.0, 0.4,
            0.1, 0.5, 0.8,
            0.0, 0.1, 1.0
        ),
        nrow = 5,
        byrow = TRUE
    )
    colnames(partition) <- c("Low", "Medium", "High")
    rownames(partition) <- paste0("y", 1:5)

    expected <- matrix(
        c(
            0,   0.1, 0.4, 1.0, 0,
            0,   0.1, 1.0, 0.1, 0,
            0,   1.0, 0.4, 0.1, 0,
            0,   0.1, 0.4, 0.1, 0
        ),
        nrow = 4,
        byrow = TRUE
    )

    res <- infer(x, rules, partition, alg = "goedel", type = "implicative")
    expect_equal(res, expected)
})
