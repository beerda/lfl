m <- matrix(1:10/10, ncol=2, byrow=TRUE)
colnames(m) <- c('a', 'b')
d <- as.data.frame(m)
#       a    b
# [1,]  0.1  0.2
# [2,]  0.3  0.4
# [3,]  0.5  0.6
# [4,]  0.7  0.8
# [5,]  0.9  1.0




test_that('lcut on vector', {
    testedHedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr", "ty")
    testedAtomic <- c("sm", "me", "bi")

    smHedgeNames <- c("ex", "si", "ve", "", "ml", "ro", "qr", "vr")
    meHedgeNames <- c("ty", "", "ml", "ro", "qr", "vr")
    biHedgeNames <- smHedgeNames

    res <- lcut(d$a,
                context=ctx3(0, 0.5, 1),
                atomic=testedAtomic,
                hedges=testedHedges,
                name='a')

    expectedAttrs <- c(paste(smHedgeNames, 'sm', 'a', sep='.'),
                       paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'))
    expectedAttrs <- sub('^\\.', '', expectedAttrs)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 22)
    expect_equal(nrow(res), 5)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), rep('a', 22))

    s <- matrix(c(0,1,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #SiSm
                  0,0,0,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VeSm
                  0,0,0,0,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #Sm
                  0,0,0,0,0,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #MlSm
                  0,0,0,0,0,0,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,0,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0,0,0, 0,1,1,1,1,1, 0,0,0,0,0,0,0,0,  #TyMe
                  0,0,0,0,0,0,0,0, 0,0,1,1,1,1, 0,0,0,0,0,0,0,0,  #Me
                  0,0,0,0,0,0,0,0, 0,0,0,1,1,1, 0,0,0,0,0,0,0,0,  #MlMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,1,1, 0,0,0,0,0,0,0,0,  #RoMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,1, 0,0,0,0,0,0,0,0,  #QrMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrMe

                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,1,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,1,1,1,1,1,1,  #SiBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,1,1,1,1,1,  #VeBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0), #VrBi
                nrow=22,
                ncol=22,
                byrow=TRUE)
    expect_equal(specs(res), s)
})



test_that('lcut on vector (some hedges disabled)', {
    testedHedges <- c("ex", "ve", "ml", "vr", "ty")
    testedAtomic <- c("me", "bi")

    meHedgeNames <- c("ty", "ml", "vr")
    biHedgeNames <- c("ex", "ve", "ml", "vr")

    expectedAttrs <- c(paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'))

    res <- lcut(d$a,
                 context=ctx3(0, 0.5, 1),
                 atomic=testedAtomic,
                 hedges=testedHedges,
                 name='a')

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 7)
    expect_equal(nrow(res), 5)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), rep('a', 7))

    s <- matrix(c(0,1,1, 0,0,0,0,  #TyMe
                  0,0,1, 0,0,0,0,  #MlMe
                  0,0,0, 0,0,0,0,  #VrMe

                  0,0,0, 0,1,1,1,  #ExBi
                  0,0,0, 0,0,1,1,  #VeBi
                  0,0,0, 0,0,0,1,  #MlBi
                  0,0,0, 0,0,0,0), #VrBi
                nrow=7,
                ncol=7,
                byrow=TRUE)

    expect_equal(specs(res), s)
    expect_true(is.fsets(res))
})


test_that('lcut on matrix', {
    testedHedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr", "ty")
    testedAtomic <- c("sm", "me", "bi")

    smHedgeNames <- c("ex", "si", "ve", "", "ml", "ro", "qr", "vr")
    meHedgeNames <- c("ty", "", "ml", "ro", "qr", "vr")
    biHedgeNames <- smHedgeNames

    expectedAttrs <- c(paste(smHedgeNames, 'sm', 'a', sep='.'),
                       paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'),
                       paste(smHedgeNames, 'sm', 'b', sep='.'),
                       paste(meHedgeNames, 'me', 'b', sep='.'),
                       paste(biHedgeNames, 'bi', 'b', sep='.'))
    expectedAttrs <- sub('^\\.', '', expectedAttrs)

    res <- lcut(as.matrix(m),
                context=ctx3(0, 0.5, 1),
                hedges=testedHedges)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 44)
    expect_equal(nrow(res), 5)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), c(rep('a', 22), rep('b', 22)))

    s <- matrix(c(0,1,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #SiSm
                  0,0,0,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VeSm
                  0,0,0,0,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #Sm
                  0,0,0,0,0,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #MlSm
                  0,0,0,0,0,0,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,0,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0,0,0, 0,1,1,1,1,1, 0,0,0,0,0,0,0,0,  #TyMe
                  0,0,0,0,0,0,0,0, 0,0,1,1,1,1, 0,0,0,0,0,0,0,0,  #Me
                  0,0,0,0,0,0,0,0, 0,0,0,1,1,1, 0,0,0,0,0,0,0,0,  #MlMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,1,1, 0,0,0,0,0,0,0,0,  #RoMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,1, 0,0,0,0,0,0,0,0,  #QrMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrMe

                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,1,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,1,1,1,1,1,1,  #SiBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,1,1,1,1,1,  #VeBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0), #VrBi
                byrow=TRUE,
                nrow=22,
                ncol=22)
    sfill <- matrix(0, nrow=22, ncol=22)
    s <- cbind(rbind(s, sfill), rbind(sfill, s))

    expect_equal(specs(res), s)
})


test_that('lcut on data frame', {
    testedHedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr", "ty")
    testedAtomic <- c("sm", "me", "bi")

    smHedgeNames <- c("ex", "si", "ve", "", "ml", "ro", "qr", "vr")
    meHedgeNames <- c("ty", "", "ml", "ro", "qr", "vr")
    biHedgeNames <- smHedgeNames

    expectedAttrs <- c(paste(smHedgeNames, 'sm', 'a', sep='.'),
                       paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'),
                       paste(smHedgeNames, 'sm', 'b', sep='.'),
                       paste(meHedgeNames, 'me', 'b', sep='.'),
                       paste(biHedgeNames, 'bi', 'b', sep='.'))
    expectedAttrs <- sub('^\\.', '', expectedAttrs)

    res <- lcut(as.data.frame(m),
                context=ctx3(0, 0.5, 1),
                hedges=testedHedges)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 44)
    expect_equal(nrow(res), 5)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), c(rep('a', 22), rep('b', 22)))

    s <- matrix(c(0,1,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #SiSm
                  0,0,0,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VeSm
                  0,0,0,0,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #Sm
                  0,0,0,0,0,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #MlSm
                  0,0,0,0,0,0,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,0,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0,0,0, 0,1,1,1,1,1, 0,0,0,0,0,0,0,0,  #TyMe
                  0,0,0,0,0,0,0,0, 0,0,1,1,1,1, 0,0,0,0,0,0,0,0,  #Me
                  0,0,0,0,0,0,0,0, 0,0,0,1,1,1, 0,0,0,0,0,0,0,0,  #MlMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,1,1, 0,0,0,0,0,0,0,0,  #RoMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,1, 0,0,0,0,0,0,0,0,  #QrMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrMe

                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,1,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,1,1,1,1,1,1,  #SiBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,1,1,1,1,1,  #VeBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0), #VrBi
                byrow=TRUE,
                nrow=22,
                ncol=22)
    sfill <- matrix(0, nrow=22, ncol=22)
    s <- cbind(rbind(s, sfill), rbind(sfill, s))

    expect_equal(specs(res), s)
})


test_that('lcut on single row data frame', {
    testedHedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr", "ty")
    testedAtomic <- c("sm", "me", "bi")

    smHedgeNames <- c("ex", "si", "ve", "", "ml", "ro", "qr", "vr")
    meHedgeNames <- c("ty", "", "ml", "ro", "qr", "vr")
    biHedgeNames <- smHedgeNames

    expectedAttrs <- c(paste(smHedgeNames, 'sm', 'a', sep='.'),
                       paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'),
                       paste(smHedgeNames, 'sm', 'b', sep='.'),
                       paste(meHedgeNames, 'me', 'b', sep='.'),
                       paste(biHedgeNames, 'bi', 'b', sep='.'))
    expectedAttrs <- sub('^\\.', '', expectedAttrs)

    res <- lcut(m[1, , drop=FALSE],
                context=ctx3(0, 0.5, 1),
                hedges=testedHedges)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 44)
    expect_equal(nrow(res), 1)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), c(rep('a', 22), rep('b', 22)))

    s <- matrix(c(0,1,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #SiSm
                  0,0,0,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VeSm
                  0,0,0,0,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #Sm
                  0,0,0,0,0,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #MlSm
                  0,0,0,0,0,0,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,0,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0,0,0, 0,1,1,1,1,1, 0,0,0,0,0,0,0,0,  #TyMe
                  0,0,0,0,0,0,0,0, 0,0,1,1,1,1, 0,0,0,0,0,0,0,0,  #Me
                  0,0,0,0,0,0,0,0, 0,0,0,1,1,1, 0,0,0,0,0,0,0,0,  #MlMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,1,1, 0,0,0,0,0,0,0,0,  #RoMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,1, 0,0,0,0,0,0,0,0,  #QrMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrMe

                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,1,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,1,1,1,1,1,1,  #SiBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,1,1,1,1,1,  #VeBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0), #VrBi
                byrow=TRUE,
                nrow=22,
                ncol=22)
    sfill <- matrix(0, nrow=22, ncol=22)
    s <- cbind(rbind(s, sfill), rbind(sfill, s))

    expect_equal(specs(res), s)
})


test_that('lcut on empty row data frame', {
    testedHedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr", "ty")
    testedAtomic <- c("sm", "me", "bi")

    smHedgeNames <- c("ex", "si", "ve", "", "ml", "ro", "qr", "vr")
    meHedgeNames <- c("ty", "", "ml", "ro", "qr", "vr")
    biHedgeNames <- smHedgeNames

    expectedAttrs <- c(paste(smHedgeNames, 'sm', 'a', sep='.'),
                       paste(meHedgeNames, 'me', 'a', sep='.'),
                       paste(biHedgeNames, 'bi', 'a', sep='.'),
                       paste(smHedgeNames, 'sm', 'b', sep='.'),
                       paste(meHedgeNames, 'me', 'b', sep='.'),
                       paste(biHedgeNames, 'bi', 'b', sep='.'))
    expectedAttrs <- sub('^\\.', '', expectedAttrs)

    res <- lcut(m[FALSE, , drop=FALSE],
                context=ctx3(0, 0.5, 1),
                hedges=testedHedges)

    expect_true(is.fsets(res))
    expect_equal(ncol(res), 44)
    expect_equal(nrow(res), 0)
    expect_equal(colnames(res), expectedAttrs)
    expect_equal(vars(res), c(rep('a', 22), rep('b', 22)))

    s <- matrix(c(0,1,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #SiSm
                  0,0,0,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VeSm
                  0,0,0,0,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #Sm
                  0,0,0,0,0,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #MlSm
                  0,0,0,0,0,0,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,0,0,1, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0,0,0, 0,1,1,1,1,1, 0,0,0,0,0,0,0,0,  #TyMe
                  0,0,0,0,0,0,0,0, 0,0,1,1,1,1, 0,0,0,0,0,0,0,0,  #Me
                  0,0,0,0,0,0,0,0, 0,0,0,1,1,1, 0,0,0,0,0,0,0,0,  #MlMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,1,1, 0,0,0,0,0,0,0,0,  #RoMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,1, 0,0,0,0,0,0,0,0,  #QrMe
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  #VrMe

                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,1,1,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,1,1,1,1,1,1,  #SiBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,1,1,1,1,1,  #VeBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0,0), #VrBi
                byrow=TRUE,
                nrow=22,
                ncol=22)
    sfill <- matrix(0, nrow=22, ncol=22)
    s <- cbind(rbind(s, sfill), rbind(sfill, s))

    expect_equal(specs(res), s)
})


test_that('lcut empty data.frame', {
    testedHedges <- c("ex", "ml", "ro", "qr", "vr")
    hedgeNames   <- c("Ex", "Ml", "Ro", "Qr", "Vr")

    smHedgeNames <- c("Ex", "", "Ml", "Ro", "Qr", "Vr")
    biHedgeNames <- smHedgeNames

    attrs <- c(paste(smHedgeNames, 'Sm', sep=''),
               paste(biHedgeNames, 'Bi', sep=''))

    d <- d[FALSE, , drop=FALSE]
    res <- lcut3(d,
                 atomic=c('sm', 'bi'),
                 context=c(0, 0.5, 1),
                 hedges=testedHedges)

    expectedAttrs <- c(paste(attrs, '.a', sep=''),
                       paste(attrs, '.b', sep=''))
    expect_true(is.matrix(res))
    expect_equal(ncol(res), 24)
    expect_equal(nrow(res), 0)
    expect_equal(sort(colnames(res)), sort(expectedAttrs))
    expect_true(inherits(res, 'fsets'))
    expect_equivalent(vars(res), c(rep('a', 12), rep('b', 12)))
    #expect_equal(sort(names(vars(res))), sort(expectedAttrs))
    #expect_equal(sort(colnames(specs(res))), sort(expectedAttrs))
    #expect_equal(sort(rownames(specs(res))), sort(expectedAttrs))

    s <- matrix(c(0,1,1,1,1,1, 0,0,0,0,0,0,  #ExSm
                  0,0,1,1,1,1, 0,0,0,0,0,0,  #Sm
                  0,0,0,1,1,1, 0,0,0,0,0,0,  #MlSm
                  0,0,0,0,1,1, 0,0,0,0,0,0,  #RoSm
                  0,0,0,0,0,1, 0,0,0,0,0,0,  #QrSm
                  0,0,0,0,0,0, 0,0,0,0,0,0,  #VrSm

                  0,0,0,0,0,0, 0,1,1,1,1,1,  #ExBi
                  0,0,0,0,0,0, 0,0,1,1,1,1,  #Bi
                  0,0,0,0,0,0, 0,0,0,1,1,1,  #MlBi
                  0,0,0,0,0,0, 0,0,0,0,1,1,  #RoBi
                  0,0,0,0,0,0, 0,0,0,0,0,1,  #QrBi
                  0,0,0,0,0,0, 0,0,0,0,0,0), #VrBi
                byrow=TRUE,
                nrow=12,
                ncol=12)
    sfill <- matrix(0, nrow=12, ncol=12)
    s <- cbind(rbind(s, sfill), rbind(sfill, s))

    mapper <- seq_along(expectedAttrs)
    names(mapper) <- expectedAttrs
    #s <- s[colnames(res), colnames(res)]
    s <- s[mapper[colnames(res)], mapper[colnames(res)]]

    expect_equal(specs(res), s)
    expect_true(is.fsets(res))
})
