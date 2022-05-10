.ft.weights <- function(xmemb, antecedents) {
    res <- apply(xmemb, 1, function(row) {
        fire <- sapply(seq_len(ncol(antecedents)),
                       function(i) row[antecedents[, i], drop = FALSE])
        apply(fire, 1, prod)
    })

    dimnames(res) <- NULL

    res
}


#' Fuzzy transform
#'
#' @param x the numeric matrix of input values
#' @param xmemb the partitioning of input values, i.e., a `fsets` object with membership degrees
#'     (see [fcut()])
#' @param y the numeric vector of target values
#' @param order the order of the fuzzy transform (0, 1, 2, ...)
#' @return the instance of the S3 class `ft`
#' @author Michal Burda
#' @examples
#'
#' # create the fuzzy transform object
#' y <- (1:30)^2
#' x <- as.matrix(data.frame(a = 1:30, b = 30:1))
#' xmemb <- fcut(x,
#'               breaks = list(a = equidist(x[, 'a'], 3),
#'                             b = equidist(x[, 'b'], 3)))
#' fit <- ft(x, xmemb, y, order = 1)
#'
#' # obtain function values
#' x2 <- as.matrix(data.frame(a = 10:20, b = 20:10))
#' xmemb2 <- fcut(x,
#'                breaks = list(a = equidist(x[, 'a'], 3),
#'                              b = equidist(x[, 'b'], 3)))
#' y2 <- predict(fit, x2, xmemb2)
#' print(y2)
#'
#' # compare original values with those obtained by the fuzzy transform
#' y - y2
#'
#' @export
ft <- function(x,
               xmemb,
               y,
               order = 1) {
    .mustBeNumericMatrix(x)
    .mustBe(ncol(x) >= 1, "'x' must have at least 1 column")
    .mustBe(nrow(x) >= 1, "'x' must not be empty")

    .mustBe(is.fsets(xmemb), "'xmemb' must be an instance of class 'fsets'")
    .mustBe(nrow(x) == nrow(xmemb), "the number of rows in 'x' must equal to the number of rows in 'xmemb")
    .mustBe(all(colnames(x) %in% vars(xmemb)), "colnames(x) must be equal to vars(xmemb)")
    .mustBe(all(vars(xmemb) %in% colnames(x)), "colnames(x) must be equal to vars(xmemb)")

    .mustBeNumericVector(y)
    .mustBeNumericScalar(order)
    .mustBe(order >= 0, "'order' must be greater or equal to 0")
    .mustBe(nrow(x) == length(y), "the number of rows in 'x' must be equal to the length of vector 'y'")

    inputs <- colnames(x)
    partitions <- lapply(inputs, function(var) colnames(xmemb)[vars(xmemb) == var])

    antecedents <- do.call(expand.grid, c(partitions, list(KEEP.OUT.ATTRS = FALSE)))
    antecedents <- as.matrix(antecedents)
    dimnames(antecedents) <- NULL

    weights <- .ft.weights(xmemb, antecedents)

    d <- as.data.frame(x)
    colnames(d) <- paste0('c', seq_along(d))
    lmvars <- colnames(d)
    d$y <- y

    form <- 'y ~ 1'
    for (o in seq_len(order)) {
        form <- paste0(form,  ' + ', paste0('I(', lmvars, '^', o, ')', collapse = ' + '))
    }

    .fit_lm <- function(ante) {
        w <- weights[ante, ]
        form <- as.formula(form)
        fit <- lm(formula = form, data = d, weights = w)
        as.vector(coef(fit))
    }

    consequents <- sapply(seq_len(nrow(weights)), .fit_lm)
    consequents[is.na(consequents)] <- 0    # this occurs if an input is linear combination of other inputs


    structure(list(inputs = inputs,
                   partitions = partitions,
                   order = order,
                   antecedents = antecedents,
                   consequents = consequents),
              class = 'ft')
}


#' @export
is.ft <- function(x) {
    inherits(x, 'ft') &&
        is.list(x)
}


#' @export
predict.ft <- function(fit, x, xmemb, ...) {
    .mustBe(is.ft(fit), "'fit' must be an instance of the S3 'ft' class")

    .mustBe(is.matrix(x))
    .mustBe(ncol(x) >= 1, "'x' must have at least 1 column")
    .mustBe(nrow(x) >= 1, "'x' must not be empty")

    .mustBe(is.fsets(xmemb), "'xmemb' must be an instance of class 'fsets'")
    .mustBe(nrow(x) == nrow(xmemb), "the number of rows in 'x' must equal to the number of rows in 'xmemb")
    .mustBe(all(colnames(x) %in% vars(xmemb)), "colnames(x) must be equal to vars(xmemb)")
    .mustBe(all(vars(xmemb) %in% colnames(x)), "colnames(x) must be equal to vars(xmemb)")


    weights <- .ft.weights(xmemb, fit$antecedents)
    inputs <- 1
    for (o in seq_len(fit$order)) {
        inputs <- cbind(inputs, x^o)
    }

    res <- inputs %*% fit$consequents
    res <- sapply(seq_len(nrow(res)), function(i) {
        w <- weights[, i]
        sum(res[i, ] * w) / sum(w)
    })

    res
}
