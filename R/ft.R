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
#' Compute a fuzzy tranform of the given input matrix `x`.
#'
#' @param x the numeric matrix of input values
#' @param xmemb the partitioning of input values, i.e., a `fsets` object with membership degrees
#'     (see [fcut()])
#' @param y the numeric vector of target values
#' @param order the order of the fuzzy transform (0, 1, 2, ...)
#' @return the instance of the S3 class `ft`
#' @author Michal Burda
#' @seealso [ftinv()], [is.ft()]
#' @keywords models robust
#' @references Perfilieva I. Fuzzy transforms: Theory and applications. FUZZY SET SYST,
#'     volume 157, issue 8, p. 993-1023. 2006.
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
#' xmemb2 <- fcut(x2,
#'                breaks = list(a = equidist(x[, 'a'], 3),
#'                              b = equidist(x[, 'b'], 3)))
#' y2 <- ftinv(fit, x2, xmemb2)
#' print(y2)
#'
#' # compare original values with those obtained by the fuzzy transform
#' y - y2
#'
#' @export
#' @importFrom stats coef
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
    if (!is.matrix(consequents)) {
        # order 0 ft
        consequents <- matrix(consequents, nrow=1)
    }


    structure(list(inputs = inputs,
                   partitions = partitions,
                   order = order,
                   antecedents = antecedents,
                   consequents = consequents),
              class = 'ft')
}


#' Test whether `x` is a valid object of the S3 `ft` class
#'
#' Test whether `x` has a valid format for objects of the S3 `ft` class that represents
#' the Fuzzy Transform.
#'
#' This function tests whether `x` is an instance of the `ft` class and whether it is a list
#' with the following elements: `inputs` character vector, `partitions` list, `order` number,
#' `antecedents` matrix and `consequents` matrix.
#'
#' @param x An object to be tested
#' @return `TRUE` if `x` is a valid `ft` object and `FALSE` otherwise.
#' @author Michal Burda
#' @seealso [ft()], [ftinv()]
#' @keywords models robust
#' @export
is.ft <- function(x) {
    inherits(x, 'ft') &&
        is.list(x)
}
