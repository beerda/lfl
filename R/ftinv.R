#' Inverse of the fuzzy transform
#'
#'
#' Compute an inverse of fuzzy transform `fit` for values `x` with corresponding
#' membership degrees `xmemb`.
#'
#' @param fit The fuzzy transform object as the instance of the `ft` S3 class
#' @param x The numeric matrix of input values, for which the inverse fuzzy transform
#'     has to be computed
#' @param xmemb the partitioning of input values, i.e., a `fsets` object with membership degrees
#'     (see [fcut()]). Such partitioning must correspond to the `xmemb` partitioning used to
#'     create `fit` using the [ft()] function.
#' @return The inverse of the fuzzy transform `fit`, i.e., the approximated values
#'     of the original function that was the subject of the fuzzy transform
#' @author Michal Burda
#' @seealso [ft()], [is.ft()]
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
#' @importFrom stats weighted.mean
ftinv <- function(fit, x, xmemb) {
    .mustBe(is.ft(fit), "'fit' must be an instance of the S3 'ft' class")

    .mustBe(is.matrix(x))
    .mustBe(ncol(x) >= 1, "'x' must have at least 1 column")
    .mustBe(nrow(x) >= 1, "'x' must not be empty")

    .mustBe(is.fsets(xmemb), "'xmemb' must be an instance of class 'fsets'")
    .mustBe(nrow(x) == nrow(xmemb), "the number of rows in 'x' must equal to the number of rows in 'xmemb")
    .mustBe(all(colnames(x) %in% vars(xmemb)), "colnames(x) must be equal to vars(xmemb)")
    .mustBe(all(vars(xmemb) %in% colnames(x)), "colnames(x) must be equal to vars(xmemb)")


    weights <- .ft.weights(xmemb, fit$antecedents)
    inputs <- rep(1, nrow(x))
    for (o in seq_len(fit$order)) {
        inputs <- cbind(inputs, x^o)
    }

    res <- inputs %*% fit$consequents

    sapply(seq_len(nrow(res)), function(i) weighted.mean(res[i, ], weights[, i]))
}
