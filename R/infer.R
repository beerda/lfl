#'
#' @return
#' @author Michal Burda
#' @export
infer <- function(x,
                  rules,
                  partition,
                  alg=c('goedel', 'goguen', 'lukasiewicz'),
                  type=c('mamdani', 'implicative'),
                  parallel=FALSE) {
    .mustBe(is.matrix(x), "'x' must be a matrix")

    if (is.farules(rules)) {
        rules <- rules$rules
    } else if (is.vector(rules) && is.character(rules)) {
        rules <- list(rules)
    }

    .mustBe(is.list(rules), "'rules' must be a list of rules or an instance of the 'farules' class")
    .mustBeNumericMatrix(partition)
    .mustNotBeZeroLength(partition)
    .mustBe(!is.null(colnames(partition)) && is.character(colnames(partition)), "'partition' must have colnames")

    diffAttr <- setdiff(unique(unlist(antecedents(rules))), colnames(x))
    if (length(diffAttr) > 0) {
        stop(paste0("'rules' contains predicates in antecedent-part that are not present in 'colnames(x)': ",
                    paste(diffAttr, collapse=', ', sep='')))
    }

    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    type <- match.arg(type)
    emptyResult <- NULL
    innerOp <- NULL
    outerOp <- NULL
    if (type == 'mamdani') {
        emptyResult <- rep(0, nrow(partition))
        innerOp <- alg$pt
        outerOp <- alg$ps
    } else if (type == 'implicative') {
        emptyResult <- rep(1, nrow(partition))
        innerOp <- alg$r
        outerOp <- alg$pi
    } else {
        stop("Unknown inference type")
    }

    if (length(rules) <= 0) {
        return(matrix(rep(emptyResult, nrow(x)), byrow=TRUE, nrow=nrow(x)))
    }

    innerLoop <- function(row) {
        fired <- fire(row, rules, tnorm=alg$pt, onlyAnte=TRUE)
        fired <- unlist(fired)
        if (all(fired == 0)) {
            return(emptyResult)
        }
        conseq <- unlist(consequents(rules))
        res <- aggregateConsequents(conseq,
                                    fired,
                                    partition,
                                    firing = innerOp,
                                    aggreg = outerOp)
        return(res)
    }

    i <- NULL
    if (parallel) {
        result <- foreach(i=seq_len(nrow(x))) %dopar% { innerLoop(x[i, , drop=FALSE]) }
    } else {
        result <- foreach(i=seq_len(nrow(x))) %do% { innerLoop(x[i, , drop=FALSE]) }
    }

    matrix(unlist(result), byrow = TRUE, nrow = nrow(x))
}
