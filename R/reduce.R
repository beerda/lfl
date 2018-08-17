#' Reduce the size of rule base
#'
#' From given rule base, select such set of rules that influence mostly the
#' rule base coverage of the input data.
#'
#' From a given rulebase, a rule with greatest coverage is selected. After
#' that, additional rules are selected that increase the rule base coverage the
#' most. Addition stops after the coverage exceeds \eqn{original coverage *
#' ratio}.
#'
#' Note that the size of the resulting rule base is not necessarily minimal
#' because the algorithm does not search all possible combination of rules. It
#' only finds a local minimum of rule base size.
#'
#' @param x Data for the rules to be evaluated on. Could be either a numeric
#' matrix or numeric vector.  If matrix is given then the rules are evaluated
#' on rows. Each value of the vector or column of the matrix represents a
#' predicate - it's numeric value represents the truth values (values in the
#' interval \eqn{[0, 1]}).
#' @param rules Either an object of class "farules" or list of character
#' vectors where each vector is a rule with consequent being the first element
#' of the vector. Elements of the vectors (predicate names) must correspond to
#' the `x`'s names (of columns if `x` is a matrix).
#' @param ratio A percentage of rule base coverage that must be preserved. It
#' must be a value within the \eqn{[0, 1]} interval. Value of 1 means that the
#' rule base coverage of the result must be the same as coverage of input
#' \code{rules}. A sensible value is e.g. 0.9.
#' @param tnorm Which t-norm to use as a conjunction of antecedents. The
#' default is `"goedel"`.
#' @param tconorm Which t-norm to use as a disjunction, i.e. to combine
#' multiple antecedents to get coverage of the rule base. The default is
#' `"goedel"`.
#' @param numThreads How many threads to use for computation. Value higher than
#' 1 causes that the algorithm runs in several parallel threads (using the
#' OpenMP library).
#' @return Function returns an instance of class [farules()] or a
#' list depending on the type of the `rules` argument.
#' @author Michal Burda
#' @seealso [rbcoverage()], [farules()]
#' @references M. Burda, M. Štěpnička, Reduction of Fuzzy Rule Bases Driven by
#' the Coverage of Training Data, in: Proc. 16th World Congress of the
#' International Fuzzy Systems Association and 9th Conference of the European
#' Society for Fuzzy Logic and Technology (IFSA-EUSFLAT 2015), Advances in
#' Intelligent Systems Research, Atlantic Press, Gijon, 2015.
#' @keywords models robust multivariate
#' @export
reduce <- function(x,
                   rules,
                   ratio,
                   tnorm=c("goedel", "goguen", "lukasiewicz"),
                   tconorm=c("goedel", "goguen", "lukasiewicz"),
                   numThreads=1) {
    if (is.vector(x)) {
        x <- matrix(x, nrow=1, dimnames=list(NULL, names(x)))
    }
    .mustBe(is.matrix(x) && is.numeric(x), "'x' must be a numeric vector or matrix")
    .mustBeInInterval(x, 0, 1)

    origRules <- rules
    if (is.farules(origRules)) {
        rules <- origRules$rules
    } else if (is.vector(rules) && is.character(rules)) {
        rules <- list(rules)
    }
    .mustBe(is.list(rules), "'rules' must be a list of rules, a valid farules object, or a character vector")

    .mustBeNumericScalar(numThreads)
    .mustBe(numThreads >= 1, "'numThreads' must be positive integer number")

    tnorm <- match.arg(tnorm)
    if (tnorm == 'goedel') {
        tnorm <- 'minimum'
    } else if (tnorm == 'goguen') {
        tnorm <- 'product'
    }

    tconorm <- match.arg(tconorm)
    if (tconorm == 'goedel') {
        tconorm <- 'maximum'
    } else if (tconorm == 'goguen') {
        tconorm <- 'product'
    }

    lhsSupport <- NA
    if (is.farules(origRules) && ('lhsSupport' %in% colnames(origRules$statistics))) {
        lhsSupport <- origRules$statistics[, 'lhsSupport']
    } else {
        lhsSupport <- rep(1, length(rules))
    }

    lvl <- colnames(x)
    rb <- lapply(rules, function(x) { as.integer(factor(x, levels=lvl)) - 1 })
    config <- list(data=x,
                   rules=rb,
                   lhsSupport=lhsSupport,
                   ratio=ratio,
                   tnorm=tnorm,
                   tconorm=tconorm,
                   numThreads=numThreads)
    result <- .Call("_lfl_reduce", config, PACKAGE="lfl")
    result <- result + 1

    if (is.farules(origRules)) {
        r <- origRules$rules[result]
        s <- origRules$statistics[result, , drop=FALSE]
        return(farules(rules=r, statistics=s))
    } else {
        return(rules[result])
    }
}
