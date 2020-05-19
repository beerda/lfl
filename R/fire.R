#' Evaluate rules and obtain truth-degrees
#'
#' Given truth degrees of predicates, compute the truth value of given list of rules.
#'
#' The aim of this function is to compute the truth value of each rule in a
#' `rules` list by assigning truth values to rule's predicates given by data `x`.
#'
#' `x` is a numeric vector or numeric matrix of truth values of predicates. If
#' `x` is vector then `names(x)` must correspond to the predicate names
#' in `rules`. If `x` is a matrix then each column should represent a predicate
#' and thus `colnames(x)` must correspond to predicate names in `rules`.
#' Values of `x` are interpreted as truth values, i.e., they must be from the
#' interval \eqn{[0, 1]}. If matrix is given, the resulting truth values are
#' computed row-wisely.
#'
#' `rules` may be a list of character vectors or an instance of the S3 class
#' [farules()]. The character vectors in the `rules` list represent formulae
#' in conjunctive form. If `onlyAnte=FALSE`, `fire()` treats the rule as
#' a conjunction of all predicates, i.e., a conjunction of all predicates is
#' computed. If `onlyAnte=TRUE`, the first element of each rule is removed
#' prior evaluation, i.e., a conjunction of all predicates except the first
#' are computed: this is useful if `rules` is a [farules()] object, since
#' [farules()] objects save a rule's consequent as the first element (see also
#' [antecedents()] and [consequents()] functions).
#'
#' The type of  conjunction to be computed can be specified with the `tnorm` parameter.
#'
#' @param x Truth degrees of predicates. `x` could be either a numeric
#' matrix or a numeric vector. If vector is given then each named element represents
#' a truth value of a predicate. If matrix is given then each row of the matrix
#' is evaluated sequentially as a vector. The values must be in the interval
#' \eqn{[0, 1]}.
#' @param rules Either an object of S3 class [farules()] or a list of character
#' vectors where each vector is a rule in a conjunctive form. Elements of these
#' character vectors (i.e., predicate names) must correspond to
#' the `x`'s names (of elements resp. columns if `x` is a vector resp. matrix).
#' @param tnorm A character string representing a triangular norm to be used
#' (either `"goedel"`, `"goguen"`, or `"lukasiewicz"`) or an arbitrary function
#' that performs element-wise computation on arbitrary number of vector parameters
#' similarly as e.g. [pgoedel.tnorm()], [pgoguen.tnorm()] or [plukas.tnorm()].
#' @param onlyAnte `TRUE` is useful if rules store both the antecedent and consequent
#' and if only the antecedent-part of a rule should be included into the evaluated
#' conjunction. Antecedent-part of a rule are all predicates in the vector starting from
#' the 2nd position. `TRUE` value in this parameter causes the first element of each
#' rule to be ignored.
#'
#' If `FALSE`, all predicates in a rule will be included in the conjunction.
#' @param parallel Deprecated parameter. Computation is done sequentially.
#' @return If `x` is a matrix then the result of this function is a list
#' of numeric vectors with truth values of each rule, i.e., each element of the
#' resulting list corresponds to a rule and each value of the vector in the resulting
#' list corresponds to a row of the original data matrix `x`.
#'
#' `x` as a vector is treated as a single-row matrix.
#' @author Michal Burda
#' @seealso [aggregateConsequents()], [defuzz()], [perceive()], [pbld()], [fcut()], [lcut()], [farules()]
#' @keywords models robust multivariate
#' @examples
#'
#' # fire whole rules on a vector
#' x <- 1:10 / 10
#' names(x) <- letters[1:10]
#' rules <- list(c('a', 'c', 'e'),
#'               c('b'),
#'               c('d', 'a'),
#'               c('c', 'a', 'b'))
#' fire(x, rules, tnorm='goguen', onlyAnte=FALSE)
#'
#' # fire antecedents of the rules on a matrix
#' x <- matrix(1:20 / 20, nrow=2)
#' colnames(x) <- letters[1:10]
#' rules <- list(c('a', 'c', 'e'),
#'               c('b'),
#'               c('d', 'a'),
#'               c('c', 'a', 'b'))
#' fire(x, rules, tnorm='goedel', onlyAnte=TRUE)
#'
#' # the former command should be equal to
#' fire(x, antecedents(rules), tnorm='goedel', onlyAnte=FALSE)
#'
#' @export
fire <- function(x,
                 rules,
                 tnorm=c("goedel", "goguen", "lukasiewicz"),
                 onlyAnte=TRUE,
                 parallel=FALSE) {
    if (parallel) {
        warning('"parallel=TRUE" is deprecated. Computing sequentially.')
    }
    if (is.vector(x)) {
        x <- matrix(x, nrow=1, dimnames=list(NULL, names(x)))
    }
    .mustBe(is.matrix(x), "'x' must be a vector or matrix")

    if (is.farules(rules)) {
        rules <- rules$rules
    } else if (is.vector(rules) && is.character(rules)) {
        rules <- list(rules)
    }

    .mustBe(is.list(rules), "'rules' must be a list of rules, a valid farules object, or a character vector")

    if (!is.function(tnorm)) {
        tnorm <- match.arg(tnorm)
        alg <- algebra(tnorm)
        tnorm <- alg$pt
    }

    if (onlyAnte) {
        if (any(lengths(rules) <= 0)) {
            .stop("Cannot extract antecedent from an empty rule (onlyAnte is set to TRUE)")
        }
        rules <- antecedents(rules)
    }

    unknown <- unique(unlist(rules))
    unknown <- setdiff(unknown, colnames(x))
    if (length(unknown) > 0) {
        .stop(paste0("Unknown predicates in rules: ", paste(unknown, collapse=', ')))
    }

    xx <- as.matrix(x)
    xx <- split(xx, c(col(xx)))  # convert matrix to a list of columns
    names(xx) <- colnames(x)
    res <- lapply(rules, function(rule) {
        if (length(rule) <= 0) {
            return(rep(1, nrow(x)))
        } else {
            return(do.call(tnorm, xx[rule]))
        }
    })

    return(res)
}
