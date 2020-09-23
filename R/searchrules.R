#' Searching for fuzzy association rules
#'
#' This function searches the given [fsets()] object `d` for all
#' fuzzy association rules that satisfy defined constraints. It returns a list
#' of fuzzy association rules together with some statistics characterizing them
#' (such as support, confidence etc.).
#'
#' The function searches data frame `d` for fuzzy association rules that
#' satisfy conditions specified by the parameters.
#'
#' @param d An object of class [fsets()] - it is basically a matrix
#' where columns represent the fuzzy sets and values are the membership
#' degrees. For creation of such object, use [fcut()] or [lcut()] function.
#' @param lhs Indices of fuzzy attributes that may appear on the left-hand-side
#' (LHS) of association rules, i.e. in the antecedent.
#' @param rhs Indices of fuzzy attributes that may appear on the
#' right-hand-side (RHS) of association rules, i.e. in the consequent.
#' @param tnorm A t-norm to be used for computation of conjunction of fuzzy
#' attributes. (Allowed are even only starting letters of "lukasiewicz",
#' "goedel" and "goguen").
#' @param n The non-negative number of rules to be found. If zero, the function
#' returns all rules satisfying the given conditions. If positive, only
#' `n` best rules are returned. The criterium of what is ``best'' is
#' specified with the `best` argument.
#' @param best Specifies measure accordingly to which the rules are ordered
#' from best to worst. This argument is used mainly in combination with the
#' `n` argument. Currently, only single value ("confidence") can be used.
#' @param minSupport The minimum support degree of a rule. Rules with support
#' below that number are filtered out. It must be a numeric value from interval
#' \eqn{[0, 1]}. See below for details on how the support degree is computed.
#' @param minConfidence The minimum confidence degree of a rule. Rules with
#' confidence below that number are filtered out.  It must be a numeric value
#' from interval \eqn{[0, 1]}. See below for details on how the confidence degree is
#' computed.
#' @param maxConfidence Maximum confidence threshold. After finding a rule that
#' has confidence degree above the `maxConfidence` threshold, no other
#' rule is resulted based on adding some additional attribute to its antecedent
#' part. I.e. if "Sm.age & Me.age => Sm.height" has confidence above
#' `maxConfidence` threshold, no another rule containing "Sm.age & Me.age"
#' will be produced regardless of its interest measures.
#'
#' If you want to disable this feature, set `maxConfidence` to 1.
#' @param maxLength Maximum allowed length of the rule, i.e. maximum
#' number of predicates that are allowed on the left-hand + right-hand side of the rule. If
#' negative, the maximum length of rules is unlimited.
#' @param numThreads Number of threads used to perform the algorithm in
#' parallel. If greater than 1, the OpenMP library (not to be confused with
#' Open MPI) is used for parallelization.  Please note that there are known
#' problems of using OpenMP together with another means of parallelization that
#' may be used within R. Therefore, if you plan to use the `searchrules`
#' function with some of the external parallelization mechanisms such as
#' library `doMC`, make sure that `numThreads` equals 1.  This
#' feature is available only on systems that have installed the OpenMP library.
#' @param trie Whether or not to use internal mechanism of Tries. If FALSE,
#' then in the output may appear such rule that is a descendant of a rule that
#' has confidence above `maxConfidence` threshold.
#'
#' Tries consume very much memory, so if you encounter problems with
#' insufficient memory, set this argument to FALSE. On the other hand, the size
#' of result (if `n` is set to 0) can be very high if trie is set to
#' FALSE.
#' @return A list of the following elements: `rules` and `statistics`.
#'
#' `rules` is a list of mined fuzzy association rules. Each element of
#' that list is a character vector with consequent attribute being on the first
#' position.
#'
#' `statistics` is a data frame of statistical characteristics about mined
#' rules. Each row corresponds to a rule in the `rules` list. Let us
#' consider a rule "a & b => c", let \eqn{\otimes} be a t-norm specified with
#' the `tnorm` parameter and \eqn{i} goes over all rows of a data table
#' `d`. Then columns of the `statistics` data frame are as follows:
#' * support: a rule's support degree: \eqn{1/nrow(d) * \sum_{\forall i} a(i) \otimes b(i) \otimes c(i)}
#' * lhsSupport: a support of rule's antecedent (LHS): \eqn{1/nrow(d) * \sum_{\forall i} a(i) \otimes b(i)}
#' * rhsSupport: a support of rule's consequent (RHS): \eqn{1/nrow(d) * \sum_{\forall i} c(i)}
#' * confidence: a rule's confidence degree: \eqn{support / lhsSupport}
#' @author Michal Burda
#' @seealso [fcut()], [lcut()], [farules()], [fsets()], [pbld()]
#' @keywords models robust multivariate
#' @examples
#'
#'   d <- lcut(CO2)
#'   searchrules(d, lhs=1:ncol(d), rhs=1:ncol(d))
#'
#' @export searchrules
searchrules <- function(d,
                        lhs=2:ncol(d),
                        rhs=1,
                        tnorm=c("goedel", "goguen", "lukasiewicz"),
                        n=100,
                        best=c("confidence"),
                        minSupport=0.02,
                        minConfidence=0.75,
                        maxConfidence=1,
                        maxLength=4,
                        numThreads=1,
                        trie=(maxConfidence < 1)) {

    .mustBe(is.fsets(d), "'d' must be an instance of class 'fsets'")
    .mustBe(ncol(d) >= 2, "'d' must have at least 2 columns")
    .mustBe(nrow(d) >= 1, "'d' must not be empty")
    .mustBeNumericVector(lhs)
    .mustBe(min(lhs) >= 1 && max(lhs) <= ncol(d), "'lhs' must contain valid indexes of the columns of 'd'")
    .mustBeNumericVector(rhs)
    .mustBe(min(rhs) >= 1 && max(rhs) <= ncol(d), "'rhs' must contain valid indexes of the columns of 'd'")
    .mustBeNumericScalar(n)
    .mustBe(n >= 0, "'n' must be a non-negative number (zero means unlimited number of rules)")
    .mustBeNumericScalar(minSupport)
    .mustBeInInterval(minSupport, 0, 1)
    .mustBeNumericScalar(minConfidence)
    .mustBeInInterval(minConfidence, 0, 1)
    .mustBeNumericScalar(maxConfidence)
    .mustBeInInterval(maxConfidence, 0, 1)
    .mustBe(maxConfidence >= minConfidence, "'maxConfidence' must be greater or equal to 'minConfidence'")

    if (maxLength < 0) {
        maxLength <- -1
    }

    .mustBeNumericScalar(numThreads)
    .mustBe(numThreads >= 1, "'numThreads' must be positive integer number")

    tnorm <- match.arg(tnorm)
    if (tnorm == 'goedel') {
        tnorm <- 'minimum'
    } else if (tnorm == 'goguen') {
        tnorm <- 'product'
    }

    best = match.arg(best)

    config <- list(vars=as.numeric(as.factor(vars(d))),
                   minSupport=minSupport,
                   minConfidence=minConfidence,
                   maxConfidence=maxConfidence,
                   maxLength=maxLength,
                   lhs=lhs - 1,
                   rhs=rhs - 1,
                   tnorm=tnorm,
                   n=n,
                   best=best,
                   numThreads=numThreads,
                   trie=trie)

    result <- .Call("_lfl_search", d, config, PACKAGE="lfl")

    map <- colnames(d)
    names(map) <- 1:ncol(d)

    rules <- lapply(result$rules, function(x) {
                        x <- x + 1
                        x[] <- map[unlist(x)]
                        return(x)
                    })
    if (is.null(result$statistics) || length(result$statistics) <= 0) {
        stats <- matrix(0, nrow=0, ncol=0)
    } else {
        stats <-matrix(unlist(result$statistics),
                       byrow=TRUE,
                       nrow=length(result$statistics))
        colnames(stats) <- c('support', 'lhsSupport', 'rhsSupport', 'confidence', 'lift', 'loLift', 'hiLift')

        stats <- stats[, c('support', 'lhsSupport', 'rhsSupport', 'confidence'), drop=FALSE]

        if (tnorm == 'product') {
            stats <- cbind(stats,
                           lift=stats[, 'confidence'] / stats[, 'rhsSupport'],
                           conviction=(1 - stats[, 'rhsSupport']) / (1 - stats[, 'confidence']))
        }
    }
    res <- farules(rules=rules,
                   statistics=stats)
    return(res)
}

