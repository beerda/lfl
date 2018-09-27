.defuzzEmptyRulebase <- function(values) {
    return(defuzz(rep(1, length(values)),
                    values,
                    type='dee'))
}




#' Perform a Perception-based Logical Deduction (PbLD) with given rule-base on
#' given dataset
#'
#' Take a set of rules (a rule-base) and perform a Perception-based Logical
#' Deduction (PbLD) on each row of a given [fsets()] object.
#'
#' Perform a Perception-based Logical Deduction (PbLD) with given rule-base
#' `rules` on each row of input `x`. Columns of `x` are truth
#' values of predicates that appear in the antecedent part of `rules`,
#' `partition` together with `values` determine the shape of
#' predicates in consequents: each element in `values` corresponds to a
#' row of membership degrees in `partition`.
#'
#' @param x Input to the inference. It should be an object of class
#' [fsets()] (e.g. created by using the [fcut()] or [lcut()] functions).
#' It is basically a matrix with columns representing fuzzy sets.
#'
#' Each row represents a single case of inference. Columns should be named
#' after predicates in rules' antecedents.
#' @param rules A rule-base (a.k.a. linguistic description) either in the form
#' of the [farules()] object or as a list of character vectors where
#' each element is a fuzzy set name (a predicate) and thus each such vector
#' forms a rule.
#' @param partition A [fsets()] object with columns that are
#' consequents in `rules`. These membership degrees must correspond to
#' `values`.
#' @param values Crisp values that correspond to rows of memberhsip degrees in
#' the `partition` matrix.  Function assumes that the values are sorted in
#' the ascending order.
#' @param type The type of inference to use. It can be either `"local"` or
#' `"global"` (default).
#' @param parallel Whether the processing should be run in parallel or not.
#' Parallelization is implemented using the [foreach::foreach()]
#' package. The parallel environment must be set properly in advance, e.g. with
#' the [doMC::registerDoMC()] function.
#' @return A vector of inferred defuzzified values. The number of resulting
#' values corresponds to the number of rows of the `x` argument.
#' @author Michal Burda
#' @seealso [lcut()], [searchrules()], [fire()], [aggregateConsequents()], [defuzz()]
#' @references A. Dvořák, M. Štěpnička, On perception-based logical deduction
#' and its variants, in: Proc. 16th World Congress of the International Fuzzy
#' Systems Association and 9th Conference of the European Society for Fuzzy
#' Logic and Technology (IFSA-EUSFLAT 2015), Advances in Intelligent Systems
#' Research, Atlantic Press, Gijon, 2015.
#' @keywords models robust
#' @examples
#'
#' # --- TRAINING PART ---
#' # custom context of the RHS variable
#' uptakeContext <- ctx3(7, 28.3, 46)
#'
#' # convert data into fuzzy sets
#' d <- lcut(CO2, context=list(uptake=uptakeContext))
#'
#' # split data into the training and testing set
#' testingIndices <- 1:5
#' trainingIndices <- setdiff(seq_len(nrow(CO2)), testingIndices)
#' training <- d[trainingIndices, ]
#' testing <- d[testingIndices, ]
#'
#' # search for rules
#' r <- searchrules(training, lhs=1:38, rhs=39:58, minConfidence=0.5)
#'
#' # --- TESTING PART ---
#' # prepare values and partition
#' v <- seq(uptakeContext[1], uptakeContext[3], length.out=1000)
#' p <- lcut(v, name='uptake', context=uptakeContext)
#'
#' # do the inference
#' pbld(testing, r, p, v)
#'
#' @export pbld
pbld <- function(x,
                 rules,
                 partition,
                 values,
                 type=c('global', 'local'),
                 parallel=FALSE) {
    .mustBe(is.fsets(x), "'x' must be an instance of the 'fsets' class")

    if (is.farules(rules)) {
        rules <- rules$rules
    } else if (is.vector(rules) && is.character(rules)) {
        rules <- list(rules)
    }

    .mustBe(is.list(rules), "'rules' must be a list of rules or an instance of the 'farules' class")
    .mustBeNumericMatrix(partition)
    .mustNotBeZeroLength(partition)
    .mustBe(!is.null(colnames(partition)) && is.character(colnames(partition)), "'partition' must have colnames")
    .mustBeNumericVector(values)
    .mustBe(nrow(partition) == length(values), "The length of 'values' must be equal to the number of rows of 'partition'")

    diffAttr <- setdiff(unique(unlist(antecedents(rules))), colnames(x))
    if (length(diffAttr) > 0) {
        stop(paste0("'rules' contains predicates in antecedent-part that are not present in 'colnames(x)': ",
                    paste(diffAttr, collapse=', ', sep='')))
    }

    type <- match.arg(type)

    if (length(rules) <= 0) {
        return(rep(.defuzzEmptyRulebase(values), nrow(x)))
    }

    n <- colnames(x)
    v <- as.integer(factor(vars(x)))
    specs <- specs(x)

    if (type == 'global') {
        innerLoop <- function(row) {
            fired <- fire(row, rules, tnorm=goedel.tnorm, onlyAnte=TRUE)
            fired <- unlist(fired)
            maxFired <- max(fired)
            most <- which(fired == maxFired)
            selected <- .perceiveGlobal(rules[most], n, v, specs)
            if (!any(selected)) {
                return(.defuzzEmptyRulebase(values))
            }
            conseq <- unlist(consequents(rules[most][selected]))
            degrees <- aggregateConsequents(conseq, rep(maxFired, length(conseq)), partition)
            res <- defuzz(degrees, values, type='dee')
            return(res)
        }
    } else {
        innerLoop <- function(row) {
            fired <- fire(row, rules, tnorm=goedel.tnorm, onlyAnte=TRUE)
            fired <- unlist(fired)
            most <- which(fired > 0)
            selected <- .perceiveLocal(rules[most], n, v, specs, fired[most])
            if (!any(selected)) {
                return(.defuzzEmptyRulebase(values))
            }
            conseq <- unlist(consequents(rules[most][selected]))
            degrees <- aggregateConsequents(conseq, fired[most][selected], partition)
            res <- defuzz(degrees, values, type='dee')
            return(res)
        }
    }

    i <- NULL
    if (parallel) {
        result <- foreach(i=seq_len(nrow(x))) %dopar% { innerLoop(x[i, , drop=FALSE]) }
    } else {
        result <- foreach(i=seq_len(nrow(x))) %do% { innerLoop(x[i, , drop=FALSE]) }
    }

    return(unlist(result))
}
