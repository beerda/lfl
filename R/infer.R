#######################################################################
# lfl: Linguistic Fuzzy Logic
# Copyright (C) 2026 Michal Burda
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.
#######################################################################


#' Perform Mamdani or implicative fuzzy inference with given rule-base on given
#' dataset
#'
#' Take a set of rules (a rule-base) and perform Mamdani or implicative
#' inference on each row of a given matrix (or the [fsets()] object) of truth
#'  values.
#'
#' Perform Mamdani or implicative fuzzy inference with given rule-base `rules`
#' on each row of input `x`. Columns of `x` are truth values of predicates that
#' appear in the antecedent part of `rules`. The `partition` matrix determines
#' the shape of predicates in consequents: each row represents a point in the
#' output fuzzy set for which membership degrees are computed.
#'
#' For both inference types, the predicates in the antecedent part of rules are
#' combined using a t-norm (`alg$pt`) of the given algebra `alg` (see also the
#' `fire()` function).
#'
#' For the Mamdani inference (`type = "mamdani"`), the antecedent firing
#' degrees are combined with the consequent partition using a t-norm (`alg$pt`)
#' of the selected algebra `alg`, and the resulting fuzzy sets are aggregated
#' with a t-conorm (`alg$ps`).
#'
#' For the implicative inference (`type = "implicative"`), the antecedent
#' firing degrees are combined with the consequent partition using a residuum
#' (`alg$r`) of the selected algebra `alg`, and the resulting fuzzy sets are
#' aggregated with a t-norm (`alg$pi`).
#'
#' @param x Input to the inference. It must be a numeric matrix with columns
#' representing fuzzy sets (truth values of predicates). Each row represents a
#' single case of inference. Columns should be named after predicates in rules'
#' antecedents. The values must be in the interval \eqn{[0, 1]}. `x` can also be
#' an object of class [fsets()] (e.g., created by using the [fcut()] or [lcut()]
#' functions).
#' @param rules A rule-base (a.k.a. linguistic description) either in the form
#' of the [farules()] object or as a list of character vectors where
#' each element is a fuzzy set name (a predicate) and thus each such vector
#' forms a rule. The first element of each rule is the consequent predicate,
#' and the remaining elements are antecedent predicates.
#' @param partition A numeric matrix (or a [fsets()] object) with columns that
#' are consequents in `rules`. The column names of `partition` must correspond
#' to consequent predicate names used in `rules`. Each row represents a point in the
#' output domain.
#' @param alg The algebra to use for the inference. It can be either a character
#' string (`"goedel"`, `"goguen"`, or `"lukasiewicz"`) or an instance of the
#' [algebra()] class.
#' @param type The type of inference to use. It can be either `"mamdani"` or
#' `"implicative"`.
#' @param parallel Whether the processing should be run in parallel or not.
#' Parallelization is implemented using the [foreach::foreach()]
#' package. The parallel environment must be set properly in advance, e.g., with
#' the [doMC::registerDoMC()] function.
#' @return A numeric matrix of inferred membership degrees. The rows of the
#' result correspond to rows of the `x` argument, the columns of the result
#' correspond to rows of the `partition` argument.
#' @author Michal Burda
#' @seealso [pbld()], [fire()], [aggregateConsequents()], [defuzz()], [algebra()], [farules()], [lcut()]
#' @keywords models robust
#' @examples
#' # Create rules: first element is the consequent, the rest are antecedents
#' rules <- list(
#'     c("High",   "temp_high",   "pressure_high"),
#'     c("Medium", "temp_medium", "pressure_medium"),
#'     c("Low",    "temp_low")
#' )
#'
#' # Create input data (fuzzy set membership degrees)
#' # (Alternatively, fcut() or lcut() functions can be used)
#' x <- matrix(
#'     c(0.9, 0.8, 0.2, 0.3, 0.1,
#'       0.1, 0.2, 0.4, 0.5, 0.8),
#'     nrow = 2,
#'     byrow = TRUE
#' )
#' colnames(x) <- c("temp_high", "pressure_high",
#'                   "temp_medium", "pressure_medium", "temp_low")
#'
#' # Create output partition
#' # (Alternatively, fcut() or lcut() functions can be used)
#' partition <- matrix(
#'     c(1.0, 0.2, 0.0,
#'       0.8, 0.5, 0.1,
#'       0.4, 1.0, 0.4,
#'       0.1, 0.5, 0.8,
#'       0.0, 0.1, 1.0),
#'     nrow = 5,
#'     byrow = TRUE
#' )
#' colnames(partition) <- c("Low", "Medium", "High")
#'
#' # Mamdani inference with Goedel algebra
#' infer(x, rules, partition, alg = "goedel", type = "mamdani")
#'
#' # Implicative inference with Lukasiewicz algebra
#' infer(x, rules, partition, alg = "lukasiewicz", type = "implicative")
#'
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
