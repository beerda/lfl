.perceiveGlobal <- function(rules, n, v, specs) {
    len <- length(rules)
    if (len <= 1) {
        return(rep(TRUE, len))
    }
    res <- rep(TRUE, len)
    for (i in 1:(len-1)) {
        if (res[i]) {
            ri <- rules[[i]]
            ii <- match(ri[-1], n)
            for (j in (i+1):len) {
                if (res[j]) {
                    rj <- rules[[j]]
                    ji <- match(rj[-1], n)
                    if (.is.specific(ii, ji, v, specs)) {
                        res[j] <- FALSE
                    } else if (.is.specific(ji, ii, v, specs)) {
                        res[i] <- FALSE
                    }
                }
            }
        }
    }
    return(res)
}


.perceiveLocal <- function(rules, n, v, specs, fired) {
    len <- length(rules)
    if (len <= 1) {
        return(rep(TRUE, len))
    }
    res <- rep(TRUE, len)
    for (i in 1:(len-1)) {
        if (res[i]) {
            ri <- rules[[i]]
            ii <- match(ri[-1], n)
            for (j in (i+1):len) {
                if (res[j]) {
                    rj <- rules[[j]]
                    ji <- match(rj[-1], n)
                    rispec <- .is.specific(ii, ji, v, specs)
                    rjspec <- .is.specific(ji, ii, v, specs)
                    if (rispec || rjspec) {
                        if (fired[i] > fired[j]) {
                            res[j] <- FALSE
                        } else if (fired[i] < fired[j]) {
                            res[i] <- FALSE
                        } else {
                            if (rispec) {
                                res[j] <- FALSE
                            } else {
                                res[i] <- FALSE
                            }
                        }
                    }
                }
            }
        }
    }
    return(res)
}


#' From a set of rules, remove each rule for which another rule exists that is
#' more specific.
#'
#' Examine rules in a list and remove all of them for whose other more specific
#' rules are present in the list. The specificity is determined by calling the
#' [is.specific()] function.  This operation is a part of the
#' [pbld()] inference mechanism.
#'
#' In other words, for each rule `x` in the `rules` list, it searches for another
#' rule `y` such that `is.specific(y, x)` returns TRUE. If yes then
#' `x` is removed from the list.
#'
#' @param rules A list of character vectors where each element is a fuzzy set
#' name (a predicate) and thus each such vector forms a rule.
#' @param fsets A valid instance of the [fsets()] class such that all predicates
#' in `rules` (i.e., all values of all character vectors in `rules$rules`)
#' can be found in `colnames(fsets)`
#' @param type The type of perception to use. It can be either `"local"`
#' or `"global"` (default).
#' @param fired If `type=="global"` then this argument can be NULL. If
#' `type` is `"local"` then `fired` must be a numeric vector of
#' values in the interval \eqn{[0,1]} indicating the truth values of all rules,
#' i.e. the length of the vector must be equal to the number of rules in the
#' `rules` argument.
#' @param vars A deprecated parameter that must be `NULL`. Formerly, it was
#' a named (typically character) vector that determined which
#' predicates originate from the same variable, i.e. which of them semantically
#' deal with the same property.  For that purpose, each value from any vector
#' stored in the `rules` list must be present in `names(vars)`.  See
#' also [vars()] function of the [fsets()] class.
#' @param specs A deprecated parameter that must be `NULL`. Formerly, it was
#' a square numeric matrix containing values from \eqn{\{0, 1\}}.
#' It is a specificity matrix for which each row and column corresponds to an
#' `rules`'es predicate `specs[i][j] = 1` if and only if the
#' \eqn{i}-th predicate is more specific (i.e. the corresponding fuzzy set is a
#' subset of) than the \eqn{j}-th predicate (i.e. `x[, j]`).  See also
#' [specs()] function of the [fsets()] class.
#' @return A modified list of rules for which no other more specific rule
#' exists. (Each rule is a vector.)
#' @author Michal Burda
#' @seealso [is.specific()], [fsets()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#' # prepare fsets
#' f <- lcut(data.frame(a=0:1, b=0:1, c=0:1, d=0:1))
#'
#' # run perceive function: (sm.a, bi.c) has
#' # more specific rule (ve.sm.a, bi.c)
#' perceive(list(c('sm.a', 'bi.c'),
#'               c('ve.sm.a', 'bi.c'),
#'               c('sm.b', 'sm.d')),
#'          f)
#'
#' @export perceive
perceive <- function(rules,
                     fsets,
                     type=c('global', 'local'),
                     fired=NULL,
                     vars=NULL,
                     specs=NULL) {
    if (!(is.null(vars) && is.null(specs))) {
        .stop('"vars" and "specs" parameters are defunct. Specify "fsets" parameter instead.')
    }
    type <- match.arg(type)
    if (!is.list(rules) && !is.null(rules)) {
        stop("'rules' must be a list of rules")
    }
    .mustBe(is.fsets(fsets), '"fsets" must be a valid instance of the "fsets" S3 class')
    unlisted <- unique(unlist(antecedents(rules)))
    if (length(intersect(unlisted, colnames(fsets))) != length(unlisted)) {
        stop('All predicates in "rules" must be from colnames of "fsets"')
    }

    n <- colnames(fsets)
    v <- as.integer(factor(vars(fsets)))
    specs <- specs(fsets)

    if (type == 'local') {
        if (!is.vector(fired) || !is.numeric(fired)) {
            stop("If type of perception is 'local' then 'fired' must be a numeric vector")
        }
        return(.perceiveLocal(rules, n, v, specs, fired))
    } else {
        return(.perceiveGlobal(rules, n, v, specs))
    }
}
