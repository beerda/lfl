#' Extract consequent-part (right-hand side) of rules in a list
#'
#' Given a list of rules or an instance of the S3 [farules()] class,
#' the function returns a list of their consequents (i.e.
#' right-hand side of rules).
#'
#' This function assumes `rules` to be a valid [farules()] object or
#' a list of character vectors where
#' the first element of each vector is a consequent part and the
#' rest is an antecedent part of rules. Function returns a list of
#' consequents.
#'
#' @param rules Either a list of character vectors or an object of class [farules()].
#' @return A list of character vectors.
#' @author Michal Burda
#' @seealso [antecedents()], [farules()], [searchrules()]
#' @keywords models robust
#' @export
#' @examples
#'     rules <- list(c('a', 'b', 'c'), c('d'), c('a', 'e'))
#'     consequents(rules)
#'     unlist(consequents(rules))   # as vector
consequents <- function(rules) {
    .mustBe(is.farules(rules) ||
                (is.list(rules) && all(sapply(rules, function(r) { is.character(r) && length(r) > 0 }))),
            "'rules' must be either a list of character vectors or a valid farules object")
    r <- rules
    if (is.farules(r)) {
        r <- r$rules
    }
    return(lapply(r, function(rule) rule[1]))
}
