#' Create an instance of S3 class `farules` which represents a set of fuzzy
#' association rules and their statistical characteristics.
#'
#' This function is a constructor that returns an instance of the `farules` S3 class.
#' To search for fuzzy association rules, refer to the [searchrules()] function.
#'
#' @param rules A list of character vectors, where each vector represents a
#' rule and each value of the vector represents a predicate. The first value of
#' the vector is assumed to be a rule's consequent, the rest is a rule's antecedent.
#' @param statistics A numeric matrix of various statistical characteristics of
#' the rules. Each column of that matrix corresponds to some statistic (such as
#' support, confidence, etc.) and each row corresponds to a rule in the list of
#' `rules`.
#' @return Returns an object of class `farules`.
#' @author Michal Burda
#' @seealso [searchrules()]
#' @keywords models robust
#' @export
farules <- function(rules, statistics) {
    .mustBe(is.list(rules) &&
                all(sapply(rules, function(r) { is.character(r) && length(r) > 0 })),
            "'rules' must be a list of non-empty character vectors")
    .mustBeNumericMatrix(statistics)
    .mustBe(nrow(statistics) == length(rules), "The length of 'rules' must be the same as 'nrow(statistics)'")

    res <- structure(list(rules=rules,
                          statistics=statistics),
                     class=c('farules', 'list'))
    return(res)
}


#' Test whether `x` inherits from the S3 `farules` class.
#'
#' @param x An object being tested.
#' @return `TRUE` if `x` is a valid [farules()] object and `FALSE` otherwise.
#' @author Michal Burda
#' @seealso \code{\link{farules}}
#' @keywords models robust
#' @export
is.farules <- function(x) {
    return(inherits(x, 'farules') &&
               is.list(x) &&
               is.list(x$rules) &&
               is.matrix(x$statistics))
}


#' Convert the instance of the [farules()] S3 class into a data frame.
#' Empty [farules()] object is converted into an empty [data.frame()].
#'
#' @param x An instance of class [farules()] to be transformed.
#' @param ...  Unused.
#' @return A data frame of statistics of the rules that are stored in the given
#' [farules()] object. Row names of the resulting data frame are in
#' the form: `A1 & A2 & ... & An => C`, where `Ai` are antecedent
#' predicates and `C` is a consequent. An empty [farules()] object
#' is converted into an empty [data.frame()] object.
#' @author Michal Burda
#' @seealso [farules()], [searchrules()]
#' @keywords models robust multivariate
#' @export
as.data.frame.farules <- function(x, ...) {
    if (length(x$rules) <= 0) {
        return(data.frame())
    } else {
        r <- x$statistics
        rownames(r) <- sapply(x$rules, function(rule) {
            ante <- rule[-1]
            conseq <- rule[1]
            paste(paste(ante, collapse=' & '), conseq, sep=' => ')
        })
        return(as.data.frame(r))
    }
}


#' Print an instance of the [farules()] S3 class in a human readable form.
#'
#' @param x An instance of the [farules()] S3 class
#' @param ...  Unused.
#' @return None.
#' @author Michal Burda
#' @seealso [farules()], [searchrules()]
#' @keywords models robust
#' @export
print.farules <- function(x, ...) {
    if (length(x$rules) <= 0) {
        cat('Empty rule base\n')
    } else {
        print(as.data.frame(x))
    }
}


#' Take a sequence of instances of S3 class [farules()] and combine them into a single
#' object. An error is thrown if some argument does not inherit from the [farules()]
#' class.
#'
#' @param ...  A sequence of objects of class [farules()] to be concatenated.
#' @param recursive This argument has currently no function and is added here
#' only for compatibility with generic \code{\link{c}} function.
#' @return An object of class [farules()] that is created by merging the
#' arguments together, i.e.  by concatenating the rules and row-binding the
#' statistics of given objects.
#' @author Michal Burda
#' @seealso [farules()], [searchrules()]
#' @keywords models robust
#' @examples
#'     ori1 <- farules(rules=list(letters[1:3],
#'                                letters[2:5]),
#'                     statistics=matrix(runif(16), nrow=2))
#'     ori2 <- farules(rules=list(letters[4],
#'                                letters[3:8]),
#'                     statistics=matrix(runif(16), nrow=2))
#'     res <- c(ori1, ori2)
#'     print(res)
#' @export
c.farules <- function(..., recursive=FALSE) {
    dots <- list(...)

    .mustBe(all(sapply(dots, is.farules)),
            "Cannot concatenate arguments that are not valid 'farules' objects")
    if (recursive) {
        warning("Recursion not implemented in 'c.farules'")
    }

    rules <- do.call('c', lapply(dots, '[[', 'rules'))
    statistics <- do.call('rbind', lapply(dots, '[[', 'statistics'))
    return(farules(rules, statistics))
}
