#' Determine whether the first set `x` of predicates is more specific (or equal)
#' than `y` with respect to `vars` and `specs`.
#'
#' The function takes two character vectors of predicates and determines whether
#' `x` is more specific (or equal w.r.t. the specificity) than `y`. The
#' specificity relation is fully determined with the values of the [vars()] vector
#' and the [specs()] incidence matrix that is encapsulated in the given `fsets` object.
#'
#' Let \eqn{x_i} and \eqn{y_j} represent some predicates of vectors `x` and `y`,
#' respectively. Function assumes that each vector `x` and `y` does not
#' contain two or more predicates with the same value of [vars()].
#'
#' This function returns TRUE iff all of the following conditions hold:
#' * for any \eqn{y_j} there exists \eqn{x_i} such that \eqn{vars[y_j] = vars[x_i]};
#' * for any \eqn{x_i} there either does not exist \eqn{y_j} such that
#'   \eqn{vars[x_i] = vars[y_j]}, or \eqn{x_i = y_j}, or \eqn{specs[x_i, y_j] = 1}.
#'
#' @param x The first character vector of predicates.
#' @param y The second character vector of predicates.
#' @param fsets A valid instance of the [fsets()] class such that all values in `x` and `y`
#' can be found in `colnames(fsets)`
#' @param vars Deprecated parameter must be `NULL`.
#' @param specs Deprecated parameter must be `NULL`.
#' @return TRUE or FALSE (see description).
#' @author Michal Burda
#' @seealso [perceive()], [pbld()], [fsets()], [vars()], [specs()]
#' @keywords models robust
#' @examples
#'     # prepare fsets object
#'     v <- c(rep('a', 3), rep('b', 3), rep('c', 3), rep('d', 3))
#'     s <- matrix(c(0,1,0, 0,0,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'
#'                   0,0,0, 0,1,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'
#'                   0,0,0, 0,0,0, 0,1,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'
#'                   0,0,0, 0,0,0, 0,0,0, 0,1,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0,
#'                   0,0,0, 0,0,0, 0,0,0, 0,0,0),
#'                 byrow=TRUE,
#'                 ncol=12)
#'     m <- matrix(0, nrow=1, ncol=12)
#'     colnames(m) <- paste(rep(c('VeSm', 'Sm', 'Bi'), times=4),
#'                          rep(c('a', 'b', 'c', 'd'), each=3),
#'                          sep='.')
#'     f <- fsets(m, v, s)
#'
#'
#'     # returns TRUE
#'     is.specific(c('VeSm.a', 'Bi.c'),
#'                 c('VeSm.a', 'Bi.c'),
#'                 f)
#'
#'     # returns TRUE (x and y swapped return FALSE)
#'     is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
#'                 c('Sm.a', 'Bi.c', 'Sm.d'),
#'                 f)
#'
#'     # returns TRUE (x and y swapped return FALSE)
#'     is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
#'                 c('VeSm.a', 'Bi.c'),
#'                 f)
#'
#'     # returns TRUE (x and y swapped return FALSE)
#'     is.specific(c('VeSm.a', 'Bi.c', 'Sm.d'),
#'                 character(),
#'                 f)
#'
#'     # returns FALSE
#'     is.specific(c('Sm.a'), c('Bi.c'), f)
#'
#'     # returns FALSE
#'     is.specific(c('VeSm.a', 'Sm.c'),
#'                 c('Sm.a', 'Bi.c'),
#'                 f)
#' @export
is.specific <- function(x, y, fsets, vars=NULL, specs=NULL) {
    if (!(is.null(vars) && is.null(specs))) {
        .stop('"vars" and "specs" parameters are defunct. Specify "fsets" parameter instead.')
    }
    .mustBeCharacterVector(x)
    .mustBeCharacterVector(y)
    .mustBe(is.fsets(fsets), '"fsets" must be a valid instance of the "fsets" S3 class')

    n <- colnames(fsets)
    xi <- match(x, n)
    yi <- match(y, n)

    .mustBe(!any(is.na(xi)), 'All values in "x" must be from colnames of "fsets"')
    .mustBe(!any(is.na(yi)), 'All values in "y" must be from colnames of "fsets"')

    if ((length(unique(xi)) != length(xi)) || (length(unique(yi)) != length(yi))) {
        .stop('Unable to work with rules containing the same predicate more times')
    }

    v <- as.integer(factor(vars(fsets)))
    .is.specific(xi, yi, v, specs(fsets))
}


.is.specific <- function(xi, yi, v, specs) {
    return(.Call('_lfl_specificity',
                 xi-1, yi-1,  # C++ array indexing is from 0
                 v, specs, PACKAGE='lfl'))
}
