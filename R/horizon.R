.horizonAllowedTable <- list('ctx3'=list('sm'=1:3, 'me'=2:4, 'bi'=3:5),
                             'ctx5'=list('sm'=c(1,2,4), 'lm'=2:4, 'me'=c(2,4,6), 'um'=4:6, 'bi'=c(4,6,7)),
                             'ctx3bilat'=list('neg.bi'=1:3, 'neg.me'=2:4, 'neg.sm'=c(3,4,4),
                                           'ze'=3:5, 'sm'=c(4,4,5), 'me'=4:6, 'bi'=5:7),
                             'ctx5bilat'=list('neg.bi'=c(1,2,4), 'neg.um'=2:4, 'neg.me'=c(2,4,6),
                                           'neg.lm'=4:6, 'neg.sm'=c(4,6,6), 'ze'=c(4,6,8),
                                           'sm'=c(6,6,8), 'lm'=6:8, 'me'=c(6,8,10), 'um'=8:10,
                                           'bi'=c(8,10,11)))

#' Create a function that computes linguistic horizons
#'
#' Based on given `context` and `atomic` expression, this function returns a function that computes a linguistic
#' horizon, i.e. a triangular function representing basic limits of what humans treat as "small", "medium", "big" etc.
#' within given `context`. Linguistic horizon stands as a base for creation of linguistic expressions. A linguistic
#' expression is created by applying a [hedge()] on horizon. (Atomic linguistic expression is created from horizon by
#' applying an empty (`-`) hedge).
#'
#' The values of the `atomic` parameter have the following meaning (in ascending order):
#' * `neg.bi`: big negative (far from zero)
#' * `neg.um`: upper medium negative (between medium negative and big negative)
#' * `neg.me`: medium negative
#' * `neg.lm`: lower medium negative (between medium negative and small negative)
#' * `neg.sm`: small negative (close to zero)
#' * `ze`: zero
#' * `sm`: small
#' * `lm`: lower medium
#' * `me`: medium
#' * `um`: upper medium
#' * `bi`: big
#'
#' Based on the context type, the following atomic expressions are allowed:
#' * [ctx3()] (trichotomy): small, medium, big;
#' * [ctx5()] (pentachotomy): small, lower medium, medium, upper medium, big;
#' * [ctx3bilat()] (bilateral trichotomy): negative big, negative medium, negative small,
#'   zero, small, medium, big;
#' * [ctx5bilat()] (bilateral pentachotomy): negative big, negative medium, negative
#'   small, zero, small, medium, big.
#'
#' This function is quite low-level. Perhaps a more convenient way to create linguistic expressions
#' is to use the [lingexpr()] function.
#'
#' @param context A context of linguistic expressions (see [ctx3()], [ctx5()], [ctx3bilat()] or [ctx5bilat()])
#' @param atomic An atomic expression whose horizon we would like to obtain
#' @return A function of single argument that must be a numeric vector
#' @author Michal Burda
#' @seealso [ctx3()], [ctx5()], [ctx3bilat()], [ctx5bilat()], [hedge()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#'     plot(horizon(ctx3(), 'sm'), from=-1, to=2)
#'     plot(horizon(ctx3(), 'me'), from=-1, to=2)
#'     plot(horizon(ctx3(), 'bi'), from=-1, to=2)
#'
#'     a <- horizon(ctx3(), 'sm')
#'     plot(a)
#'     h <- hedge('ve')
#'     plot(h)
#'     verySmall <- function(x) h(a(x))
#'     plot(verySmall)
#' @export
horizon <- function(context,
                    atomic=c('sm', 'me', 'bi', 'lm', 'um', 'ze',
                             'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um')) {
    type <- NULL
    for (clazz in names(.horizonAllowedTable)) {
        if (inherits(context, clazz)) {
            type <- clazz
        }
    }
    .mustBe(!is.null(type),
            paste0("'context' must be an object of one of the following classes: '",
                   paste(names(.horizonAllowedTable), collapse="', '"), "'"))

    allowed <- .horizonAllowedTable[[type]]
    atomic <- match.arg(atomic)
    .mustBeOneOf(atomic, names(allowed))

    space <- c(-Inf, context, Inf)
    arg <- space[allowed[[atomic]]]
    return(triangular(arg[1], arg[2], arg[3]))
}
