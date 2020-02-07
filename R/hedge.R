#' A list of the parameters that define the shape of the hedges.
#' @export
defaultHedgeParams <- list('ex'=c(0.77, 0.90, 0.99),
                           'si'=c(0.71, 0.85, 0.962),
                           've'=c(0.66, 0.79, 0.915),
                           'ty'=c(0.88, 0.95, 1),
                           '-' =c(0.45, 0.68, 0.851),
                           'ml'=c(0.43, 0.6, 0.727),
                           'ro'=c(0.4, 0.52, 0.619),
                           'qr'=c(0.3, 0.42, 0.528),
                           'vr'=c(0.1, 0.2, 0.421))



#' Linguistic hedges
#'
#' Returns a function that realizes linguistic hedging - i.e. transformation of linguistic
#' horizon (see [horizon()]) into a linguistic expression.
#'
#' `hedge()` returns a function that realizes the selected linguistic hedge on its parameter:
#' * `ex`: extremely,
#' * `si`: significantly,
#' * `ve`: very,
#' * `ty`: typically,
#' * `-`: empty hedge (no hedging),
#' * `ml`: more or less,
#' * `ro`: roughly,
#' * `qr`: quite roughly,
#' * `vr`: very roughly.
#'
#' This function is quite low-level. Perhaps a more convenient way to create linguistic expressions
#' is to use the [lingexpr()] function.
#'
#' @param type The type of the required linguistic hedge
#' @param hedgeParams Parameters that determine the shape of the hedges
#' @return Returns a function with a single argument, which has to be a numeric
#' vector.
#' @author Michal Burda
#' @seealso [horizon()], [lingexpr()], [fcut()], [lcut()], [ctx()]
#' @keywords models robust
#' @examples
#'     a <- horizon(ctx3(), 'sm')
#'     plot(a)
#'     h <- hedge('ve')
#'     plot(h)
#'     verySmall <- function(x) h(a(x))
#'     plot(verySmall)
#'
#'     # the last plot should be equal to:
#'     plot(lingexpr(ctx3(), atomic='sm', hedge='ve'))
#'
#' @export
hedge <- function(type=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'),
                  hedgeParams=defaultHedgeParams) {
    type <- match.arg(type)
    params <- hedgeParams[[type]]
    return(function(x) {
        .Call('_lfl_hedge', as.numeric(as.vector(x)), params, PACKAGE='lfl')
    })
}
