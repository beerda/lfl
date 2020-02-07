#' Creator of functions representing linguistic expressions
#'
#' A linguistic expression represents vague human terms such as "very small", "extremely big" etc. Such notions are
#' always reasoned within a given context. `lingexpr` returns a function that models a selected linguistic expression.
#' Accordingly to the given `context`, `atomic` expression (such as "small", "big") and a linguistic `hedge` (such as
#' `very`, `extremely`), the returned function transforms numeric values into degrees (from `[0, 1]` interval),
#' at which the values correspond to the expression.
#'
#' Based on the context type, the following atomic expressions are allowed:
#' * [ctx3()] (trichotomy): small, medium, big;
#' * [ctx5()] (pentachotomy): small, lower medium, medium, upper medium, big;
#' * [ctx3bilat()] (bilateral trichotomy): negative big, negative medium, negative small,
#'   zero, small, medium, big;
#' * [ctx5bilat()] (bilateral pentachotomy): negative big, negative medium, negative
#'   small, zero, small, medium, big.
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
#' `hedge` parameter has the following meaning:
#' * `ex`: extremely,
#' * `si`: significantly,
#' * `ve`: very,
#' * `ty`: typically,
#' * `-`: empty hedge,
#' * `ml`: more or less,
#' * `ro`: roughly,
#' * `qr`: quite roughly,
#' * `vr`: very roughly.
#'
#' Accordingly to the theory of linguistic expressions by Novak, not every hedge is applicable to each atomic
#' expression. The combinations of allowed pairs can be found in [allowed.lingexpr]. Trying to create forbidden
#' combination results in error.
#'
#' @param context A context of linguistic expressions (see [ctx3()], [ctx5()], [ctx3bilat()] or [ctx5bilat()])
#' @param atomic An atomic expression whose horizon we would like to obtain
#' @param hedge The type of the required linguistic hedge ('-' for no hedging)
#' @param hedgeParams Parameters that determine the shape of the hedges
#' @return Returns a function with a single argument, which has to be a numeric vector.
#' @author Michal Burda
#' @seealso [horizon()], [hedge()], [fcut()], [lcut()], [ctx()]
#' @keywords models robust
#' @examples
#'     small <- lingexpr(ctx3(0, 0.5, 1), atomic='sm', hedge='-')
#'     small(0)   # 1
#'     small(0.8) # 0
#'     plot(small)
#'
#'     verySmall <- lingexpr(ctx3(0, 0.5, 1), atomic='sm', hedge='ve')
#'     plot(verySmall)
#' @export
lingexpr <- function(context,
                     atomic=c('sm', 'me', 'bi', 'lm', 'um', 'ze',
                              'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um'),
                     hedge=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'),
                     hedgeParams=defaultHedgeParams) {
    atomic <- match.arg(atomic)
    h <- match.arg(hedge)
    if (!allowed.lingexpr[h, atomic]) {
        stop(paste0("Hedge '", hedge, "' is not applicable on atomic expression '", atomic, "'"))
    }
    rm(hedge) # in order to access global hedge() function

    hor <- horizon(context, atomic)
    hed <- hedge(h, hedgeParams=hedgeParams)

    return(function(x) {
        hed(hor(x))
    })
}

#' @rdname lingexpr
#' @export
allowed.lingexpr <- array(
    # ex si ve ty -  ml ro qr vr
    c(T, T, T, F, T, T, T, T, T,  # neg.bi
      F, F, F, F, T, F, F, F, F,  # neg.um
      F, F, F, T, T, T, T, T, T,  # neg.me
      F, F, F, F, T, F, F, F, F,  # neg.lm
      T, T, T, F, T, T, T, T, T,  # neg.sm
      F, F, F, F, T, T, T, F, F,  # ze
      T, T, T, F, T, T, T, T, T,  # sm
      F, F, F, F, T, F, F, F, F,  # lm
      F, F, F, T, T, T, T, T, T,  # me
      F, F, F, F, T, F, F, F, F,  # um
      T, T, T, F, T, T, T, T, T), # bi
    dim=c(9, 11),
    dimnames=list(c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'),
                  c('neg.bi', 'neg.um', 'neg.me', 'neg.lm', 'neg.sm', 'ze', 'sm', 'lm', 'me', 'um', 'bi')))

