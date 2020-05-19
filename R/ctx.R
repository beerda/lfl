#' Context for linguistic expressions
#'
#' A context describes a range of allowed values for a data column.
#'
#' A context describes a range of allowed values for a data column. For that,
#' only the borders of the interval, i.e. minimum and maximum, are usually
#' needed, but we use contexts to hold more additional information that is
#' crucial for the construction of linguistic expressions.
#'
#' Currently, four different contexts are supported that determine the types of
#' possible linguistic expressions, as constructed with [lingexpr()].
#' Unilateral or bilateral context is allowed in the variants of trichotomy or
#' pentachotomy. Trichotomy distinguishes three points in the interval: the
#' lowest value, highest value, and center. Pentachotomy adds lower center and
#' upper center to them. As opposite to unilateral, the bilateral context
#' handles explicitly the negative values. That is, bilateral context expects
#' some middle point, the origin (usually 0), around which the positive and
#' negative values are placed.
#'
#' Concretely, the type of the context determines the allowed atomic
#' expressions as follows:
#' * `ctx3`: trichotomy (low, center, high) enables atomic expressions:
#'   small, medium, big;
#' * `ctx5`: pentachotomy (low, lowerCenter, center, upperCenter, high) enables
#'   atomic expressions: small, lower medium, medium, upper medium, big;
#' * `ctx3bilat`: bilateral trichotomy (negMax, negCenter, origin, center, max)
#'   enables atomic expressions: negative big, negative medium, negative small,
#'   zero, small, medium, big;
#' * `ctx5bilat`: bilateral pentachotomy (negMax, negCenter, origin, center,
#'   max) enables atomic expressions: negative big, negative medium, negative
#'   small, zero, small, medium, big.
#'
#' The `as.ctx*` functions return instance of the appropriate class. The
#' functions perform the conversion so that missing points of the new context
#' are computed from the old context that is being transformed. In the
#' subsequent table, rows represent compatible values of different context
#' types:
#'
#' \tabular{llll}{
#' ctx3   \tab ctx5        \tab ctx3bilat \tab ctx5bilat      \cr
#'        \tab             \tab negMax    \tab negMax         \cr
#'        \tab             \tab           \tab negUpperCenter \cr
#'        \tab             \tab negCenter \tab negCenter      \cr
#'        \tab             \tab           \tab negLowerCenter \cr
#' low    \tab low         \tab origin    \tab origin         \cr
#'        \tab lowerCenter \tab           \tab lowerCenter    \cr
#' center \tab center      \tab center    \tab center \cr
#'        \tab upperCenter \tab           \tab upperCenter \cr
#' high   \tab high        \tab max       \tab max
#' }
#'
#' The `as.ctx*` conversion is performed by replacing values by rows, as
#' indicated in the table above.  When converting from a context with less
#' points to a context with more points (e.g. from unilateral to bilateral, or
#' from trichotomy to pentachotomy), missing points are computed as follows:
#' * `center` is computed as a mean of `origin` (or `low`) and `max` (or `high`).
#' * `lowerCenter` is computed as a mean of `origin` (or `low`) and `center`.
#' * `upperCenter` is computed as a mean of `mas` (or `high`) and `center`.
#' * negative points (such as `negMax`, `negCenter` etc.) are computed
#' symmetrically around `origin` to the corresponding positive points.
#'
#' The code `as.ctx*` functions allow the parameter to be also a numeric
#' vector of size equal to the number of points required for the given context
#' type, i.e. 3 (`ctx3`), 5 (`ctx3bilat`, `ctx5`), or 9 (`ctx5bilat`).
#'
#' @param negMax Lowest negative value of a bilateral context.
#' @param negUpperCenter A typical negative value between `negMax` and
#' `negCenter`.
#' @param negCenter A negative middle value.
#' @param negLowerCenter A typical negative value between `negCenter` and
#' `negOrigin`.
#' @param origin Origin, i.e. the initial point of the bilateral context. It is
#' typically a value of zero.
#' @param lowerCenter A typical positive value between origin and center.
#' @param center A positive middle value of a bilateral context, or simply a
#' middle value of an unilateral context.
#' @param upperCenter A typical positive value between center and maximum.
#' @param max Highest value of a bilateral context.
#' @param low Lowest value of an unilateral context.
#' @param high Highest value of an unilateral context.
#' @param relCenter A relative quantity used to compute the `negCenter`
#' and/or `center`, if they are not specified explicitly. The sensible
#' value is 0.5 for context symmetric around center, or 0.42 as proposed by
#' Novak.
#' @param x A value to be examined or converted. For `as.ctx*`, it can be
#' an instance of any `ctx*` class or a numeric vector of size equal to
#' the number of points required for the given context type.
#' @return `ctx*` and `as.ctx*` return an instance of the appropriate
#' class. `is.ctx*` returns `TRUE` or `FALSE`.
#' @author Michal Burda
#' @seealso [minmax()], [lingexpr()], [horizon()], [hedge()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#'     ctx3(low=0, high=10)
#'     as.ctx3bilat(ctx3(low=0, high=10))
#' @name ctx
NULL


#' @rdname ctx
#' @export
ctx3 <- function(low=0,
                 center=low+(high-low)*relCenter,
                 high=1,
                 relCenter=0.5) {
    .mustBeNumericScalar(low)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(high)
    .mustBeNumericScalar(relCenter)
    .mustBe(0 < relCenter && relCenter < 1, "'relCenter' must be a number between 0 and 1")
    .mustBe(low < center, "'low' must be lower than 'center'")
    .mustBe(center < high, "'center' must be lower than 'high'")

    structure(c(low, center, high),
              names=c('low', 'center', 'high'),
              class=c('ctx3', 'numeric'))
}


#' @rdname ctx
#' @export
ctx3bilat <- function(negMax=-1,
                      negCenter=origin+(negMax-origin)*relCenter,
                      origin=0,
                      center=origin+(max-origin)*relCenter,
                      max=1,
                      relCenter=0.5) {
    .mustBeNumericScalar(negMax)
    .mustBeNumericScalar(negCenter)
    .mustBeNumericScalar(origin)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(max)
    .mustBeNumericScalar(relCenter)
    .mustBe(0 < relCenter && relCenter < 1, "'relCenter' must be a number between 0 and 1")
    .mustBe(negMax < negCenter, "'negMax' must be lower than 'negCenter'")
    .mustBe(negCenter < origin, "'negCenter' must be lower than 'origin'")
    .mustBe(origin < center, "'origin' must be lower than 'center'")
    .mustBe(center < max, "'center' must be lower than 'max'")

    structure(c(negMax, negCenter, origin, center, max),
              names=c('negMax', 'negCenter', 'origin', 'center', 'max'),
              class=c('ctx3bilat', 'numeric'))
}


#' @rdname ctx
#' @export
ctx5 <- function(low=0,
                 lowerCenter=mean(c(low, center)),
                 center=low+(high-low)*relCenter,
                 upperCenter=mean(c(center, high)),
                 high=1,
                 relCenter=0.5) {
    .mustBeNumericScalar(low)
    .mustBeNumericScalar(lowerCenter)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(upperCenter)
    .mustBeNumericScalar(high)
    .mustBeNumericScalar(relCenter)
    .mustBe(0 < relCenter && relCenter < 1, "'relCenter' must be a number between 0 and 1")
    .mustBe(low < lowerCenter, "'low' must be lower than 'lowerCenter'")
    .mustBe(lowerCenter < center, "'lowerCenter' must be lower than 'center'")
    .mustBe(center < upperCenter, "'center' must be lower than 'upperCenter'")
    .mustBe(upperCenter < high, "'upperCenter' must be lower than 'high'")

    structure(c(low, lowerCenter, center, upperCenter, high),
              names=c('low', 'lowerCenter', 'center', 'upperCenter', 'high'),
              class=c('ctx5', 'numeric'))
}


#' @rdname ctx
#' @export
ctx5bilat <- function(negMax=-1,
                      negUpperCenter=mean(c(negCenter, negMax)),
                      negCenter=origin+(negMax-origin)*relCenter,
                      negLowerCenter=mean(c(origin, negCenter)),
                      origin=0,
                      lowerCenter=mean(c(origin, center)),
                      center=origin+(max-origin)*relCenter,
                      upperCenter=mean(c(center, max)),
                      max=1,
                      relCenter=0.5) {
    .mustBeNumericScalar(negMax)
    .mustBeNumericScalar(negUpperCenter)
    .mustBeNumericScalar(negCenter)
    .mustBeNumericScalar(negLowerCenter)
    .mustBeNumericScalar(origin)
    .mustBeNumericScalar(lowerCenter)
    .mustBeNumericScalar(center)
    .mustBeNumericScalar(upperCenter)
    .mustBeNumericScalar(max)
    .mustBeNumericScalar(relCenter)
    .mustBe(0 < relCenter && relCenter < 1, "'relCenter' must be a number between 0 and 1")
    .mustBe(negMax < negUpperCenter, "'negMax' must be lower than 'negUpperCenter'")
    .mustBe(negUpperCenter < negCenter, "'negUpperCenter' must be lower than 'negCenter'")
    .mustBe(negCenter < negLowerCenter, "'negCenter' must be lower than 'negLowerCenter'")
    .mustBe(negLowerCenter < origin, "'negLowerCenter' must be lower than 'origin'")
    .mustBe(origin < lowerCenter, "'origin' must be lower than 'lowerCenter'")
    .mustBe(lowerCenter < center, "'lowerCenter' must be lower than 'center'")
    .mustBe(center < upperCenter, "'center' must be lower than 'upperCenter'")
    .mustBe(upperCenter < max, "'upperCenter' must be lower than 'max'")

    structure(c(negMax, negUpperCenter, negCenter, negLowerCenter, origin, lowerCenter, center, upperCenter, max),
              names=c('negMax', 'negUpperCenter', 'negCenter', 'negLowerCenter', 'origin',
                      'lowerCenter', 'center', 'upperCenter', 'max'),
              class=c('ctx5bilat', 'numeric'))
}


#' @rdname ctx
#' @export
as.ctx3 <- function(x) UseMethod("as.ctx3")

#' @rdname ctx
#' @export
as.ctx3.ctx3 <- identity

#' @rdname ctx
#' @export
as.ctx3.ctx3bilat <- function(x) ctx3(x['origin'], x['center'], x['max'])

#' @rdname ctx
#' @export
as.ctx3.ctx5 <- function(x) ctx3(x['low'], x['center'], x['high'])

#' @rdname ctx
#' @export
as.ctx3.ctx5bilat <- function(x) ctx3(x['origin'], x['center'], x['max'])

#' @rdname ctx
#' @export
as.ctx3.default <- function(x) {
    x <- as.vector(x)
    .mustBe(length(x) == 3, "'x' must be of length 3")
    ctx3(x[1], x[2], x[3])
}


#' @rdname ctx
#' @export
as.ctx3bilat <- function(x) UseMethod("as.ctx3bilat")

#' @rdname ctx
#' @export
as.ctx3bilat.ctx3bilat <- identity

#' @rdname ctx
#' @export
as.ctx3bilat.ctx3 <- function(x) ctx3bilat(2*x['low']-x['high'],
                                           2*x['low']-x['center'],
                                           x['low'],
                                           x['center'],
                                           x['high'])

#' @rdname ctx
#' @export
as.ctx3bilat.ctx5 <- function(x) as.ctx3bilat(as.ctx3(x))

#' @rdname ctx
#' @export
as.ctx3bilat.ctx5bilat <- function(x) ctx3bilat(x['negMax'], x['negCenter'], x['origin'], x['center'], x['max'])

#' @rdname ctx
#' @export
as.ctx3bilat.default <- function(x) {
    x <- as.vector(x)
    .mustBe(length(x) == 5, "'x' must be of length 5")
    ctx3bilat(x[1], x[2], x[3], x[4], x[5])
}


#' @rdname ctx
#' @export
as.ctx5 <- function(x) UseMethod("as.ctx5")

#' @rdname ctx
#' @export
as.ctx5.ctx5 <- identity

#' @rdname ctx
#' @export
as.ctx5.ctx3 <- function(x) ctx5(x['low'],
                                 mean(c(x['low'], x['center'])),
                                 x['center'],
                                 mean(c(x['center'], x['high'])), x['high'])

#' @rdname ctx
#' @export
as.ctx5.ctx3bilat <- function(x) as.ctx5(as.ctx3(x))

#' @rdname ctx
#' @export
as.ctx5.ctx5bilat <- function(x) ctx5(x['origin'], x['lowerCenter'], x['center'], x['upperCenter'], x['max'])

#' @rdname ctx
#' @export
as.ctx5.default <- function(x) {
    x <- as.vector(x)
    .mustBe(length(x) == 5, "'x' must be of length 5")
    ctx5(x[1], x[2], x[3], x[4], x[5])
}


#' @rdname ctx
#' @export
as.ctx5bilat <- function(x) UseMethod("as.ctx5bilat")

#' @rdname ctx
#' @export
as.ctx5bilat.ctx5bilat <- identity

#' @rdname ctx
#' @export
as.ctx5bilat.ctx3 <- function(x) as.ctx5bilat(as.ctx5(x))

#' @rdname ctx
#' @export
as.ctx5bilat.ctx3bilat <- function(x) ctx5bilat(x['negMax'],
                                                mean(c(x['negMax'], x['negCenter'])),
                                                x['negCenter'],
                                                mean(c(x['negCenter'], x['origin'])),
                                                x['origin'],
                                                mean(c(x['origin'], x['center'])),
                                                x['center'], mean(c(x['center'], x['max'])),
                                                x['max'])

#' @rdname ctx
#' @export
as.ctx5bilat.ctx5 <- function(x) {
    shift <- 2*x['low']
    ctx5bilat(shift-x['high'], shift-x['upperCenter'], shift-x['center'], shift-x['lowerCenter'],
              x['low'], x['lowerCenter'], x['center'], x['upperCenter'], x['high'])
}

#' @rdname ctx
#' @export
as.ctx5bilat.default <- function(x) {
    x <- as.vector(x)
    .mustBe(length(x) == 9, "'x' must be of length 9")
    ctx5bilat(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9])
}

#' @rdname ctx
#' @export
is.ctx3 <- function(x) {
    return(inherits(x, 'ctx3') &&
           is.numeric(x) &&
           length(x) == 3)
}


#' @rdname ctx
#' @export
is.ctx3bilat <- function(x) {
    return(inherits(x, 'ctx3bilat') &&
           is.numeric(x) &&
           length(x) == 5)
}


#' @rdname ctx
#' @export
is.ctx5 <- function(x) {
    return(inherits(x, 'ctx5') &&
           is.numeric(x) &&
           length(x) == 5)
}


#' @rdname ctx
#' @export
is.ctx5bilat <- function(x) {
    return(inherits(x, 'ctx5bilat') &&
           is.numeric(x) &&
           length(x) == 9)
}
