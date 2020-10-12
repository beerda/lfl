#' A factory function for creation of sugeno-integrals.
#'
#' @param measure A non-decreasing function that assigns a truth value from the
#'   \eqn{[0, 1]} interval to the either relative or absolute quantity
#' @param relative Whether the measure assumes relative or absolute quantity.
#'   Relative quantity is always a number from the \eqn{[0,1]} interval
#' @param strong Whether to use the strong conjunction (`TRUE`) or the weak
#'   conjunction (`FALSE`)
#' @param alg The underlying algebra must be either a string (one from 'lukasiewicz',
#'   'goedel' or 'goguen') or an instance of the S3 class [algebra()].
#' @return A two-argument function, which expects two numeric vectors of equal length
#'   (the vector elements are recycled to ensure equal lengths). The first argument, `x`,
#'   is a vector of membership degrees to be measured, the second argument, `w`, is
#'   the vector of weights.
#'
#'   Let \eqn{U} be the set of input vector indices (1 to `length(x)`). Then the sugeno integral
#'   computes the truth values accordingly to the following formula:
#'   \eqn{\vee_{z \subseteq U} \wedge_{u \in z} (x[u]) CONJ measure(m_z)},
#'   where
#'   \eqn{m_z = sum(w[z]) / sum(w)} if `relative==TRUE` or \eqn{m_z = sum(w)} if `relative==FALSE`
#'   and where CONJ is a strong conjunction (i.e. `alg$pt`) or a weak conjunction
#'   (i.e. `alg$pi`) accordingly to the `strong` parameter.
#' @author Michal Burda
#' @keywords models robust
#' @seealso [quantifier()], [lingexpr()]
#' @examples
#'   # Dvorak <1> "almost all" quantifier
#'   q <- sugeno(lingexpr(ctx3(), atomic='bi', hedge='ex'))
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   q(x=a, w=1)
#'
#'   # Dvorak <1,1> "almost all" quantifier
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   b <- c(0.2, 1, 0, 0.5, 0.8)
#'   q <- sugeno(lingexpr(ctx3(), atomic='bi', hedge='ex'))
#'   q(x=lukas.residuum(a, b), w=1)
#'
#'   # Murinová <1,1> "almost all" quantifier
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   b <- c(0.2, 1, 0, 0.5, 0.8)
#'   q <- sugeno(lingexpr(ctx3(), atomic='bi', hedge='ex'))
#'   q(x=lukas.residuum(a, b), w=a)
#' @export
sugeno <- function(measure,
                   relative=TRUE,
                   strong=FALSE,
                   alg=c('lukasiewicz', 'goedel', 'goguen')) {
    .mustBeFunction(measure)
    .mustBeLogicalScalar(relative)
    .mustBeLogicalScalar(strong)
    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    relate <- if (relative) function(u) { u / u[length(u)] } else identity
    conj <- if (strong) alg$pt else alg$pi

    function(x, w=1) {
        .mustBeNumericVector(x)
        .mustBeNumericVector(w)
        .mustBe(all(!is.na(w)), "The 'w' argument must not contain NAs")

        l <- max(length(x), length(w))
        x <- rep_len(x, l)
        w <- rep_len(w, l)
        o <- alg$order(x, decreasing=TRUE)
        m <- measure(relate(cumsum(w[o])))
        alg$s(conj(x[o], m))
    }
}
