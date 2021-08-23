# Create an (intermediate) fuzzy quantifier
#
#' A quantifier is a function that computes a fuzzy truth value of a claim about
#' the quantity. This function creates the <1>-type quantifier. (See the examples
#' below on how to use it as a quantifier of the <1,1> type.)
#'
#' @param quantity the quantity to be evaluated. 'all' computes the degree of
#'   truth to which all elements of the universe have the given property,
#'   'almost.all', #'   'most', and 'many' evaluate whether the property is
#'   present in extremely big, very big, or not small number of elements from
#'   the universe, where these linguistic expressions are internally modeled
#'   using the [lingexpr()] function. 'at.least' quantity requires the 'n'
#'   argument to be specified, as it computes the truth value that at least
#'   \eqn{n} elements from the universe have the given property.
#' @param n the number of elements in the 'at.least n' quantifier
#' @param alg the underlying algebra in which to compute the quantifier.
#'   Note that the algebra must have properly defined the `order` function,
#'   as in the case of 'goedel', 'goguen', or 'lukasiewicz' algebra, (see the
#'   [algebra()] function) or as in the [dragonfly()] or [lowerEst()] algebra.
#' @return A two-argument function, which expects two numeric vectors of equal length
#'   (the vector elements are recycled to ensure equal lengths). The first argument, `x`,
#'   is a vector of membership degrees to be measured, the second argument, `w`, is
#'   the vector of weights to which the element belongs to the universe.
#'
#'   Let \eqn{U} be the set of input vector indices (1 to `length(x)`). Then the quantifier
#'   computes the truth values accordingly to the following formula:
#'   \eqn{\vee_{z \subseteq U} \wedge_{u \in z} (x[u]) \wedge measure(m_z)},
#'   where
#'   \eqn{m_z = sum(w)} for `"some"` and `"at.least` and \eqn{m_z = sum(w[z]) / sum(w)} otherwise.
#'   See [sugeno()] for more details on how the quantifier is evaluated.
#'
#'   Setting `w` to 1 yields to operation of the <1> quantifier as developed by Dvořák et al.
#'   To compute the <1,1> quantifier as developed by Dvořák et al., e.g. "almost all A are B", `w` must
#'   be set again to 1 and `x` to the result of the implication \eqn{A \Rightarrow B}.
#'   To compute the <1,1> quantifier as proposed by Murinová et al., e.g. "almost all A are B",
#'   `x` must be set to the result of the implication \eqn{A \Rightarrow B} and `w` to the membership
#'   degrees of \eqn{A}. See the examples below.
#' @references Dvořák, A., Holčapek, M. L-fuzzy quantifiers of type <1> determined by fuzzy measures.
#'   Fuzzy Sets and Systems vol.160, issue 23, 3425-3452, 2009.
#' @references Dvořák, A., Holčapek, M. Type <1,1> fuzzy quantifiers determined by fuzzy measures.
#'   IEEE International Conference on Fuzzy Systems (FuzzIEEE), 2010.
#' @references Murinová, P., Novák, V. The theory of intermediate quantifiers in fuzzy natural logic
#'   revisited and the model of "Many". Fuzzy Sets and Systems, vol 388, 2020.
#' @author Michal Burda
#' @keywords models robust
#' @seealso [sugeno()], [lingexpr()]
#' @examples
#'   # Dvorak <1> "almost all" quantifier
#'   q <- quantifier('almost.all')
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   q(x=a, w=1)
#'
#'   # Dvorak <1,1> "almost all" quantifier (w set to 1)
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   b <- c(0.2, 1, 0, 0.5, 0.8)
#'   q <- quantifier('almost.all')
#'   q(x=lukas.residuum(a, b), w=1)
#'
#'   # Murinová <1,1> "almost all" quantifier (note w set to a)
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   b <- c(0.2, 1, 0, 0.5, 0.8)
#'   q <- quantifier('almost.all')
#'   q(x=lukas.residuum(a, b), w=a)
#'
#'   # Murinová <1,1> "some" quantifier
#'   a <- c(0.9, 1, 1, 0.2, 1)
#'   b <- c(0.2, 1, 0, 0.5, 0.8)
#'   q <- quantifier('some')
#'   q(x=plukas.tnorm(a, b), w=a)
#' @export
quantifier <- function(quantity=c('all', 'almost.all', 'most', 'many', 'some', 'at.least'),
                       n=NULL,
                       alg=c('lukasiewicz', 'goedel', 'goguen')) {
    quantity <- match.arg(quantity)
    if (is.character(alg)) {
        alg <- match.arg(alg)
        alg <- algebra(alg)
    }
    .mustBe(is.algebra(alg), "'alg' must be either one of 'goedel', 'goguen', lukasiewicz', or an instance of class 'algebra'")

    m <- NULL
    r <- NULL
    if (quantity == 'all') {
        m <- function(x) { (x >= 1) + 0 }
        r <- TRUE
    } else if (quantity == 'almost.all') {
        m <- lingexpr(context=ctx3(), atomic='bi', hedge='ex')
        r <- TRUE
    } else if (quantity == 'most') {
        m <- lingexpr(context=ctx3(), atomic='bi', hedge='ve')
        r <- TRUE
    } else if (quantity == 'many') {
        m <- lingexpr(context=ctx3(), atomic='sm', hedge='-', negated=TRUE)
        r <- TRUE

    # quantifiers with decreasing measure function are still unclear to me
    #} else if (quantity == 'few') {
        #m <- lingexpr(context=ctx3(), atomic='sm', hedge='si')
        #r <- TRUE
    #} else if (quantity == 'several') {
        #m <- lingexpr(context=ctx3(), atomic='sm', hedge='ve')
        #r <- TRUE
    #} else if (quantity == 'at.most') {
        #m <- function(x) { (x <= n) + 0 }
        #r <- FALSE

    } else if (quantity == 'some') {
        m <- function(x) { (x > 0) + 0 }
        r <- FALSE
    } else if (quantity == 'at.least') {
        .mustBeNumericScalar(n)
        .mustBe(n >= 0)
        m <- function(x) { (x >= n) + 0 }
        r <- FALSE
    } else {
        .stop(paste0('Unknown quantity: ', quantity))
    }

    sugeno(measure=m, relative=r, strong=FALSE, alg=alg)
}
