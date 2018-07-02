#' Algebra for Fuzzy Sets
#'
#' Compute triangular norms (t-norms), triangular conorms (t-conorms), residua,
#' bi-residua, and negations.
#'
#' `goedel.tnorm`, `lukas.tnorm`, and `goguen.tnorm` compute the
#' Goedel, Lukasiewicz, and Goguen triangular norm (t-norm) from all values in
#' the arguments. If the arguments are vectors they are combined together
#' firstly so that a numeric vector of length 1 is returned.
#'
#' `pgoedel.tnorm`, `plukas.tnorm`, and `pgoguen.tnorm` compute
#' the same t-norms, but in an element-wise manner. I.e. the values
#' with indices 1 of all arguments are used to compute the t-norm, then the
#' second values (while recycling the vectors if they do not have the same
#' size) so that the result is a vector of values.
#'
#' `goedel.tconorm`, `lukas.tconorm`, `goguen.tconorm`, are
#' similar to the previously mentioned functions, exept that they compute
#' triangular conorms (t-conorms).  `pgoedel.tconorm`,
#' `plukas.tconorm`, and `pgoguen.tconorm` are their element-wise alternatives.
#'
#' `goedel.residuum`, `lukas.residuum`, and `goguen.residuum`
#' compute residua (i.e. implications) and `goedel.biresiduum`,
#' `lukas.biresiduum`, and `goguen.biresiduum` compute bi-residua. Residua and
#' bi-residua are computed in an element-wise manner, for each corresponding
#' pair of values in `x` and `y` arguments.
#'
#' `invol.neg` and `strict.neg` compute the involutive and strict
#' negation, respectively.
#'
#' Let \eqn{a}, \eqn{b} be values from the interval \eqn{[0, 1]}. The realized functions
#' can be defined as follows:
#' * Goedel t-norm: \eqn{min\{a, b\}};
#' * Goguen t-norm: \eqn{ab};
#' * Lukasiewicz t-norm: \eqn{max\{0, a+b-1\}};
#' * Goedel t-conorm: \eqn{max\{a, b\}};
#' * Goguen t-conorm: \eqn{a+b-ab};
#' * Lukasiewicz t-conorm: \eqn{min\{1, a+b\}};
#' * Goedel residuum (standard Goedel implication): \eqn{1} if \eqn{a \le b} and
#'   \eqn{b} otherwise;
#' * Goguen residuum (implication): \eqn{1} if \eqn{a \le b} and \eqn{b/a}
#'   otherwise;
#' * Lukasiewicz residuum (standard Lukasiewicz implication): \eqn{1} if
#'   \eqn{a \le b} and \eqn{1-a+b} otherwise;
#' * Involutive negation: \eqn{1-x};
#' * Strict negation: \eqn{1} if \eqn{x=0} and \eqn{0} otherwise.
#'
#' Bi-residuum \eqn{B} is derived from t-norm \eqn{T} and residuum \eqn{R}
#' as follows: \deqn{B(a, b) = T(R(a, b), R(b, a)).}
#'
#' The arguments have to be numbers from the interval \eqn{[0, 1]}. Values
#' outside that range cause an error. NaN values are treated as NAs.
#'
#' If some argument is NA or NaN, the result is NA.
#'
#' `algebra` returns a named list of functions that together form Goedel,
#' Goguen, or Lukasiewicz algebra:
#' * `"goedel"`: strict negation and Goedel t-norm, t-conorm, residuum, and bi-residuum;
#' * `"goguen"`: strict negation and Goguen t-norm, t-conorm, residuum, and bi-residuum;
#' * `"lukasiewicz"`: involutive negation and Lukasiewicz t-norm, t-conorm, residuum, and bi-residuum.
#' Moreover, `algebra` returns supremum and infimum functions computed as maximum and minimum,
#' respectively.
#'
#' `is.algebra` tests whether the given `a` argument is a valid
#' algebra, i.e. a list returned by the `algebra` function.
#'
#' @param ...  For t-norms and t-conorms, these arguments are numeric vectors
#' of values to compute t-norms or t-conorms from.  Values outside the
#' \eqn{[0,1]} interval cause an error. NA values are also permitted.
#'
#' For the `algebra()` function, these arguments are passed to the factory
#' functions that create the algebra. (Currently unused.)
#' @param x Numeric vector of values to compute a residuum or bi-residuum from.
#' Values outside the \eqn{[0,1]} interval cause an error. NA values are also
#' permitted.
#' @param y Numeric vector of values to compute a residuum or bi-residuum from.
#' Values outside the \eqn{[0,1]} interval cause an error. NA values are also
#' permitted.
#' @param name The name of the algebra to be created. Must be one of: "goedel",
#' "lukasiewicz", "goguen" (or an unambiguous abbreviation).
#' @param stdneg `TRUE` if to force the use of a "standard" negation (i.e.
#' involutive negation).  Otherwise, the appropriate negation is used in the
#' algebra (e.g. strict negation in Goedel and Goguen algebra and involutive
#' negation in Lukasiewicz algebra).
#' @param a An object to be checked if it is a valid algebra (i.e. a list
#' returned by the `algebra` function).
#' @return Functions for t-norms and t-conorms (such as `goedel.tnorm`)
#' return a numeric vector of size 1 that is the result of the appropriate
#' t-norm or t-conorm applied on all values of all arguments.
#'
#' Element-wise versions of t-norms and t-conorms (such as `pgoedel.tnorm`)
#' return a vector of results after applying the appropriate t-norm or t-conorm
#' on argument in an element-wise (i.e. by indices) way. The
#' resulting vector is of length of the longest argument (shorter arguments are
#' recycled).
#'
#' Residua and bi-residua functions return a numeric vector of length of the
#' longest argument (shorter argument is recycled).
#'
#' `strict.neg` and `invol.neg` compute negations and return a
#' numeric vector of the same size as the argument `x`.
#'
#' `algebra` returns a list of functions of the requested algebra:
#' `"n"` (negation), `"t"` (t-norm), `"pt"` (element-wise t-norm),
#' `"c"` (t-conorm), `"pc"` (element-wise t-conorm), `"r"` (residuum),
#' `"b"` (bi-residuum), `"s"` (supremum),
#' `"ps"` (element-wise supremum), `"i"` (infimum), and
#' `"pi"` (element-wise infimum).
#'
#' @author Michal Burda
#' @keywords models robust
#' @examples
#'     # direct and element-wise version of functions
#'     goedel.tnorm(c(0.3, 0.2, 0.5), c(0.8, 0.1, 0.5))  # 0.1
#'     pgoedel.tnorm(c(0.3, 0.2, 0.5), c(0.8, 0.1, 0.5)) # c(0.3, 0.1, 0.5)
#'
#'     # algebras
#'     x <- runif(10)
#'     y <- runif(10)
#'     a <- algebra('goedel')
#'     a$n(x)     # negation
#'     a$t(x, y)  # t-norm
#'     a$pt(x, y) # element-wise t-norm
#'     a$c(x, y)  # t-conorm
#'     a$pc(x, y) # element-wise t-conorm
#'     a$r(x, y)  # residuum
#'     a$b(x, y)  # bi-residuum
#'     a$s(x, y)  # supremum
#'     a$ps(x, y) # element-wise supremum
#'     a$i(x, y)  # infimum
#'     a$pi(x, y) # element-wise infimum
#'
#'     is.algebra(a) # TRUE
#' @export
algebra <- function(name, stdneg=FALSE, ...) {
    name <- match.arg(name, names(.algebras))
    res <- .algebras[[name]](...)
    if (stdneg) {
        res[['n']] <- invol.neg
    }
    return(res)
}


#' @rdname algebra
#' @export
is.algebra <- function(a) {
  return(is.list(a) &&
           is.function(a$n) &&
           is.function(a$t) &&
           is.function(a$pt) &&
           is.function(a$c) &&
           is.function(a$pc) &&
           is.function(a$r) &&
           is.function(a$b) &&
           is.function(a$s) &&
           is.function(a$ps) &&
           is.function(a$i) &&
           is.function(a$pi))
}


.elementWisely <- function(f) {
    function(...) {
        elts <- list(...)
        if (length(elts) <= 0L) {
            return(NULL)
        }
        vals <- lapply(elts, as.numeric)
        res <- do.call('mapply', c(list(f), vals))
        mostattributes(res) <- attributes(elts[[1L]])
        return(res)
    }
}


###########################################################
# t-norms

#' @rdname algebra
#' @export
goedel.tnorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_goedel_tnorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
lukas.tnorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_lukas_tnorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
goguen.tnorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_goguen_tnorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
pgoedel.tnorm <- .elementWisely(goedel.tnorm)

#' @rdname algebra
#' @export
plukas.tnorm <- .elementWisely(lukas.tnorm)

#' @rdname algebra
#' @export
pgoguen.tnorm <- .elementWisely(goguen.tnorm)


###########################################################
# t-conorms

#' @rdname algebra
#' @export
goedel.tconorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_goedel_tconorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
lukas.tconorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_lukas_tconorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
goguen.tconorm <- function(...) {
    vals <- as.numeric(c(...))
    .Call('_lfl_goguen_tconorm', vals, PACKAGE='lfl')
}

#' @rdname algebra
#' @export
pgoedel.tconorm <- .elementWisely(goedel.tconorm)

#' @rdname algebra
#' @export
plukas.tconorm <- .elementWisely(lukas.tconorm)

#' @rdname algebra
#' @export
pgoguen.tconorm <- .elementWisely(goguen.tconorm)


###########################################################
# residua

#' @rdname algebra
#' @export
goedel.residuum <- function(x, y) {
    .Call('_lfl_goedel_residuum', as.numeric(x), as.numeric(y), PACKAGE='lfl')
}

#' @rdname algebra
#' @export
lukas.residuum <- function(x, y) {
    .Call('_lfl_lukas_residuum', as.numeric(x), as.numeric(y), PACKAGE='lfl')
}

#' @rdname algebra
#' @export
goguen.residuum <- function(x, y) {
    .Call('_lfl_goguen_residuum', as.numeric(x), as.numeric(y), PACKAGE='lfl')
}


###########################################################
# bi-residua

#' @rdname algebra
#' @export
goedel.biresiduum <- function(x, y) {
    pgoedel.tnorm(goedel.residuum(x, y), goedel.residuum(y, x))
}

#' @rdname algebra
#' @export
lukas.biresiduum <- function(x, y) {
    plukas.tnorm(lukas.residuum(x, y), lukas.residuum(y, x))
}

#' @rdname algebra
#' @export
goguen.biresiduum <- function(x, y) {
    pgoguen.tnorm(goguen.residuum(x, y), goguen.residuum(y, x))
}


###########################################################
# negations

#' @rdname algebra
#' @export
invol.neg <- function(x) {
    vals <- as.numeric(c(x))
    res <- .Call('_lfl_invol_neg', vals, PACKAGE='lfl')
    mostattributes(res) <- attributes(x)
    return(res)
}

#' @rdname algebra
#' @export
strict.neg <- function(x) {
    vals <- as.numeric(c(x))
    res <- .Call('_lfl_strict_neg', vals, PACKAGE='lfl')
    mostattributes(res) <- attributes(x)
    return(res)
}


.tnorms <- list(goedel=goedel.tnorm,
                lukasiewicz=lukas.tnorm,
                goguen=goguen.tnorm)

.ptnorms <- list(goedel=pgoedel.tnorm,
                 lukasiewicz=plukas.tnorm,
                 goguen=pgoguen.tnorm)

.tconorms <- list(goedel=goedel.tconorm,
                  lukasiewicz=lukas.tconorm,
                  goguen=goguen.tconorm)

.ptconorms <- list(goedel=pgoedel.tconorm,
                  lukasiewicz=plukas.tconorm,
                  goguen=pgoguen.tconorm)

.residua <- list(goedel=goedel.residuum,
                 lukasiewicz=lukas.residuum,
                 goguen=goguen.residuum)

.biresidua <- list(goedel=goedel.biresiduum,
                   lukasiewicz=lukas.biresiduum,
                   goguen=goguen.biresiduum)

.negations <- list(involutive=invol.neg,
                   strict=strict.neg,
                   lukasiewicz=invol.neg,
                   goedel=strict.neg,
                   goguen=strict.neg)

.algebras <- list('goedel'=function(...) {
                        list(n=strict.neg,
                             t=goedel.tnorm,
                             pt=pgoedel.tnorm,
                             c=goedel.tconorm,
                             pc=pgoedel.tconorm,
                             r=goedel.residuum,
                             b=goedel.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm)
                  },
                  'lukasiewicz'=function(...) {
                        list(n=invol.neg,
                             t=lukas.tnorm,
                             pt=plukas.tnorm,
                             c=lukas.tconorm,
                             pc=plukas.tconorm,
                             r=lukas.residuum,
                             b=lukas.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm)
                  },
                  'goguen'=function(...) {
                        list(n=strict.neg,
                             t=goguen.tnorm,
                             pt=pgoguen.tnorm,
                             c=goguen.tconorm,
                             pc=pgoguen.tconorm,
                             r=goguen.residuum,
                             b=goguen.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm)
                   })
