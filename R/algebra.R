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
#' similar to the previously mentioned functions, except that they compute
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
#' * Goedel t-norm: \eqn{min{a, b}};
#' * Goguen t-norm: \eqn{ab};
#' * Lukasiewicz t-norm: \eqn{max{0, a+b-1}};
#' * Goedel t-conorm: \eqn{max{a, b}};
#' * Goguen t-conorm: \eqn{a+b-ab};
#' * Lukasiewicz t-conorm: \eqn{min{1, a+b}};
#' * Goedel residuum (standard Goedel implication): \eqn{1} if \eqn{a \le b} and
#'   \eqn{b} otherwise;
#' * Goguen residuum (implication): \eqn{1} if \eqn{a \le b} and \eqn{b/a}
#'   otherwise;
#' * Lukasiewicz residuum (standard Lukasiewicz implication): \eqn{1} if
#'   \eqn{a \le b} and \eqn{1-a+b} otherwise;
#' * Involutive negation: \eqn{1-x};
#' * Strict negation: \eqn{1} if \eqn{x=0} and \eqn{0} otherwise.
#'
#' Bi-residuum \eqn{B} is derived from residuum \eqn{R}
#' as follows: \deqn{B(a, b) = inf(R(a, b), R(b, a)),}
#' where \eqn{inf} is the operation of infimum, which for all three algebras
#' corresponds to the \eqn{min} operation.
#'
#' The arguments have to be numbers from the interval \eqn{[0, 1]}. Values
#' outside that range cause an error. NaN values are treated as NAs.
#'
#' If some argument is NA or NaN, the result is NA. For other handling of missing values,
#' see [algebraNA].
#'
#' Selection of a t-norm may serve as a basis for definition of other operations.
#' From the t-norm, the operation of a residual implication may be defined, which
#' in turn allows the definition of a residual negation. If the residual negation
#' is not involutive, the involutive negation is often added as a new operation
#' and together with the t-norm can be used to define the t-conorm. Therefore,
#' the `algebra` function returns a named list of operations derived from the selected
#' Goedel, Goguen, or Lukasiewicz t-norm. Concretely:
#' * `algebra("goedel")`: returns the strict negation as the residual negation,
#'    the involutive negation, and also the Goedel t-norm, t-conorm, residuum, and bi-residuum;
#' * `algebra("goguen")`: returns the strict negation as the residual negation,
#'    the involutive negation, and also the Goguen t-norm, t-conorm, residuum, and bi-residuum;
#' * `algebra("lukasiewicz")`: returns involutive negation as both residual and involutive
#'    negation, and also the Lukasiewicz t-norm, t-conorm, residuum, and bi-residuum.
#'
#' Moreover, `algebra` returns the supremum and infimum functions computed as maximum and minimum,
#' respectively.
#'
#' `is.algebra` tests whether the given `a` argument is a valid
#' algebra, i.e. a list returned by the `algebra` function.
#'
#' @param name The name of the algebra to be created. Must be one of: "goedel",
#' "lukasiewicz", "goguen" (or an unambiguous abbreviation).
#' @param stdneg (Deprecated.) `TRUE` if to force the use of a "standard" negation (i.e.
#' involutive negation).  Otherwise, the appropriate negation is used in the
#' algebra (e.g. strict negation in Goedel and Goguen algebra and involutive
#' negation in Lukasiewicz algebra).
#' @param ...  For t-norms and t-conorms, these arguments are numeric vectors
#' of values to compute t-norms or t-conorms from.  Values outside the
#' \eqn{[0,1]} interval cause an error. NA values are also permitted.
#'
#' For the `algebra()` function, these arguments are passed to the factory
#' functions that create the algebra. (Currently unused.)
#' @param a An object to be checked if it is a valid algebra (i.e. a list
#' returned by the `algebra` function).
#' @param x Numeric vector of values to compute a residuum or bi-residuum from.
#' Values outside the \eqn{[0,1]} interval cause an error. NA values are also
#' permitted.
#' @param y Numeric vector of values to compute a residuum or bi-residuum from.
#' Values outside the \eqn{[0,1]} interval cause an error. NA values are also
#' permitted.
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
#' `"n"` (residual negation), `"ni"` (involutive negation), `"t"` (t-norm),
#' `"pt"` (element-wise t-norm),
#' `"c"` (t-conorm), `"pc"` (element-wise t-conorm), `"r"` (residuum),
#' `"b"` (bi-residuum), `"s"` (supremum),
#' `"ps"` (element-wise supremum), `"i"` (infimum), and
#' `"pi"` (element-wise infimum).
#'
#' For Lukasiewicz algebra, the elements `"n"` and `"ni"` are the same, i.e.
#' the `invol.neg` function. For Goedel and Goguen algebra, `"n"` (the residual
#' negation) equals `strict.neg` and `"ni"` (the involutive negation) equals
#' `invol.neg`.
#'
#' `"s"`, `"ps"`, `"i"`, `"pi"` are the same for each type of algebra:
#' `goedel.conorm`, `pgoedel.conorm`, `goedel.tnorm`, and `pgoedel.tnorm`.
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
#'     a$n(x)     # residual negation
#'     a$ni(x)    # involutive negation
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
    if (stdneg) {
      .Deprecated('algebra',
                  msg=paste('The "stdneg" argument is deprecated. If you need',
                            'the involutive negation, use the "ni" function',
                            'returned in list by the algebra() function.'))
    }
    name <- match.arg(name, names(.algebras))
    res <- .algebras[[name]](...)
    if (stdneg) {
        res[['n']] <- invol.neg
    }
    class(res) <- c('algebra', 'list')
    res$algebratype <- name
    return(res)
}


#' Print an instance of the [algebra()] S3 class in a human readable form.
#'
#' @param x An instance of the [algebra()] S3 class
#' @param ...  Unused.
#' @return None.
#' @author Michal Burda
#' @seealso [algebra()]
#' @keywords models robust
#' @export
#' @importFrom utils str
print.algebra <- function(x, ...) {
  cat('Algebra:', paste0(x$algebratype, collapse='/'), '\n')
  x$algebratype <- NULL
  str(x, give.attr=FALSE, no.list=TRUE)
}


#' @rdname algebra
#' @export
is.algebra <- function(a) {
  return(is.list(a) &&
           inherits(a, 'algebra') &&
           is.function(a$n) &&
           is.function(a$ni) &&
           is.function(a$t) &&
           is.function(a$pt) &&
           is.function(a$c) &&
           is.function(a$pc) &&
           is.function(a$r) &&
           is.function(a$b) &&
           is.function(a$s) &&
           is.function(a$ps) &&
           is.function(a$i) &&
           is.function(a$pi) &&
           is.function(a$order))
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
pgoedel.tnorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_pgoedel_tnorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}

#' @rdname algebra
#' @export
plukas.tnorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_plukas_tnorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}


#' @rdname algebra
#' @export
pgoguen.tnorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_pgoguen_tnorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}


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
pgoedel.tconorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_pgoedel_tconorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}

#' @rdname algebra
#' @export
plukas.tconorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_plukas_tconorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}


#' @rdname algebra
#' @export
pgoguen.tconorm <- function(...) {
  elts <- list(...)
  if (length(elts) <= 0L) {
    return(NULL);
  }
  vals <- lapply(elts, as.numeric)
  res <- .Call('_lfl_pgoguen_tconorm', vals, PACKAGE='lfl')
  mostattributes(res) <- attributes(elts[[1L]])
  res
}


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
    pgoedel.tnorm(lukas.residuum(x, y), lukas.residuum(y, x))
}

#' @rdname algebra
#' @export
goguen.biresiduum <- function(x, y) {
    pgoedel.tnorm(goguen.residuum(x, y), goguen.residuum(y, x))
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

.defaultOrder <- function(x, decreasing=FALSE) {
  order(x, decreasing=decreasing)
}

.algebras <- list('goedel'=function(...) {
                        list(n=strict.neg,
                             ni=invol.neg,
                             t=goedel.tnorm,
                             pt=pgoedel.tnorm,
                             c=goedel.tconorm,
                             pc=pgoedel.tconorm,
                             r=goedel.residuum,
                             b=goedel.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm,
                             order=.defaultOrder)
                  },
                  'lukasiewicz'=function(...) {
                        list(n=invol.neg,
                             ni=invol.neg,
                             t=lukas.tnorm,
                             pt=plukas.tnorm,
                             c=lukas.tconorm,
                             pc=plukas.tconorm,
                             r=lukas.residuum,
                             b=lukas.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm,
                             order=.defaultOrder)
                  },
                  'goguen'=function(...) {
                        list(n=strict.neg,
                             ni=invol.neg,
                             t=goguen.tnorm,
                             pt=pgoguen.tnorm,
                             c=goguen.tconorm,
                             pc=pgoguen.tconorm,
                             r=goguen.residuum,
                             b=goguen.biresiduum,
                             i=goedel.tnorm,
                             pi=pgoedel.tnorm,
                             s=goedel.tconorm,
                             ps=pgoedel.tconorm,
                             order=.defaultOrder)
                   })
