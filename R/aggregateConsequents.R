#' Aggregation of fired consequents into a resulting fuzzy set
#'
#' Take a character vector of consequent names, a numeric vector representing
#' the degree of consequents' firing and a matrix that models fuzzy sets
#' corresponding to the consequent names, and perform an aggregation of the
#' consequents into a resulting fuzzy set.
#'
#' This function is typically used within an inference mechanism, after a set of
#' firing rules is determined and membership degrees of their antecedents are
#' computed, to combine the consequents of the firing rules into a resulting
#' fuzzy set. The result of this function is then typically defuzzified
#' (see [defuzz()]) to  obtain a crisp result of the inference.
#'
#' Function assumes a set of rules with antecedents firing at degrees given in
#' `degrees` and with consequents in `conseq`. The meaning of the consequents is
#' modeled with fuzzy sets whose membership degree values are captured in the
#' `partition` matrix.
#'
#' With default values of `firing` and `aggreg` parameters, the function
#' computes a fuzzy set that results from a conjunction (Goedel minimum t-norm)
#' of all provided implicative (Lukasiewicz residuum) rules.
#'
#' In detail, the function first computes the fuzzy set of each fired consequent
#' by calling `part\[i\] <- firing(degrees\[i\], partition\[, conseq\[i\]\])` for each
#' `i`-th consequent and the results are aggregated using the `aggreg`
#' parameter: `aggreg(part\[1\], part\[2\], ...)`. In order to aggregate consequents
#' in a Mamdani-Assilian's fashion, set `firing` to [pgoedel.tnorm()] and `aggreg`
#' to [pgoedel.tconorm()].
#'
#' @param conseq A character vector of consequents. Each value in the vector
#'   must correspond to a name of some column of the `partition` matrix.
#'   The length of this vector must be the same as of the `degrees`
#'   argument.
#' @param degrees A numeric vector of membership degrees at which the
#'   corresponding consequents (see the `conseq` argument) are fired.
#' @param partition A matrix of membership degrees that describes the meaning of
#'   the consequents in vector `conseq`: each column of the matrix
#'   corresponds to a fuzzy set that models a single consequent (of a name given
#'   by column names of the matrix), each row corresponds to a single crisp
#'   value (which is not important for this function), hence each cell
#'   corresponds to a membership degree in which the crisp value is a member of
#'   a fuzzy set modelling the consequent.  Each consequent in `conseq`
#'   must correspond to some column of this matrix. Such matrix may be created
#'   e.g. by using [fcut()] or [lcut()] functions.
#' @param firing A two-argument function used to compute the resulting truth value of the consequent.
#'   Function is evaluated for each consequent in `conseq`, with corresponding `degrees` value
#'   as the first argument and corresponding truth-value of the consequent (from `partition`)
#'   as the second argument. In default, the Lukasiewicz residuum ([lukas.residuum()]) is
#'   evaluated that way.
#' @param aggreg An aggregation function to be used to combine fuzzy sets resulting from firing
#'   the consequents with the `firing` function. The function should accept multiple
#'   numeric vectors of membership degrees as its arguments.
#'   In default, the Goedel t-norm ([pgoedel.tnorm()]) is evaluated.
#' @return A vector of membership degrees of fuzzy set elements that correspond
#'   to rows in the `partition` matrix. If empty vector of consequents is
#'   provided, vector of 1's is returned. The length of the resulting vector
#'   equals to the number of rows of the `partition` matrix.
#'
#' @author Michal Burda
#' @seealso [fire()], [perceive()], [defuzz()], [fcut()], [lcut()]
#' @keywords models robust
#' @examples
#'     # create a partition matrix
#'     partition <- matrix(c(0:10/10, 10:0/10, rep(0, 5),
#'                           rep(0, 5), 0:10/10, 10:0/10,
#'                           0:12/12, 1, 12:0/12),
#'                         byrow=FALSE,
#'                         ncol=3)
#'     colnames(partition) <- c('a', 'b', 'c')
#'
#'     # the result of aggregation is equal to:
#'     # pmin(1, partition[, 1] + (1 - 0.5), partition[, 2] + (1 - 0.8))
#'     aggregateConsequents(c('a', 'b'), c(0.5, 0.8), partition)
#'
#' @export aggregateConsequents
aggregateConsequents <- function(conseq,
                                 degrees,
                                 partition,
                                 firing=lukas.residuum,
                                 aggreg=pgoedel.tnorm) {
    .mustBeCharacterVector(conseq)
    .mustBeNumericVector(degrees)
    .mustBe(length(conseq) == length(degrees),
            "The length of 'conseq' and 'degrees' must be the same")
    .mustBeNumericMatrix(partition)
    .mustBe(nrow(partition) > 0 && ncol(partition) > 0,
            "'partition' must not be empty matrix")
    .mustBe(!is.null(colnames(partition)) && is.character(colnames(partition)),
            "'partition' must have colnames")

    uniqConseq <- unique(conseq)
    .mustBe(length(intersect(uniqConseq, colnames(partition))) == length(uniqConseq),
            "Not all consequents are present in 'partition'")

    if (length(conseq) == 0) {
      return(rep(1, nrow(partition)))
    }

    res <- lapply(seq_along(conseq), function(i) {
      firing(degrees[i], partition[, conseq[i]])
    })
    return(do.call(aggreg, res))
}
