#' Test whether \code{x} is a valid object of the \code{frbe} class
#' 
#' Test whether \code{x} has a valid format for the objects of the \code{frbe}
#' class.
#' 
#' This function tests wheter \code{x} inherits from \code{frbe} i.e. whether
#' it is a list with the following elements: \code{forecasts} data frame,
#' \code{features} data frame, \code{weights} vector, and \code{mean} vector.
#' 
#' @param x An object being tested.
#' @return TRUE if \code{x} is a valid \code{frbe} object and FALSE otherwise.
#' @author Michal Burda
#' @seealso \code{\link{frbe}}
#' @references Štěpnička, M., Burda, M., Štěpničková, L. Fuzzy Rule Base
#' Ensemble Generated from Data by Linguistic Associations Mining. FUZZY SET
#' SYST. 2015.
#' @keywords models robust
#' @export is.frbe
is.frbe <- function(x) {
    return(inherits(x, 'frbe') && 
           is.list(x) &&
           is.data.frame(x$forecasts) &&
           is.data.frame(x$features) &&
           is.vector(x$weights) &&
           is.vector(x$mean))
}
