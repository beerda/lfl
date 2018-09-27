#' Print an instance of the [frbe()] class
#'
#' Format an object of the [frbe()] class into human readable form
#' and print it to the output.
#'
#' Format an object of the [frbe()] class into human readable form
#' and print it to the output.
#'
#' @param x An instance of [frbe()] class
#' @param ... Unused.
#' @return None.
#' @author Michal Burda
#' @seealso [frbe()]
#' @references Štěpnička, M., Burda, M., Štěpničková, L. Fuzzy Rule Base
#' Ensemble Generated from Data by Linguistic Associations Mining. FUZZY SET
#' SYST. 2015.
#' @keywords models robust
#' @examples
#'
#'   # prepare data (from the forecast package)
#'   library(forecast)
#'   horizon <- 10
#'   train <- wineind[-1 * (length(wineind)-horizon+1):length(wineind)]
#'   test <- wineind[(length(wineind)-horizon+1):length(wineind)]
#'   f <- frbe(ts(train, frequency=frequency(wineind)), h=horizon)
#'   print(f)
#'   print(test)
#'
#' @export
print.frbe <- function(x, ...) {
    .mustBe(is.frbe(x), "'x' is not a valid 'frbe' object")
    xx <- x
    class(xx) <- NULL
    print(xx, ...)
}

