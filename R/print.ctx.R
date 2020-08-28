.printCtx <- function(name) {
    function(x, ...) {
        clazz <- class(x)[1]
        cat('Linguistic context: ', name, ' (', clazz, ')', '\n', sep='')
        class(x) <- NULL
        print(x)
    }
}

#' Print the linguistic context
#'
#' Format an object of the [ctx3()], [ctx5()], [ctx3bilat()] and the [ctx5bilat()]
#' class into human readable form and print it to the output.
#'
#' @param x A linguistic context to be printed
#' @param ... Unused.
#' @return Nothing.
#' @author Michal Burda
#' @seealso [ctx3()], [ctx5()], [ctx3bilat()], [ctx5bilat()], [minmax()]
#' @keywords models robust
#' @examples
#'
#'   context <- ctx3()
#'   print(context)
#'
#' @rdname print.ctx
#' @export
print.ctx3 <- .printCtx('unilateral trichotomy')

#' @rdname print.ctx
#' @export
print.ctx5 <- .printCtx('unilateral pentachotomy')

#' @rdname print.ctx
#' @export
print.ctx3bilat <- .printCtx('bilateral trichotomy')

#' @rdname print.ctx
#' @export
print.ctx5bilat <- .printCtx('bilateral pentachotomy')
