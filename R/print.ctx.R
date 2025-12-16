#######################################################################
# lfl: Linguistic Fuzzy Logic
# Copyright (C) 2025 Michal Burda
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.
#######################################################################


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
