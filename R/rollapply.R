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


.rollapply <- function(x, window, f, ...) {
    if (length(x) < window) {
        return(NULL)
    }

    res <- sapply(seq_len(length(x) - window + 1), function(i) {
        ii <- seq(from = i, to = i + window - 1)
        f(x[ii], ...)
    })

    if (is.matrix(res)) {
        res <- t(res)
    }

    res
}
