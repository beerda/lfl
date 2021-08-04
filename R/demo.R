#' @export
#' @import shiny
#' @import ggplot2
demo <- function(name=c('lcut')) {
    name <- match.arg(name)
    if (name == 'lcut') {
        .demoLcut()
    } else {
        stop('Unknown demo:', name)
    }
}
