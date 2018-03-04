#' @title 'grep' and return hits
#' @description This is a wrapper for \code{\link{grep}} to return not only the indexes
#' of the hits (optional), but the hits themselves. (This is mainly a helper function
#' for \code{find_var} but can at times be useful.)
#' @param pattern pattern to look for
#' @param x character string to inspect
#' @param index logical; whether to also return indexes of hits (defaul FALSE)
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' grepr(pattern="a", x=names(datasets::mtcars))
#' @seealso \code{\link{grep}}, \code{\link{grepl}}
#' @export
grepr <- function(pattern, x, index=FALSE, ...) {
    g <- grep(pattern, x, ...)
    m <- x[g]
    if(index) names(m) <- g
    if(length(m)==0) NULL else m
}
