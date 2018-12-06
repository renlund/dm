##' insert linebreaks to a character vector
##'
##' Given a maximum number of character 'n', insert a linebreak before this number
##'     is reached. N.B. Words longer than 'n' will however NOT be abbreviated.
##' @title add linebreaks
##' @param s a character string
##' @param n a maximum number of characters
##' @param linebreak defaults to newline \code{"\n"}
##' @param splitby defaults to space \code{" "}
##' @return character vector
##' @examples
##'     s <- paste0("En very long and perhaps supercalifragilisticexpialidocious",
##'                 " sentence in desparate need of linebreaks")
##'     cat(insert_linebreak(s, n = 20))
##'     cat(insert_linebreak(s, n = 30))
##'     cat(insert_linebreak(s, n = 7))
##'     cat(insert_linebreak(s, n = 100))
##' @export
insert_linebreak <- function(s, n, linebreak = "\n", splitby = " "){
    .required_properties(x = s, class = "character", length = 1)
    .required_properties(x = n, class = "numeric", length = 1)
    if(n < 1) stop("need n > 0")
    x <- unlist(strsplit(s, splitby))
    xn <- unlist(lapply(x, nchar))
    cxn <- cumsum(xn)
    is <- NULL
    dummy <- 0
    while(length(cxn) > 0 & dummy < 100000){
        dummy <- dummy + 1
        if(cxn[1] <= n){
            m <- max(cxn[cxn <= n])
            i <- which(cxn == m)
        } else {
            m <- cxn[1]
            i <- 1
        }
        cxn <- (cxn - m)[cxn-m > 0]
        is <- c(is, i)
    }
    if(length(is) > 1){
        id <- 1
        S <- NULL
        for(i in is){ ## i = is[1]
            S <- c(S, paste0(x[id:(id+i-1)], collapse = splitby))
            id <- id + i
        }
        paste0(S, collapse = linebreak)
    } else {
        s
    }
}
