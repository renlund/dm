##' insert linebreaks to a character vector
##'
##' Given a maximum number of character 'n', insert a linebreak before this number
##'     is reached. N.B. Words longer than 'n' will however NOT be abbreviated.
##' @title add linebreaks
##' @param s a character string
##' @param n a maximum number of characters
##' @param linebreak defaults to newline \code{"\n"}
##' @param splitby defaults to space \code{" "}
##' @param max.it maximum number of iteration of a while loop
##' @return character vector
##' @examples
##'     s <- paste0("En very long and perhaps supercalifragilisticexpialidocious",
##'                 " sentence in desparate need of linebreaks")
##'     cat(insert_linebreak(s, n = 20))
##'     cat(insert_linebreak(s, n = 30))
##'     cat(insert_linebreak(s, n = 7))
##'     cat(insert_linebreak(s, n = 100))
##' @export
insert_linebreak <- function(s, n, linebreak = "\n", splitby = " ", max.it = 10000){
    .required_properties(x = s, class = "character")
    .required_properties(x = n, class = "numeric", length = 1)
    if(n < 1) stop("need n > 0")
    ORIGINAL <- s
    R <- rep(NA_character_, length(ORIGINAL))
    for(index in seq_along(s)){ ## index = 1
        s <- ORIGINAL[index]
        x <- unlist(strsplit(s, splitby))
        xn <- unlist(lapply(x, nchar))
        cxn <- cumsum(xn)
        is <- NULL
        dummy <- 0
        while(length(cxn) > 0 & dummy < max.it){
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
        R[index] <- if(length(is) > 1){
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
    R
}

if(FALSE){
    s = c("Suslf nk fsdnjk fnsdnj  sdjkfnk sjdnf",
          "asjkdb  abhasdjb jas asbh adsbh jad",
          "sd IUAHFIASHF IAH ASFK ndf ksd")
    insert_linebreak(s, n = 12)
}

##' @describeIn insert_linebreak insert linebreak in factor
##' @param x something interpretable as factor
##' @param ... passed to \code{insert_linebreak}
##' @export
factor_il <- function(x, ...){
    fac_x <- as.factor(x)
    lev <- levels(fac_x)
    lev_il <- insert_linebreak(lev, ...)
    x_il <- insert_linebreak(as.character(fac_x), ...)
    factor(x_il, lev_il)
}

if(FALSE){
    a <- "a very long sentence indeed, very looong"
    b <- "random words banana horse keyboard window exotic fruit"
    factor_il(x = c(a,b,b,a), n = 18)
    factor_il(factor(c(a,b,b,a)), n = 18)
}
