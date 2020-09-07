##' contrast to data frame
##'
##' turn a contrast.rms object into a data frame
##' @param c a contrast rms object
##' @return a data.frame
##' @export
contrast_rms2df <- function(c){
    rm <- c("var", "df.residual", "X", "cnames", "nvary",
            "conf.type", "conf.int", "redundant")
    for(j in names(c)){
        if(is.null(c[j]) || is.null(c[[j]])) rm <- c(rm, j)
    }
    c <- c[setdiff(names(c), rm)]
    ## for(k in rm) c[k] <- NULL
    class(c) <- "list"
    as.data.frame(c)
}

##' @describeIn  contrast_rms2df mimick \code{broom::tidy} behaviour  as applied
##'     to a contrast.rms object
##' @export
contrast_rms2tidy <- function(c){
    x <- contrast_rms2df(c)
    names(x)[names(x) == "Contrast"] <- "estimate"
    names(x)[names(x) == "SE"] <- "std.error"
    names(x)[names(x) == "Z"] <- "statistic"
    names(x)[names(x) == "Lower"] <- "conf.low"
    names(x)[names(x) == "Upper"] <- "conf.high"
    names(x)[names(x) == "Pvalue"] <- "p.value"
    x
}
