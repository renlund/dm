##' @title derivation documentation
##' @description save documentation on derived variables
##' @param name name of variable assigned
##' @param expr the expression for creating variable
##' @param dmd the documentation
##' @param label a label to assign to the new variable
##' @export
dmd <- function(name, expr, dmd = NULL, label = NULL, overwrite = FALSE){
    .required_properties(name, class = 'character', length = 1)
    .required_properties(dmd, class = c('character', 'NULL'), length = 0:1)
    v <- list(
        'dmd' = if(is.null(dmd)) '' else dmd,
        'label' = if(is.null(label)) '' else label,
        'expr' = as.character(as.expression(substitute(expr)))
    )
    dm_derive_set(name = name, value = v, overwrite = overwrite)
    ## print(v) ## for testing
    if(is.null(label)) expr else structure(expr, label = label)
}

##' @title print 'dm_derive' object
##' @description prints a data frame version of selected info in a 'dm_derive'
##'     object or returns that data frame
##' @param x a 'dm_derive' object
##' @param ... arguments passed to print.data.frame
##' @param print if \code{FALSE} then a data.frame is returned
##' @export
print.dm_derive <- function(x, ..., print = TRUE){
    if(length(x) == 0){
        message("no derivation documentation")
        return(invisible(NULL))
    }
    X <- data.frame(
        variable = names(x),
        comment = unlist(lapply(x, function(z) z$dmd)),
        ## label = unlist(lapply(x, function(z) z$label)),
        ## expr = unlist(lapply(x, function(z) z$expr)),
        stringsAsFactors = FALSE, row.names = NULL
    )
    if(print){
        print(X)
        invisible(NULL)
    } else X
}

if(FALSE){

    n <- 6
    ## test within data frame
    d <- data.frame(
        rowid = dmd("rowid", 1:n,
                    dmd = "Row identification"),
        gr = rep(LETTERS[1:2], each = n/2),
        x = n:1
    )

    ## test within 'within'
    d <- within(
        d,
        expr = y <- dmd("y", x^2, dmd = 'x squared')
    )

    ## test within `<-`
    d$z <- dmd("z", expr = d$y - d$x, dmd = "y squared")

    ## test within lapply
    foo <- function(d.gr){
        d.gr$score <- dmd('score', d.gr$z + 1:nrow(d.gr),
                          dmd = "z + rownumber in subgroup by 'gr'")
        d.gr
    }
    d <- do.call(rbind, lapply(split(d, f = d$gr), FUN = foo))

    ## test within dplyr
    ## require(dplyr)
    ## d <- d %>%
    ##     mutate(foo = dmd('foo', 1, dmd = "a constant = 1")) %>%
    ##     group_by(rowid) %>%
    ##     mutate(id.n = dmd('id.n', n(), dmd = 'no. of occurences of rowid')) %>%
    ##     ungroup()

}
