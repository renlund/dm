##' @title extract variable information
##' @description get names, associated labels (or any specific attribute), and
##'     class of variables in a data set
##' @param df a data frame (or similar object)
##' @param attrib name of attribute (default 'label')
##' @param name optionally, the name of the data frame for output
##' @return a data frame
##' @export
db_info <- function(df, attrib = 'label', name = NULL){
    if(is.null(name)) name <- as.character(substitute(df))
    foo <- function(x){
        if(is.null(r <- attr(x, attrib)[1])) "" else r
    }
    bar <- function(x){
        if(is.null(r <- class(x)[1])) "" else r
    }
    r <- data.frame(
        name,
        names(df),
        unlist(lapply(df, foo)),
        unlist(lapply(df, bar)),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
    names(r) <- c('source', 'variable', attrib, 'class')
    r
}

##' find info in variables
##'
##' look for a search string in variable names and label (or other) attribute
##' @param pattern search string
##' @param ignore.case logical
##' @param attrib name of attribute to search in (default 'label')
##' @param ... arguments passed to \code{grepl}
##' @param verbose logical
##' @export
dm_find <- function(pattern, ignore.case = TRUE, attrib = 'label', ..., verbose = TRUE){
    R <- NULL
    for(K in ls(envir = .GlobalEnv)){
        if( "data.frame" %in% class(get(K, envir = .GlobalEnv))){
            di <- db_info(get(K, envir = .GlobalEnv), attrib = attrib, name = K)
            filter1 <- grepl(pattern = pattern, x = di[['variable']],
                             ignore.case = ignore.case, ... )
            filter2 <- grepl(pattern = pattern, x = di[[attrib]],
                             ignore.case = ignore.case, ... )
            tmp <- subset(di, subset =  filter1 | filter2)
            R <- if(is.null(R)) tmp else rbind(R, tmp)
        } else next
    }
    if(nrow(R) == 0){
        if(verbose) message("dm_find found nothing")
        invisible(as.data.frame(NULL))
    } else {
        if(verbose) message("dm_find found:")
        R
    }
}

if(FALSE){

    AFG <- data.frame(
        arg = 1,
        boo = "a"
    )
    attr(AFG$arg, "label") <- "a label of sorts"
    attr(AFG$boo, "info")  <- "an INFO of sorts"
    BRT <- data.frame(
        farg = 2,
        bobb = "b"
    )
    attr(BRT$bobb, "label") <- "another label of sorts"
    attr(BRT$farg, "label") <- "lots if label"
    attr(BRT$farg, "info") <- "lots if info"

    db_info(AFG)
    db_info(AFG, attrib = 'info')
    db_info(BRT)
    db_info(BRT, attrib = 'info')
    dm_find("or")
    dm_find("or", attrib = "info")
    dm_find("^foo")
    dm_find("o")

    rm(AFG, BRT)

}
