##' clustering
##'
##' create cluster variable indicated by other variable
##' @param x variable from which to create cluster
##' @param by name of method
##' @export
cluster.by <- function(x, by){
    if(length(x) == 0 | is.null(x)){
        warning("'x' is too little (NULL or length 0)")
        return(invisible(NULL))
    }
    .required_properties(by, class = "character", length = 1)
    if(by == "incl.next"){
        cluster.by.incl.next(incl.next = x)
    } else {
        s <- paste0("by = '", by, "' currently not supported")
        warning(s)
        invisible(NULL)
    }
}

##' @describeIn cluster.by cluster by 'include.next' indicator
##' @param incl.next a logical variable that for each line include wether the
##'     next line should belong to the same cluster as the current line
##' @export
cluster.by.incl.next <- function(incl.next){
    n <- length(incl.next)
    c(1, 1+cumsum(!incl.next[-n]))
}
