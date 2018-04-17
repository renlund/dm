##' remove unwanted attributes from data frame or similar object
##'
##' @title remove attributes
##' @param x data frame or similar
##' @param attr name of one or more attribute to nullify
##' @export
dm_kill_attr <- function(x, attr){
    .required_properties(x = x, class = 'data.frame')
    .required_properties(x = attr, class = 'character')
    foo <- function(z){
        for(A in attr){
            attr(z, which = A) <- NULL
        }
        z
    }
    x[] <- lapply(x, foo)
    x
}
