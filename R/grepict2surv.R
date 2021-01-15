##' @title turn grepict alias into survival data
##' @description make a grepict long format into wide survival data variables
##' @param g returned from grepict (long format, stacked)
##' @param prefix prefix for time and event variable
##' @return a data frame
##' @export
grepict2surv <- function(g, prefix = c('time' = 't.', 'event' = 'ev.')){
    .required_properties(x = prefix, class = 'character',
                         length = 2, nm = 'prefix')
    .required_data_names(data.names = names(g),
                         required = c('id', 'begin', 'time', 'event', 'first.id'))
    G <- subset(g, g$first.id == 1)
    AL <- unique(G$alias)
    R <- NULL
    if(length(AL) != 0){
        for(i in seq_along(AL)){
            nm <- AL[i]
            d <- subset(G, G$alias == nm, select = c('id', 'begin', 'time', 'event'))
            names(d) <- c('id', 'begin', paste0(prefix, nm))
            R <- if(is.null(R)) d else merge(R, d, by = c('id', 'begin'))
        }
        rownames(R) <- NULL
        R
    } else {
        message("that's odd, no values in 'alias")
        as.data.frame(R)
    }
}
