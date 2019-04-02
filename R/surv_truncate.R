##' truncate Surv object
##'
##' censor at 'ttime' for all later times
##' @title Surv truncate
##' @param x Surv object
##' @param ttime truncating/censoring time
##' @param surv.output  return a Surv object (default \code{TRUE}),  else a data
##'     frame with variables \code{time} and \code{event}.
##' @export
surv_truncate <- function(x, ttime = NULL, surv.output = TRUE){
    if(is.null(ttime)) stop("ttime must be given")
    if(class(x) != "Surv") stop("x not of class 'Surv'")
    t <- x[,1]
    e <- x[,2]
    if(all(t < ttime)){
        message("all times already smaller than ttime")
    }
    tt <- ifelse(t <= ttime, t, ttime)
    ee <- ifelse(t <= ttime, e, 0)
    if(surv.output){
        survival::Surv(time = tt, event = ee)
    } else {
        data.frame(time = tt, event = ee)
    }
}

if(FALSE){
    t1 <- c(1, 1, 3, 3)
    e1 <- c(1, 0, 1, 0)
    surv_truncate(survival::Surv(t1,e1), ttime = 2)
}
