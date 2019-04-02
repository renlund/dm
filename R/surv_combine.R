##' combine Surv variables
##'
##' combine two or more Surv objects into a combined Surv object
##' @title Surv combine
##' @param ... Surv objects
##' @param  comb.type how the Surv  objects are combined. 'EELC'  (Early Events,
##'     Late Censored) thinks of the Surv  objects as informative of each other,
##'     e.g. if the first observation in the  first row is censored at 1 for the
##'     first variable  and at 2 for  the second variable, then  the combination
##'     will pick the  second one. More specifically, it will  pick the earliest
##'     event (as an event) if there is at least one event, and it will pick the
##'     latest censored (as censored) if  there are no events. 'earliest' thinks
##'     of the Surv objects  as being very distinct, so it  will simply pick the
##'     earliest time (as event or censored).
##' @param surv.output  return a Surv object (default \code{TRUE}),  else a data
##'     frame with variables \code{time} and \code{event}.
##' @export
surv_combiner <- function(..., comb.type = 'EELC', surv.output = TRUE){
    if(!comb.type %in% c('EELC', 'earliest')){
        s <- paste0("'comb.type' must be one of\n",
               " - 'EELC' (Early Events, Late Censored) which chooses the ",
               "first timepoint among events (if any) as an event and the ",
               "latest timepoint among non-events (if no events) as a ",
               "non-event , or\n",
               " - 'earliest' which always chooses the earliest timepoint ",
               "and the corresponding event/non-event." )
        stop(s)
    }
    l <- eval(list(...))
    if(!setequal(unique(unlist(lapply(l, class))), "Surv")){
        warning("not all arguments are of class 'Surv' only")
    }
    if(length(n <- unlist(unique(lapply(l, function(x) dim(x)[1])))) != 1){
        stop("not all arguments of the same length")
    }
    t <- do.call(cbind, lapply(l, function(x) x[,1]))
    e <- do.call(cbind, lapply(l, function(x) x[,2]))
    tt <- ee <- rep(NA, n)
    if(comb.type == 'earliest'){
        i <- apply(t, MARGIN = 1, FUN = which.min)
        for(k in 1:n){
            tt[k] <- t[k, i[k]]
            ee[k] <- e[k, i[k]]
        }
    } else if(comb.type == 'EELC'){
        ev <- which(rowSums(e) > 0)
        nev <- which(rowSums(e) == 0)
        tInf <- t; tInf[e == 0] <- Inf
        tt[ev] <- apply(tInf[ev, ], MARGIN = 1, FUN = min)
        tt[nev] <- apply(t[nev, ], MARGIN = 1, FUN = max)
        ee[ev] <- 1
        ee[nev] <- 0
    } else {
        stop("If you see this error message, then I can't program.")
    }
    if(surv.output){
        survival::Surv(time = tt, event = ee)
    } else {
        data.frame(time = tt, event = ee)
    }
}

if(FALSE){
    e1 <- c(0, 0, 0, 1, 1, 1)
    t1 <- c(2, 2, 2, 2, 2, 2)
    s1 <- Surv(time = t1, event = e1)
    e2 <- c(0, 0, 1, 1, 1, 0)
    t2 <- c(1, 3, 3, 1, 3, 3)
    s2 <- Surv(time = t2, event = e2)
    e3 <- c(0,   0,   1,   1,   1, 0)
    t3 <- c(1.5, 3.5, 2.5, 1.5, 3.5, 2.5)
    s3 <- Surv(time = t3, event = e3)
    ## tmp <- function(...) eval(list(...))
    ## l <- tmp(s1, s2)
    data.frame(
        s1 = s1,
        s2 = s2,
        s = surv_combiner(s1, s2, comb.type = 'EELC'),
        se = surv_combiner(s1, s2, comb.type = 'earliest')
    )
    data.frame(
        s1 = s1,
        s2 = s2,
        s3 = s3,
        s = surv_combiner(s1, s2, s3, comb.type = 'EELC'),
        se = surv_combiner(s1, s2, s3, comb.type = 'earliest')
    )
    surv_combiner(s1, s2, surv.output = FALSE)
    rm(e1,e2,t1,t2,s1,s2,tmp,l)
}
