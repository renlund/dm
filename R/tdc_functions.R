##' participants over time
##'
##' \code{survival::tmerge} creates time periods (per patient) starting at
##' 'tstart' and ending at 'tstop'. This function counts the number of
##' individuals in the study at each time point of interest (set of all distinct
##' time points starting or ending an interval). For each such point, a patient
##' is in the study at T if they have a time period starting at, or before, T,
##' AND the corresponding end strictly after T.
##' @param x data frame with time periods starting at tstart, ending at tstop.
##' @param tstart name of variable acting as tstart
##' @param tstop name of variable acting as tstop
##' @return data frame with variables 'time' and 'in.study' (count)
##' @export
tdc_in_study <- function(x, tstart = "tstart", tstop = "tstop"){
    ts <- sort(unique(c(x[[tstart]], x[[tstop]])))
    ts <- ts[-length(ts)]
    foo <- function(t){
        sum(x[[tstart]] <= t & x[[tstop]] > t)
    }
    data.frame(time = ts,
               in.study = unlist(lapply(ts, foo)))
}

##' stats for tdc
##'
##' Attempt to describe a time dependent covariate (tdc).
##' \code{survival::tmerge} creates time periods (per patient) starting at
##' 'tstart' and ending at 'tstop'. This function calculates a statistic at each
##' time point of interest (set of all distinct time points starting or ending
##' an interval). For each such point, the values of a tdc is gathered to
##' calculate a statistic (determined by argument 'stat').
##' @param x data frame with time periods starting at tstart, ending at tstop.
##' @param var name of variable of interest
##' @param stat function to calculate stat. Note: can return a vector
##' @param stat.names names of columns created, if set to NULL, the function
##'     will provide them
##' @param tstart name of variable acting as tstart
##' @param tstop name of variable acting as tstop
##' @return a data frame
##' @export
tdc_stat <- function(x, var, stat, stat.names = NULL,
                     tstart = "tstart", tstop = "tstop"){
    x <- x[, c(tstart, tstop, var)]
    ts <- sort(unique(c(x[[tstart]], x[[tstop]])))
    ts <- ts[-length(ts)]
    foo <- function(t, data = x){
        s <- data[data[[tstart]] <= t & data[[tstop]] > t, ]
        stat(s[[var]])
    }
    l <- lapply(ts, foo)
    n <- unique(unlist(lapply(l, length)))
    if(length(n) != 1){
        stop("out length of 'stat' differ")
    }
    r <- if(n > 1){
        cbind(
            data.frame(time = ts),
            as.data.frame(do.call(rbind, l))
        )
    } else {
        data.frame(time = ts,
                   stat = unlist(l))
    }
    if(!is.null(stat.names)){
        if(ncol(r) == length(stat.names) + 1){
            names(r) <- c("time", stat.names)
        } else {
            warning("bad length of 'stat.names'")
        }
    }
    r
}


if(FALSE){

    set.seed(20210908)
    d <- data.frame(
        id = c(1, 1, 1,
               2, 2, 2,
               3, 3, 3),
        tstart = c(0, 10, 20,
                   0, 9, 19,
                   1, 11, 23),
        tstop = c(10, 20, 30,
                  9, 19, 29,
                  8, 21, 31),
        v = rpois(9, 10),
        ev = 0
    )

    d[order(d$tstart, rev(d$tstop)), ]
    tdc_in_study(x = d)
    ## not quite the same as:
    ## survival::survfit(survival::Surv(tstart, tstop,ev) ~ 1, data = d) %>%
    ##     broom::tidy()

    d[order(d$tstart, rev(d$tstop)), ]
    tdc_stat(d, "v", mean)
    tdc_stat(d, "v", mean, stat.names = "HolyCow!")
    tdc_stat(d, "v", function(x) c("min" = min(x), "max" = max(x)))
    tdc_stat(d, "v", function(x) c("min" = min(x), "max" = max(x)),
             stat.names = c("Yeah", "FooBar"))


}
