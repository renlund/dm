##' @title multiple survival curves
##' @description wrapper for survfit to create multiple survival curves for
##'     multiple endpoints, stratifications and groupings
##' @param surv the \code{Surv} objects (as character, refering to variables in
##'     'data') OR the common names of time + event, if these have similar names
##'     (differing only in perfix)
##' @param data the data
##' @param strata stratifying variable
##' @param glist a grouping list (list of logical vectors of the same length as
##'     rows in data, i.e. can be overlapping groups), or the name of a grouping
##'     variable in 'data'
##' @param w weight or name of variable that provides weights
##' @param type passed to \code{survival::survfit}
##' @param prefix prefix values for times and events
##' @param vs_age logical; should KM curves be with age on the x-axis?
##' @param age_var character; name of age variable. N.B. important that age is
##'     measured in the same units as the time variable
##' @param age_bound numeric; what age to start from
##' @param progress primitive displayer of progress
##' @param stringsAsFactors logical, if TRUE will keep ordering of ingoing stuff
##' @return a data frame (much like a \code{broom::tidy}-version of the output)
##' @export
##' @examples
##' n = 1200
##' df <- data.frame(
##'         t.foo = abs(rnorm(n, 10, 2)),
##'         ev.foo = rbinom(n, 1, 0.2),
##'         t.bar = abs(rnorm(n, 10, 3)),
##'         ev.bar = rbinom(n, 1, 0.3),
##'         strutt = sample(letters[1:2], n, TRUE),
##'         gsak = sample(LETTERS[4:7], n, TRUE),
##'         wait = .5 + abs(rnorm(n, 0, 1))
##' )
##' str(survfit_it(data = df, surv = c("foo", "bar"), w = "wait",
##'           strata = "strutt", glist = "gsak"))
survfit_it <- function(surv, data,
                       strata = NULL, glist = NULL, w = NULL,
                       type = "kaplan-meier",
                       prefix = c(event = "ev.", time = "t."),
                       vs_age = FALSE, age_var = NULL, age_bound = NULL,
                       progress = FALSE,
                       stringsAsFactors = TRUE){
    N <- nrow(data)
    if(is.null(w)) w <- rep(1, N)
    if(is.character(w)) w <- data[[w]]
    if(is.null(glist)) glist <- list(All = rep(TRUE, N))
    if(!is.list(glist)) glist <- make_glist(glist, ref = data)
    glist_lev <- names(glist)
    if(is.null(strata)){
        data$strata <- factor(rep("no strata", N))
        strata <- "strata"
    }
    if(!is.factor(data[[strata]])){
        data[[strata]] <- factor(data[[strata]])
    }
    strata_lev <- levels(data[[strata]])
    if(!vs_age & !is.null(age_var) & !is.null(age_bound)){
        message(paste0("[survfit_it] 'age_var' and 'age_bound' will only ",
                       "matter if 'vs_age' is TRUE."))
    }
    subSet <- TRUE ## will be changed if vs_age is TRUE
    if(!all(surv %in% names(data))){
        ## if variables are not of class Surv they must have consistent
        ## naming, with the same prefix for the time- and event variable, resp.
        for(i in seq_along(surv)){
            ti <- tryCatch(
                expr = {
                    get(paste0(prefix["time"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oops! cant find time ", surv[i],
                                " in the data\n "))
                }
            )
            ev <- tryCatch(
                expr = {
                    get(paste0(prefix["event"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oopsie! cant find event ", surv[i],
                                " in the data\n"))
                }
            )
            if(vs_age){
                age <- tryCatch(
                    expr = {
                        get(age_var, data)
                    },
                    error = function(e){
                        stop(paste0(" oops! cant find age variable ", age_var,
                                    " in the data\n "))
                    }
                )
                t2 <- age + ti ## XK ti NEED TO BE IN SAME UNITS AS AGE
                subSet <- t2 >= age_bound
                t1 <- pmin(age, age_bound)
                data[[surv[i]]] <- survival::Surv(time = t1,
                                                  time2 = t2,
                                                  event = ev,
                                                  type = "counting")
            } else {
                data[[surv[i]]] <- survival::Surv(time = ti,
                                                  event = ev)
            }
        }
    } else {
        if(vs_age){
            s <- paste0("vs_age TRUE not implemented in combination ",
                        "with outcome variables of class 'Surv'")
            stop(s)
        }
    }
    R <- NULL
    for(i in seq_along(glist)){
        for(j in seq_along(strata_lev)){
            filter <- glist[[i]] & data[[strata]] == strata_lev[j] & subSet
            X <- data[filter, ]
            W <- w[filter]
            for(k in seq_along(surv)){
                if(!"Surv" %in% class(data[[surv[k]]])){
                    warning(surv[k], " not of class 'Surv', skipping.")
                }
                if(progress){
                    cat(paste0(names(glist)[i], "(", i, "/", length(glist), "), ",
                               paste0(strata_lev[j], "(", j, "/", length(strata_lev), "), ",
                                      paste0(surv[k], "(", k, "/", length(surv), ")\n"))))
                }
                if(nrow(X) > 0){
                    s <- survival::survfit(stats::formula(paste0(surv[k], " ~ 1")),
                                           data = X,
                                           weight = W,
                                           type = type)
                    tmp <- data.frame(
                        'time' = s$time,
                        'estimate' = s$surv,
                        'ci.low' = s$lower,
                        'ci.high' = s$upper,
                        'n.risk' = s$n.risk,
                        'n.event' = s$n.event,
                        'n.censor' = s$n.censor,
                        'std.error' = s$std.err,
                        stringsAsFactors = FALSE
                    )
                } else {
                    tmp <- data.frame(
                        'time' = NA,
                        'estimate' = NA,
                        'ci.low' = NA,
                        'ci.high' = NA,
                        'n.risk' = NA,
                        'n.event' = NA,
                        'n.censor' = NA,
                        'std.error' = NA,
                        stringsAsFactors = FALSE
                    )
                }
                tmp$outcome <- surv[k]
                tmp$strata <- strata_lev[j]
                tmp$group <- names(glist)[i]
                R <- if(is.null(R)) tmp else rbind(R, tmp)
            }
        }
    }
    if(stringsAsFactors){
        R$outcome <- factor(R$outcome, levels = surv)
        R$strata <- factor(R$strata, levels = strata_lev)
        R$group <- factor(R$group, levels = glist_lev)
    }
    R
}

## helper function for 'survfit_it'
## @param x thing to create glist from
## @param ref reference for thing
make_glist <- function(x, ref = NULL){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)) stop("nah1")
            }
        } else {
            if(length(x) != length(ref)) stop("nah2")
        }
    }
    y <- as.factor(x)
    if(length(levels)>100) stop("nah3")
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}
##' at risk counter
##'
##' count individuals at risk and (cumulative) events at given time points
##' @param x an object created by \code{survfit_it} or equivalent
##' @param tp timepoints wanted
##' @param tidy output in tidy format (default \code{FALSE})
##' @return a data frame
##' @importFrom stats reshape
##' @export
at_risk <- function(x, tp = NULL, tidy = FALSE){
    dummy <- FALSE
    LO <- if(is.factor(x$outcome)){
              dummy <- TRUE
              levels(x$outcome)
          } else unique(x$outcome)
    LS <- if(is.factor(x$strata)) levels(x$strata) else unique(x$strata)
    LG <- if(is.factor(x$group)) levels(x$group) else unique(x$group)
    x$outcome <- as.character(x$outcome)
    x$strata <- as.character(x$strata)
    x$group <- as.character(x$group)
    R <- NULL
    for(O in LO){ ## O = LO[1]
        for(S in LS){ ## S = LS[1]
            for(G in LG){ ## G = LG[1]
                for(t in tp){ ## t = tp[2]
                    sel1 <- x$outcome == O &
                        x$strata == S &
                        x$group == G
                    sel2A <- x$time <= t
                    sel2B <- x$time >= t
                    dA <- subset(x, sel1 & sel2A)
                    dB <- subset(x, sel1 & sel2B)
                    ## r <- if(any(sel1 & sel2A)){
                    ##          min(dA$n.risk)
                    ##      } else{
                    ##          max(x$n.risk[sel1])
                    ##      }
                    r <- if(any(sel1 & sel2B, na.rm = TRUE)){
                             max(dB$n.risk)
                         } else{
                             0 ## min(x$n.risk[sel1])
                         }
                    e <- if(any(sel1 & sel2A, na.rm = TRUE)){
                             sum(dA$n.event)
                         } else 0
                    tmp <- data.frame(
                        outcome = O,
                        strata = S,
                        group = G,
                        time = t,
                        at.risk = r,
                        event = e,
                        stringsAsFactors = FALSE
                    )
                    R <- if(is.null(R)) tmp else rbind(R, tmp)
                }
            }
        }
    }
    if(dummy){
        R$outcome <- factor(R$outcome, levels = LO)
        R$strata <- factor(R$strata, levels = LS)
        R$group <- factor(R$group, levels = LG)
    }
    if(tidy){
        R
    } else {
        a <- reshape(data = subset(R, TRUE, select = setdiff(names(R), "event")),
                     idvar = c("outcome", "strata", "group"),
                     direction = 'wide', timevar = 'time', v.names = 'at.risk')
        names(a) <- gsub("at.risk.", "time.", names(a), fixed = TRUE)
        a$count = "at.risk"
        b <- reshape(data = subset(R, TRUE, select = setdiff(names(R), "at.risk")),
                     idvar = c("outcome", "strata", "group"),
                     direction = 'wide', timevar = 'time', v.names = 'event')
        names(b) <- gsub("event.", "time.", names(b), fixed = TRUE)
        b$count <- "event"
        n <- c("outcome", "strata", "group", "count")
        m <- setdiff(names(a), n)
        P <- subset(rbind(a, b), TRUE, select = c(n,m))
        if(dummy) P$count <- factor(P$count, levels = c("at.risk", "event"))
        Q <- P[order(P$outcome, P$strata, P$group, P$count), ]
        rownames(Q) <- NULL
        Q
    }
}


if(FALSE){

    n = 3*1000
    df <- data.frame(
        t.foo = abs(rnorm(n, 10, 2)),
        ev.foo = rbinom(n, 1, 0.2),
        t.bar = abs(rnorm(n, 10, 3)),
        ev.bar = rbinom(n, 1, 0.3),
        strutt = sample(letters[1:2], n, TRUE),
        gsak = sample(LETTERS[4:7], n, TRUE),
        wait = .5 + abs(rnorm(n, 0, 1))
    )
    X <- survfit_it(data = df, surv = c("foo", "bar"),
                   ## w = "wait",
                   strata = "strutt", glist = "gsak")
    ggplot(X, aes(time, estimate, color = strata)) +
        geom_line() +
        facet_grid(outcome ~ group)

    (ar <- at_risk(x = X, tp = c(5, 10, 15), tidy = FALSE)) ## XK KOLLA DETTA)

}
