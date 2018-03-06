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
    if(!all(surv %in% names(data))){
        ## if variables are not of class Surv they must have consistent
        ## naming, with the same prefix for the time- and event variable, resp.
        for(i in seq_along(surv)){
            ti <- tryCatch(
                expr = {
                    get(paste0(prefix["time"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oops! cant find time ", surv,
                                " in the data\n "))
                }
            )
            ev <- tryCatch(
                expr = {
                    get(paste0(prefix["event"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oopsie! cant find event ", surv,
                                " in the data\n"))
                }
            )
            data[[surv[i]]] <- survival::Surv(time = ti, event = ev)
        }
    }
    R <- NULL
    for(i in seq_along(glist)){
        for(j in seq_along(strata_lev)){
            filter <- glist[[i]] & data[[strata]] == strata_lev[j]
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

if(FALSE){

    n = 3*1000
    df <- data_frame(
        t.foo = abs(rnorm(n, 10, 2)),
        ev.foo = rbinom(n, 1, 0.2),
        t.bar = abs(rnorm(n, 10, 3)),
        ev.bar = rbinom(n, 1, 0.3),
        strutt = sample(letters[1:2], n, TRUE),
        gsak = sample(LETTERS[4:7], n, TRUE),
        wait = .5 + abs(rnorm(n, 0, 1))
    )
    X <- survfit_it(data = df, surv = c("foo", "bar"),
                   w = "wait",
                   strata = "strutt", glist = "gsak")
    ggplot(X, aes(time, estimate, color = strata)) +
        geom_line() +
        facet_grid(outcome ~ group)

}
