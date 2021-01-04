##' @title fix censored dates
##' @description fix censored dates of the type '20110000' or '20110200' by
##'     selecting the midpoint of the censored time interval. If a lower bound
##'     is given, the midpoint between this point and the end of the censored
##'     interval is chosen.
##' @param x dates, possibly censored, as character or numeric (e.g. 20070101)
##' @param sep separator in x, if any
##' @param low.bound the lower bound
##' @param verbose logical; print additional info, maybe
##' @param bound4all logical (default \code{FALSE}); apply bound also to
##'     uncensored dates?
##' @param ignore.na logical; ignore missing values in x?
##' @export
cdate <- function(x, sep = NULL, low.bound = NULL, verbose = TRUE,
                  bound4all = FALSE, ignore.na = TRUE){
    ## check arguments
    if(Lnull <- is.null(low.bound)) bound4all <- FALSE
    .required_properties(x, class = c('character', 'numeric'))
    .required_properties(sep, class = c('character', 'NULL'), length = 0:1)
    .required_properties(bound4all, class = 'logical', length = 1)
    .required_properties(ignore.na, class = 'logical', length = 1)
    n <- length(x)
    if(n == 0) as.Date(character(0))
    .required_properties(low.bound, class = c('Date', 'NULL'), length = c(0, n))
    ## make dates directly, for those possible
    dform <- paste0("%Y", sep, "%m", sep, "%d")
    dd <- as.Date(x, format = dform)
    ## fail <- which(is.na(dd))
    fail <- which(is.na(dd) & !is.na(x)) ## surely, this must be what I want?
    dummy <- 0
    if(verbose){
        if(length(fail) == 0){
            message('all x interpretable as dates')
        } else {
            message('some x not interpretable as dates (at most 100 printed):\n')
            if(Lnull){
                print(utils::head(x[fail], n = 100))
            } else {
                print(utils::head(data.frame(
                    not_ok_dates = x[fail],
                    low.bound = low.bound[fail]
                ), n = 100))
            }
            message("\nwe'll try to fix them\n")
            dummy <- 1
        }
    }
    if(bound4all){
        w <- dd < low.bound
        if(any(w)){
            message("some interpretable dates before lower bound",
                    " (at most 100 printed):")
            print(utils::head(subset(data.frame(
                before.bound = x,
                low.bound = low.bound
            ), subset = w), n = 100))
            stop(" ...and that is an error")
        }
    }
    for(i in fail){
        tmp <- tryCatch(
            expr = fix.single.cdate(x = x[i],
                                    sep = sep,
                                    low.bound = low.bound[i],
                                    ignore.na = ignore.na,
                                    bound4all = bound4all),
            error = function(e){
                stop(paste0("\n\nFailed to fix x = ", x[i], " at index ", i,
                            ", with error message:", e, "\n",
                            if(!Lnull){
                                paste0("Lower bound is ", low.bound[i], ".\n")
                            }))
            }
        )
        dd[i] <- tmp
    }
    if(verbose && dummy == 1) cat("fixed!")
    dd
}


fix.single.cdate <- function(x, sep = NULL,
                             low.bound = NULL, bound4all = FALSE,
                             ignore.na = TRUE){
    if(is.null(low.bound) || is.na(low.bound)) low.bound <- NULL ## might be called with NA
    .required_properties(x, class = c('character', 'numeric'), length = 1)
    .required_properties(low.bound, class = c("Date", "NULL"), length = c(0,1))
    .required_properties(ignore.na, class = 'logical', length = 1)
    .required_properties(sep, class = c('character', 'NULL'), length = 0:1)
    if(is.na(x)){
        if(ignore.na) return(as.Date(NA)) else stop("x is missing")
    }
    if(!is.null(sep)){ ## expect form 20010101, else make it so
        x <- paste0(unlist(strsplit(x, split = sep, fixed = TRUE)),
                    collapse = "")
    }
    if(nchar(x) != 8){
        stop("wrong number of characters")
    }
    ## extract year, month, day
    y <- as.numeric(substr(x, 1, 4))
    m <- as.numeric(substr(x, 5, 6))
    d <- as.numeric(substr(x, 7, 8))
    ## if no lower bound, set to 1st jan 1 year prior
    if(is.null(low.bound)){
        low.bound <- as.Date(paste0(y-1, "-01-01"))
    }
    ## test if already date
    r <- tryCatch(expr = as.Date(x, format = "%Y%m%d"),
                  error = function(e) NA)
    ## if r was not interpretable as date then m or d == 0
    if(d != 0 && m != 0){
        warning("unknown censoring")
        if(m == 0 && d != 0){
            d <- 0
            warning("day has been censored")
        }
    }
    before_txt <- paste0("date (", x, ") before lower bound (", low.bound, ")")
    if(!is.na(r)){
        if(bound4all && r < low.bound) stop(before_txt)
        return(r)
    }
    ## get boundary year, month, day
    bound_y <- as.numeric(substr(low.bound, 1, 4))
    bound_m <- as.numeric(substr(low.bound, 6, 7))
    bound_d <- as.numeric(substr(low.bound, 9, 10))
    ## set NA:s to be overwritten
    start_m <- end_m <- start_d <- NA_real_
    if(y < bound_y){
        stop(before_txt)
    } else if(y > bound_y){
        start_m <- if(m == 0) 1 else m
        start_d <- 1
        end_m <- if(m==0) 12 else m
    } else if(m != 0 && m < bound_m){ ## implict that y == bound_y
        stop(before_txt)
    } else if(m == 0 || m > bound_m){
        start_m <- if(m == 0) bound_m else m
        start_d <- if(m == 0) bound_d else 1
        end_m <- if(m == 0) 12 else m
     } else if(d != 0 && d < bound_d){ ## implicit that m == bound_m
        stop(before_txt)
    } else {
        start_m <- bound_m
        start_d <- bound_d
        end_m <- bound_m
    }
    ## sanity check
    if(is.na(start_m) || is.na(end_m) || is.na(start_d)){
        stop("if the author of this function is correct, ",
             "noone should ever see this error message")
    }
    end_d <- mdays(y, end_m)
    ## create start and end days
    start <- create_date(y, start_m, start_d)
    end <- create_date(y, end_m, end_d)
    ## return day inbetween start and end
    start + difftime(end, start) / 2
}

add0maybe <- function(x){
    if(nchar(x)==1) paste0("0", x) else x
}
create_date <- function(a, b, c){
    as.Date(paste(c(a, add0maybe(b), add0maybe(c)), collapse = "-"))
}

mdays <- function(y, m){
    if(!m %in% 1:12) stop("m needs to be integer; 1 <= m <= 12")
    if(!y %in% 1582:3000){
        y <- as.integer(y)
        if(y < 1582) warning("before start of gregorian calendar")
        if(y > 3000) warning("woha! thats far into the future")
    }
    ref <- as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))
    leap <- y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)
    ref[m] + if(m == 2 & leap) 1L else 0L
}

if(FALSE){
    mdays(2000, 2)
    mdays(2001, 2)
    mdays(2100, 2)
    mdays(3200, 2)
}

## OLD VERSION (slow for large 'x' input):
## @title fix censored dates
## @description fix censored dates of the type '20110000' or '20110200' by
##     selecting the midpoint of the censored time interval. If a lower bound
##     is given, the midpoint between this point and the end of the censored
##     interval is chosen.
## @param x dates
## @param sep seprator in x, if any
## @param low.bound the lower bound
## @param ok.years span of years that are ok, values outside will generate an
##     error, \code{NULL} (default) all years are accepted
## @param ignore.na logical; ignore missing values?
## @export
## cdate <- function(x, sep = NULL, low.bound = NULL, ok.years = NULL,
##                   ignore.na = TRUE){
##     if(L <- !is.null(low.bound)){
##         if(length(x) != length(low.bound)){
##             stop("want same length of bound as x")
##         }
##     }
##     n <- length(x)
##     if(n==0) return(NULL)
##     r <- as.Date(rep(NA, n))
##     for(i in 1:n){
##         tmp <- tryCatch(
##             fix.single.cdate(x = x[i], sep = sep,
##                              low.bound = if(L) low.bound[i] else NULL,
##                              ok.years = ok.years, ignore.na = ignore.na),
##             error = function(e){
##                 stop(paste0("\n\nFailed to fix x = ", x[i], " at index ", i,
##                             ", with error message ", e,
##                             if(L) paste0("Lower bound is ", low.bound[i], ".")))
##             }
##         )
##         r[i] <- tmp
##     }
##     r
## }

## - #' helpers for cdate
## fix.single.cdate <- function(x, sep = NULL, low.bound = NULL,
##                              ok.years = NULL, ignore.na = TRUE){
##     .required_properties(x, length = 1)
##     ## if(length(x) != 1) stop("want length 1 vector 'x'")
##     if(!is.null(low.bound)) if(!class(low.bound) %in% 'Date'){
##                                 stop("low.bound should be a Date")
##                             }
##     if(ignore.na) if(is.na(x)) return(as.Date(NA))
##     if(!is.null(sep)){ ## expect form 20010101, else make it so
##         x <- paste0(unlist(strsplit(x, split = sep, fixed = TRUE)),
##                     collapse = "")
##     }
##     if(nchar(x) != 8){
##         stop("wrong number of characters")
##     }
##     ## ## test if already date
##     ## r <- tryCatch(expr = as.Date(x, format = "%Y%m%d"),
##     ##               error = function(e) NA)
##     ## if(!is.na(r)){
##     ##     if(!is.null(low.bound) && r < low.bound){
##     ##         stop("date before lower bound")
##     ##     } else return(r)
##     ## }
##     y <- as.numeric(substr(x, 1, 4))
##     if(!is.null(ok.years)){
##         if(!y %in% ok.years) stop("not an ok year")
##     }
##     m <- as.numeric(substr(x, 5, 6))
##     d <- as.numeric(substr(x, 7, 8))
##     ## c.lev <- if(m==0) 'm' else if(d==0) 'd' else 'wot?'
##     ## if(c.lev == 'wot?') warning("not a strange date")
##     ref_y <- y
##     ref_m <- m
##     ref_d <- 0
##     b4 <- "date before lower bound"
##     if(!is.null(low.bound)){
##         tmp_y <- as.numeric(substr(low.bound, 1, 4))
##         if(tmp_y >= y){
##             ref_y <- tmp_y
##             tmp_m <- as.numeric(substr(low.bound, 6, 7))
##             if(tmp_m >= m){
##                 ref_m <- tmp_m
##                 ref_d <- as.numeric(substr(low.bound, 9, 10))
##             }
##         }
##     }
##     r <- tryCatch(expr = as.Date(paste(c(y, m, d), collapse = "-")),
##         error = function(e) -1)
##     if(class(r) == "Date") {
##         if(!is.null(low.bound)){
##             if(r<low.bound) stop("date before lower bound!")
##             return(r)
##         } else {
##             return(r)
##         }
##     }
##     if(!(m==0 | d == 0)) stop("unknown weirdness")
##     if(y < ref_y | (ref_y == y & m > 0 & m < ref_m)){
##         stop("date before lower bound!")
##     }
##     if(ref_m == 0) ref_m <- 1
##     if(ref_d == 0) ref_d <- 1
##     ref_date <- create_date(max(ref_y, y), max(ref_m, m), max(ref_d, d))
##     if(m == 0){
##         end <- create_date(y, "12", "31")
##         ref_date + difftime(end, ref_date) / 2
##     } else {
##         mdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
##         M <- mdays[as.numeric(m)]
##         end <- create_date(y, m, M)
##         ref_date + difftime(end, ref_date) / 2
##     }
## }
