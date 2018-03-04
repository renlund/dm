##' @title fix censored dates
##' @description fix censored dates of the type '20110000' or '20110200' by selecting the
##'     midpoint of the censored time interval. If a lower bound is given, the
##'     midpoint between this point and the end of the censored interval is
##'     chosen.
##' @param x dates
##' @param sep seprator in x, if any
##' @param low.bound the lower bound
##' @param ok.years span of years that are ok, values outside will generate an
##'     error, \code{NULL} (default) all years are accepted
##' @export
cdate <- function(x, sep = NULL, low.bound = NULL, ok.years = NULL){
    if(L <- !is.null(low.bound)){
        if(length(x) != length(low.bound)){
            stop("want same length of bound as x")
        }
    }
    n <- length(x)
    if(n==0) return(NULL)
    r <- as.Date(rep(NA, n))
    for(i in 1:n){
        tmp <- tryCatch(
            fix.single.cdate(x = x[i], sep = sep,
                             low.bound = if(L) low.bound[i] else NULL,
                             ok.years = ok.years),
            error = function(e){
                stop(paste0("\n\nFailed to fix x = ", x[i], " at index ", i,
                            ", with error message ", e,
                            if(L) paste0("Lower bound is ", low.bound[i], ".")))
            }
        )
        r[i] <- tmp
    }
    r
}

# - #' helper for cdate
fix.single.cdate <- function(x, sep = NULL, low.bound = NULL, ok.years = NULL){
    if(length(x) != 1) stop("want length 1 vector 'x'")
    add0maybe <- function(x){
        if(nchar(x)==1) paste0("0", x) else x
    }
    create_date <- function(a, b, c){
        as.Date(paste(c(a, add0maybe(b), add0maybe(c)), collapse = "-"))
    }
    if(!is.null(sep)){
        x <- paste0(unlist(strsplit(x, split = sep, fixed = TRUE)),
                    collapse = "")
    }
    if(nchar(x) != 8){
        stop("wrong number of characters")
    }
    y <- as.numeric(substr(x, 1, 4))
    if(!is.null(ok.years)){
        if(!y %in% ok.years) stop("not an ok year")
    }
    m <- as.numeric(substr(x, 5, 6))
    d <- as.numeric(substr(x, 7, 8))
    if(!is.null(low.bound)){
        ref_y <- as.numeric(substr(low.bound, 1, 4))
        ref_m <- as.numeric(substr(low.bound, 6, 7))
        ref_d <- as.numeric(substr(low.bound, 9, 10))
        if(ref_y < y) low.bound <- NULL
    } else {
        ref_y <- y
        ref_m <- m
        ref_d <- 0
    }
    r <- tryCatch(expr = as.Date(paste(c(y, m, d), collapse = "-")),
        error = function(e) -1)
    if(class(r) == "Date") {
        if(!is.null(low.bound)){
            if(r<low.bound) stop("date before lower bound!")
            return(r)
        } else {
            return(r)
        }
    }
    if(!(m==0 | d == 0)) stop("unknown weirdness")
    if(y < ref_y | (ref_y == y & m > 0 & m < ref_m)) stop("date before lower bound!")
    if(ref_m == 0) ref_m <- 1
    if(ref_d == 0) ref_d <- 1
    ref_date <- create_date(max(ref_y, y), max(ref_m, m), max(ref_d, d))
    if(m == 0){
        end <- create_date(y, "12", "31")
        ref_date + difftime(end, ref_date) / 2
    } else {
        mdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
        M <- mdays[as.numeric(m)]
        end <- create_date(y, m, M)
        ref_date + difftime(end, ref_date) / 2
    }
}
