##' convert censored times
##'
##' censored times might already be of the 'Surv' class, or where the 'time' and
##'     'event' components of such a class is stored separately, either way this
##'     is a function to transition between these two formats
##' @param data the data, a data frame or some such object
##' @param id is there an identifiction variable that should remain in the data set
##' @param toSurv logical; resulting variables to be of class 'Surv'?
##' @param .fix named vector of pre- or suffix that identifies the 'time' and
##'     'event'-variables (names must contain "time" and "event")
##' @param suffix is .fix a suffix?
##' @return a data frame
##' @examples
##' df <- data.frame(
##'        persnr = sprintf("id%s", 1:10),
##'        t.arg = rexp(10, 1/50),
##'        ev.arg = rbinom(10, 1, 0.7)
##' )
##' surv_converter(df, id = "persnr")
##' @export
surv_converter <- function(data, id = NULL, toSurv = NULL,
                           .fix = c("time" = "t.", "event" = "ev."),
                           suffix = TRUE){
    klass <-  unlist(lapply(data, class))
    if(is.null(toSurv)) toSurv <- !any("Surv" %in% klass)
    if(toSurv){
        surv_pair2Surv(data = data, id = id, .fix = .fix, suffix = suffix)
    } else {
        Surv2surv_pair(data = data, id = id, .fix = .fix, suffix = suffix)
    }
}

##' @describeIn surv_converter transition from separate variables to class 'Surv'
##' @export
surv_pair2Surv <- function(data, id = NULL,
                           .fix = c("time" = "t.", "event" = "ev."),
                           suffix = TRUE){
    .check_suffix(.fix)
    ts <- if(suffix){
              .fixfnc(.fix['time'], "^", suffix = TRUE)
          } else {
              .fixfnc(.fix['time'], "$", suffix = FALSE)
          }
    es <- if(suffix){
              .fixfnc(.fix['event'], "^", suffix = TRUE)
          } else {
              .fixfnc(.fix['event'], "$", suffix = FALSE)
          }
    tl <- grepl(pattern = ts, x = names(data))
    el <- grepl(pattern = es, x = names(data))
    tn <- gsub(pattern= ts, replacement = "", names(data)[tl])
    en <- gsub(pattern = es, replacement = "", names(data)[el])
    set <- intersect(tn, en)
    TN <- .fixfnc(set, .fix['time'], suffix = suffix)
    EN <- .fixfnc(set, .fix['event'], suffix = suffix)
    indx_id <- which(names(data) == id)
    if(length(indx_id) > 0){
        r <- data.frame(dummy = data[[id]], stringsAsFactors = FALSE)
    } else r <- data.frame(1:nrow(data))
    for(i in seq_along(set)){
        r[[set[i]]] <- survival::Surv(time = data[[TN[i]]],
                                      event = data[[EN[i]]])
    }
    if(length(indx_id) > 0){
        names(r)[1] <- id
        r
    } else r[,-1]
}

##' @describeIn surv_converter transition from class 'Surv' to separate variables
##' @export
Surv2surv_pair <- function(data, id = NULL,
                           .fix = c("time" = "t.", "event" = "ev."),
                           suffix = TRUE){
    .check_suffix(.fix)
    klass <-  unlist(lapply(data, class))
    indx_surv <- which(klass == "Surv")
    indx_id <- which(names(data) == id)
    if(length(indx_id) > 0){
        r <- data.frame(dummy = data[[id]])
    } else r <- data.frame(1:nrow(data))
    for(i in indx_surv){ ## i = indx_surv[1]
        time_name <- .fixfnc(names(data)[i], .fix['time'], suffix = suffix)
        event_name <- .fixfnc(names(data)[i], .fix['event'], suffix = suffix)
        r[[time_name]] <- data[, i][,1]
        r[[event_name]] <- data[, i][,2]
    }
    if(length(indx_id) > 0){
        names(r)[1] <- id
        r
    } else r[,-1]
}

.check_suffix <- function(suffix){
    if(!all(c("time", "event") %in% names(suffix))){
        stop("suffix must contain named elements 'time' and 'event'\n")
    } else invisible(NULL)
}

.fixfnc <- function(s, .fix, suffix = TRUE){
    if(suffix) paste0(.fix, s) else paste0(s, .fix)
}


if(FALSE){

    data <- data.frame(
        persnr = sprintf("id%s", 1:10),
        zip.ev = rbinom(10, 1, 0.6),
        t.arg = rexp(10, 1/50),
        ev.arg = rbinom(10, 1, 0.7),
        x = survival::Surv(rexp(10, 1/25), rbinom(10, 1, 0.5)),
        t.wtf = rexp(10, 1/50),
        y = runif(10),
        poz.t = rexp(10, 1/50),
        poz.ev = rbinom(10, 1, 0.7),
        ev.wtf = rbinom(10, 1, 0.7),
        ev.NO = rbinom(10, 1, 0.9),
        foo = survival::Surv(rexp(10, 1/25), rbinom(10, 1, 0.5)),
        zip.t = rexp(10, 1/50),
        NO.t = rexp(10, 1/30)
    )
    ## id = "persnr"

    Surv2surv_pair(data = data)
    Surv2surv_pair(data = data, id = "persnr")
    Surv2surv_pair(data = data, id = "persnr",
                   .fix = c("time" = "A", "event" = "B"))
    Surv2surv_pair(data = data, id = "persnr",
                   .fix = c("time" = "A", "event" = "B"), suffix = FALSE)


    surv_pair2Surv(data)
    surv_pair2Surv(data = data, id = "persnr")
    surv_pair2Surv(data = data, id = "persnr",
                   .fix = c("time"=".t", "event"=".ev"), suffix = FALSE)

    surv_converter(data = data)
    surv_converter(data = data, toSurv = TRUE)
    surv_converter(data = data, toSurv = FALSE)



}
