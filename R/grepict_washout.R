##' left censor / wash out
##'
##' ignore events prior to some boundary time
##' @param g object from \code{grepict}
##' @param w a boundary time
##' @param u units for diff between 'begin' and 'end' (default 'days')
##' @return a data frame like that returned from \code{grepict}
##' @export
grepict_washout <- function(g, w = NULL, u = "days"){
    if(w <= 0){
        message("w should be 1 or more")
        invisible(as.data.frame(NULL))
    }
    ## g1 can be kept, but might need first.id updated
    g1 <- subset(g, !(g$time <= w & g$event == 1))
    ## g2 can be thrown away (a record of these are in g1)
    g2 <- subset(g, (g$time <= w & g$event == 1) & g$id %in% g1$id)
    ## g3 these have no record in g1 and must get one
    g3 <- subset(g, (g$time <= w & g$event == 1) &
                    !(g$id %in% g1$id) &
                    g$first.id == 1)
    if(nrow(g3) > 0){
        g3$event = 0
        g3$time = as.numeric(difftime(g3$end, g3$begin, units = u))
        g3$date <- g3$end
        g3$match <- g3$match.in <- NA
    }
    ## now fix the g1:ers that need fixin
    g1a <- subset(g1, !(g1$id %in% g2$id)) ## no fix necessary
    g1b <- subset(g1, (g1$id %in% g2$id)) ## fix necessary
    foo <- function(X){
        n <- nrow(X)
        X$first.id <- c(1, rep(0, n-1))
        X
    }
    g1b.fix <- do.call(rbind, lapply(split(g1b, g1b$id), FUN = foo))
    rbind(g1a, g1b.fix, g3, make.row.names = FALSE)
}


if(FALSE){

    ## g <- data.frame(
    ##     id = sprintf("id%s", c(1,2,3,4,5,5,5,6,6,7,7,8,9,9)),
    ##     begin = as.Date("2007-01-01"),
    ##     end = as.Date("2008-01-01"),
    ##     date = as.Date(NA),
    ##     event = c(1,0,1,0,1,1,1,1,1,1,1,0,1,1),
    ##     time = c(10,5,30,35,2,39,40,7,8,50,60,80,1,100),
    ##     match = NA,
    ##     match.in = NA,
    ##     pattern = NA,
    ##     alias = NA,
    ##     first.id = c(1,1,1,1,1,0,0,1,0,1,0,1,1,0),
    ##     first.id_date = 1,
    ##     stringsAsFactors = FALSE
    ## )
    ## g$match = ifelse(g$event == 1, "a", NA)
    ## g$match.in = ifelse(g$event == 1, "var x", NA)
    ## g$pattern = "(a)"
    ## g$alias = "A"
    ## ## w = 27
    ## ## u = "days"
    ## for(ID in sprintf("id%s", 1:9)){
    ##     subset(g, id == ID)
    ##     subset(grepict_washout(g, 27), id == ID)
    ## }
    ## X <- readRDS("ignore/grepict_washout-data.Rds")
    ## test <- grepict_washout(X, w = 27)
    ## ## equal?
    ## summary(test$time[test$event == 0 & test$time <= 27])
    ## summary(X$time[X$event == 0 & X$time <= 27])
    ## ## equal?
    ## summary(test$time[test$event == 1 & test$time > 27])
    ## summary(X$time[X$event == 1 & X$time > 27])

    ## df <- data.frame(
    ##     rid = 1:13,
    ##     id = sprintf("id%s", 1:8)[c(1,2,2,3,4,4,5,5,6,6,6,7,8)],
    ##     begin = as.Date("2000-01-01"),
    ##     end = as.Date("2000-12-31"),
    ##     time = c(1,2,3,11,12,13,4,14,5,6,15,7,16),
    ##     event = rep(1:0, c(11,2)),
    ##     match.in = NA,
    ##     match = NA,
    ##     stringsAsFactors = FALSE
    ## )
    ## df$first.id <- as.numeric(!duplicated(df$id))
    ## df$date <- df$begin + df$time
    ## ## df[order(df$id), ]
    ## df
    ## (gw <- grepict_washout(g = df, w = 10))
    ## gw[order(gw$id), ]
    ## subset(df, !rid %in% gw$rid)

}
