##' change active status at expiration boundary
##'
##' Note: this is a rigid function that needs variables 'id' and 't' and will
##'     create a variable 'status' (if such exists it will be overwritten). Feed
##'     this function a data frame with variables 'id' (identifier) and 't'
##'     (integer valued time measurement). 't' represents times for some event
##'     whose status (1) should expire (to 0) (strictly) after the integer
##'     duration specified by 'expire' (unless the status is "refreshed" by
##'     having another 't'-value before expiration). Duplicated times (per
##'     individual) will always be removed, but the output can also be reduced
##'     to the smallest output that keeps track of the changing status by
##'     setting parameter 'slim' to \code{TRUE}.
##' @param x data frame with id variable 'id' and time variable 't'
##' @param expire expiration boundary
##' @param slim remove unnecessary statuses
##' @return a data frame
##' @export
expire_status <- function(x, expire, slim = FALSE){
    order.x <- order(x$id, x$t)
    OX <- x[order.x, ]
    s <- split(OX, f = OX$id)
    foo <- function(X, bound = expire, reduce = slim){
        X <- X[!duplicated(X$t),]
        n.X <- nrow(X)
        X$status <- 1
        d <- c(NA, X$t[2:n.X] - X$t[1:(n.X-1)])
        i <- c(which(d > expire)-1, n.X)
        Y <- X[i, ]
        Y$t <- Y$t + expire
        Y$status <- 0
        Z <- rbind(X, Y)
        order.Z <- order(Z$t)
        if(reduce){
            U <- Z[order.Z, ]
            n.U <- nrow(U)
            keep <- c(TRUE, U$status[2:n.U] != U$status[1:(n.U-1)])
            U[keep, ]
        } else {
            Z[order.Z,]
        }
    }
    l <- lapply(s, foo)
    do.call(rbind, l)
}

if(FALSE){

    ES <- data.frame(id = 1, t = c(-20,-9,0,11,12))
    expire_status(x = ES, expire = 10, slim = FALSE)
    expire_status(x = ES, expire = 10, slim = TRUE)
    identical(
        expire_status(x = ES, expire = 10, slim = FALSE),
        expire_status(x = ES[sample(1:nrow(ES)),],
                      expire = 10, slim = FALSE)
    )

    ES <- data.frame(
        id = rep(LETTERS[c(1,2)], c(5,7)),
        t = c(0,10,25,35,50,
              0,50,52,52,70,100,150)
    )
    n <- nrow(ES)
    (x <- ES[sample(1:n, n), ])

    ES
    ES1 <- expire_status(x = x, expire = 10, slim = FALSE)
    ES2 <- expire_status(x = x, expire = 10, slim = TRUE)
    ES1$no_slim = 1
    ES2$slim = 1
    (M <- merge(ES1, ES2, all.x = TRUE))
    ES$org_data = 1
    merge(M, ES, all.x = T)

    ## WORKS WITH DATES?

    ES <- data.frame(id = 1, t = as.Date("2001-01-01") + c(-20,-9,0,11,12))
    ES
    expire_status(x = ES, expire = 10, slim = FALSE)
    expire_status(x = ES, expire = 10, slim = TRUE)
    identical(
        expire_status(x = ES, expire = 10, slim = FALSE),
        expire_status(x = ES[sample(1:nrow(ES)),],
                      expire = 10, slim = FALSE)
    )

    ES <- data.frame(
        id = rep(LETTERS[c(1,2)], c(5,7)),
        t = as.Date("2010-01-01") + c(0,10,25,35,50,
              0,50,52,52,70,100,150)
    )
    n <- nrow(ES)
    (x <- ES[sample(1:n, n), ])

    ES
    ES1 <- expire_status(x = x, expire = 10, slim = FALSE)
    ES2 <- expire_status(x = x, expire = 10, slim = TRUE)
    ES1$no_slim = 1
    ES2$slim = 1
    (M <- merge(ES1, ES2, all.x = TRUE))
    ES$org_data = 1
    merge(M, ES, all.x = T)

}
