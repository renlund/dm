##' change active status at expiration boundary
##'
##' Note: this is a rigid function that needs variables 'id' and 't' (and
##' optionally 'expire') and will create a variable 'status' (if such exists it
##' will be overwritten). Feed this function a data frame with variables 'id'
##' (identifier) and 't' (integer valued time measurement) and - optionally -
##' 'expire' (positive numeri). 't' represents times for some event whose status
##' (1) should expire (to 0) (strictly) after the integer duration specified by
##' 'expire' (unless the status is "refreshed" by having another 't'-value
##' before expiration). Duplicated times (per individual) will always be
##' removed, and the output can also be reduced to the smallest output that
##' keeps track of the changing status by setting parameter 'slim' to
##' \code{TRUE}.
##' @param x data frame with id variable 'id' and time variable 't'
##' @param expire expiration boundary, if NULL needs to exists in 'x', if not
##'     NULL a variable called 'expire' will be created with the value(s) given
##'     here
##' @param slim logical; remove unnecessary statuses?
##' @seealso \code{\link{expire_status_by_inventory}}, \code{\link{expire_state_by_inventory}}
##' @return a data frame
##' @export
expire_status <- function(x, expire = NULL, slim = FALSE){
    ## check arguments and assign expire to x if given separately
    .required_data_names(data.names = names(x),
                         required = c("id", "t"))
    .required_properties(x = slim, class = "logical", length = 1, nm = "slim")
    if(is.null(expire)){
            .required_data_names(data.names = names(x),
                                 required = c("expire"))
    } else {
        .required_properties(x = expire, class = "numeric",
                             length = c(1, nrow(x)), nm = "expire")
        x$expire <- expire
    }
    if(any(x$expire <= 0)){
        stop("'expire' must be (strictly) positive")
    }
    m <- length(unique(paste(as.character(x$id), as.character(x$t))))
    if(nrow(x) != m){
        warning("duplicated times (per id) will be eliminated")
    }
    order.x <- order(x$id, x$t)
    OX <- x[order.x, c("id", "t", "expire")]
    s <- split(OX, f = OX$id)
    foo <- function(X, reduce = slim){
        X <- X[!duplicated(X$t),]
        n.X <- nrow(X)
        X$status <- 1
        d <- X$t[2:n.X] - X$t[1:(n.X-1)]
        i <- c(which(d > X$expire[-n.X]), n.X)
        Y <- X[i, ]
        Y$t <- Y$t + Y$expire
        Y$expire <- 0
        Y$status <- 0
        Z <- rbind(X, Y)
        order.Z <- order(Z$t)
        if(reduce){
            U <- Z[order.Z, c("id", "t", "status")]
            n.U <- nrow(U)
            keep <- c(TRUE, U$status[2:n.U] != U$status[1:(n.U-1)])
            U[keep, ]
        } else {
            Z[order.Z,]
        }
    }
    l <- lapply(s, foo)
    r <- do.call(rbind, l)
    rownames(r) <- NULL
    r
}

if(FALSE){ ## MANUAL TEST OF expire_status
    ## WORKS WITH DATES?
    ES <- data.frame(
        id = rep(LETTERS[c(1,2)], c(5,7)),
        t = as.Date("2010-01-01") + c(0,10,25,35,50,
              0,50,52,52,70,100,150)
    )
    n <- nrow(ES)
    (x <- ES[sample(1:n, n), ])
    ES
    ES1 <- expire_status(x = x, expire = 10, slim = FALSE)
    ES1$no_slim = 1
    ES2 <- expire_status(x = x, expire = 10, slim = TRUE)
    ES2$slim = 1
    (M <- merge(ES1, ES2, all.x = TRUE))
    ES$org_data = 1
    merge(M, ES, all.x = T)
}

##' @describeIn expire_status generalization of expire_status; the 'status' - now
##'     called 'state' (which must exist in 'x') - can be multivalued
##' @param null.state value assigned to expired state
##' @export
expire_state <- function(x, expire = NULL, null.state = "", slim = FALSE){
    ## check arguments and assign expire to x if given separately
    .required_data_names(data.names = names(x),
                         required = c("id", "t", "state"))
    .required_properties(x = slim, class = "logical", length = 1, nm = "slim")
    .required_properties(x = null.state, class = c("character", "numeric"),
                         length = 1, nm = "null.state")
    .required_properties(x = x$state, class = c("character", "numeric"),
                         nm = "x$state")
    if(is.null(expire)){
            .required_data_names(data.names = names(x),
                                 required = c("expire"))
    } else {
        .required_properties(x = expire, class = "numeric",
                             length = c(1, nrow(x)), nm = "expire")
        x$expire <- expire
    }
    if(any(x$expire <= 0)){
        stop("'expire' must be (strictly) positive")
    }
    m <- length(unique(paste(as.character(x$id), as.character(x$t))))
    if(nrow(x) != m){
        warning("duplicated times (per id) will be eliminated")
    }
    order.x <- order(x$id, x$t)
    OX <- x[order.x, c("id", "t", "state", "expire")]
    s <- split(OX, f = OX$id)
    ## X = s[[1]]
    foo <- function(X, reduce = slim){
        X <- X[!duplicated(X$t),]
        n.X <- nrow(X)
        ## X$status <- 1
        d <- X$t[2:n.X] - X$t[1:(n.X-1)]
        i <- c(which(d > X$expire[-n.X]), n.X)
        Y <- X[i, ]
        Y$t <- Y$t + Y$expire
        Y$expire <- 0
        Y$state <- null.state
        Z <- rbind(X, Y)
        order.Z <- order(Z$t)
        if(reduce){
            U <- Z[order.Z, c("id", "t", "state")]
            n.U <- nrow(U)
            keep <- c(TRUE, U$state[2:n.U] != U$state[1:(n.U-1)])
            U[keep, ]
        } else {
            Z[order.Z,]
        }
    }
    l <- lapply(s, foo)
    r <- do.call(rbind, l)
    rownames(r) <- NULL
    r
}

if(FALSE){ ## MANUAL TEST OF expire_state
    x = data.frame(
        id = rep(1:2, c(4, 4)),
        t = rep(c(0, 1, 7, 15), 2),
        state = LETTERS[c(1, 1, 2, 1, 1, 1, 2, 1)],
        expire = c(5,5,10,5,10,10,5,10)
    )
    expire_state(x, slim = FALSE)
    expire_state(x, slim = TRUE)
}

## OLD FUNCTION VERSION! change active status at expiration boundary
##
## Note: this is a rigid function that needs variables 'id' and 't' and will
##     create a variable 'status' (if such exists it will be overwritten). Feed
##     this function a data frame with variables 'id' (identifier) and 't'
##     (integer valued time measurement). 't' represents times for some event
##     whose status (1) should expire (to 0) (strictly) after the integer
##     duration specified by 'expire' (unless the status is "refreshed" by
##     having another 't'-value before expiration). Duplicated times (per
##     individual) will always be removed, but the output can also be reduced
##     to the smallest output that keeps track of the changing status by
##     setting parameter 'slim' to \code{TRUE}.
## @param x data frame with id variable 'id' and time variable 't'
## @param expire expiration boundary
## @param slim remove unnecessary statuses
## @return a data frame
expire_status_old <- function(x, expire, slim = FALSE){
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
