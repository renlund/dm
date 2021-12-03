##' cumsum bounded
##'
##' a version of cumsum that will stay within specified boundaries
##' @param v a numeric vector
##' @param low numeric; lower bound
##' @param high numeric; upper bound
##' @return a numeric vector
##' @export
##' @examples
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), low = -50)
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1)) ## low = 0 is default
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), high = 5)
cumsum_bounded <- function(v, low = 0, high = Inf){
    if(low >= high) stop("args do not make sense")
    n <- length(v)
    R <- rep(NA, n)
    bound <- function(x) min( max(x, low), high )
    R[1] <- bound(v[1])
    if(n > 1){
        for(i in 2:n){
            R[i] <- bound(R[i-1] + v[i])
        }
    }
    R
}

if(FALSE){ ## MANUAL TESTS OF cumsum_bounded
    cumsum_bounded_rec(c(0,-1,-100,1,7,-99,1,1,1,-1), low = -50)
    cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), low = -50)
    cumsum_bounded_rec(c(0,-1,-100,1,7,-99,1,1,1,-1))
    cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1))
    cumsum_bounded_rec(c(0,-1,-100,1,7,-99,1,1,1,-1), high = 5)
    cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), high = 5)
    ## check that cumsum_bounded agrees with cumsum when bounds are infinite:
    for(i in 1:100){
        tmp <- rnorm(100)
        print(sum(cumsum_bounded(tmp, low = -Inf) - cumsum(tmp)) < 1e-12)
    }
}

##' change active status at end of inventory
##'
##' Note: this is a rigid function that requires variabes 'id' and 't'.  This
##' function behaves much like \code{expire_status}, but the expiration boundary
##' is determined by end of inventory. At times 't' an individual ('id') gets
##' its inventory replenished by 'inventory' (which can be a fixed number or
##' given by that variable in the input data frame 'x'). The inventory
##' diminishes by 'usage' (1, by default) each unit of time. This function
##' calculates a 'status' variable which will be 1 as long as there is available
##' inventory, and 0 when it runs out.
##' @param x a data frame that contains 'id' and 't'
##' @param inventory inventory, if NULL needs to exists in 'x', if not NULL a
##'     variable called 'inventory' will be created with the value(s) given here
##' @param usage numeric; depreciation of inventory per unit of time
##' @param overflow_at numeric; upper bound to inventory
##' @param slim logical; remove unnecessary statuses? If TRUE 'inventory' will
##'     be dropped from the output as this data is no longer complete
##' @seealso \code{\link{expire_status}}
##' @return a data frame
##' @export
expire_status_by_inventory <- function(x, inventory = NULL,
                             usage = 1,
                             overflow_at = Inf,
                             slim = FALSE){
    .required_data_names(data.names = names(x),
                         required = c("id", "t"))
    .required_properties(x = slim,
                         class = "logical",
                         length = 1,
                         nm = "slim")
    .required_properties(x = usage,
                         class = c("numeric", "integer"),
                         length = 1,
                         nm = "usage")
    .required_properties(x = overflow_at,
                         class = c("numeric", "integer"),
                         length = 1,
                         nm = "overflow_at")
    if(is.null(inventory)){
        .required_data_names(data.names = names(x),
                             required = c("inventory"))
    } else {
        .required_properties(x = inventory,
                             class = "numeric",
                             length = c(1, nrow(x)),
                             nm = "inventory")
        x$inventory <- inventory
    }
    if(any(x$inventory <= 0)){
        stop("'inventory' must be (strictly) positive")
    }
    m <- length(unique(paste(as.character(x$id), as.character(x$t))))
    if(nrow(x) != m){
        warning("duplicated times (per id) will be eliminated")
    }
    order.x <- order(x$id, x$t)
    OX <- x[order.x, c("id", "t", "inventory")]
    s <- split(OX, f = OX$id)
    ## X = s[[1]]; reduce = slim
    foo <- function(X, reduce = slim){
        X <- X[!duplicated(X$t),]
        n.X <- nrow(X)
        diff <- as.numeric(X$t[n.X] + sum(X$inventory) - X$t[1] + 1)
        tt <- X$t[1] + 0:diff
        n <- length(tt)
        vv <- rep(-usage, n)
        times_index <- which(tt %in% X$t)
        vv[times_index] <- X$inventory - usage
        c_vv <- cumsum_bounded(vv, low = 0, high = overflow_at)
        zeros_index <- which(c(FALSE, c_vv[1:(n-1)] > 0 &
                                             c_vv[2:n] == 0)) + 1 ## XK added +1
        keep_index <- sort(unique(c(times_index, zeros_index)))
        Z <- data.frame(id = X$id[1], t = tt,
                        ## inventory = c_vv + usage,
                        remains = c_vv)[keep_index, ]
        Z$status <- ifelse(Z$remains > 0, 1, 0)
        ## Z$remains <- NULL
        ## tmp <- data.frame(id = X$id[1], t = tt, remains = c_vv)[keep_index, ]
        ## Z <- merge(X, tmp, all = TRUE)
        ## Z$status <- ifelse(Z$remains > 0, 1, 0)
        ## Z$remains <- NULL
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

if(FALSE){
    ## check correspondence to expire_status
    x <- data.frame(
        id = rep(LETTERS[c(1,2)], each = 3),
        t = c(0,10,20,
              0,10,20)
    )
    (a <- expire_status(x, expire = 5))
    (b <- expire_status_by_inventory(x, inventory = 5))
    v <- c("id", "t", "status")
    identical(a[, v],b[, v])
    ## check correspondence to scaling:
    b2 <- expire_status_by_inventory(x, inventory = 7*(5+1), usage = 7)
    identical(b[, v],b2[, v])
    ## manual look:
    EI <- data.frame(
        id = rep(LETTERS[c(1,2)], c(5,7)),
        t = as.Date("2010-01-01") + c(0,10,25,35,50,
                                      0,50,51,52,70,100,150)
    )
    n <- nrow(EI)
    (x <- EI[sample(1:n, n), ])
    EI
    EI1 <- expire_status_by_inventory(x = x, inventory = 12, slim = FALSE)
    EI1$no_slim = 1
    EI2 <- expire_status_by_inventory(x = x, inventory = 12, slim = TRUE)
    EI2$slim = 1
    (M <- merge(EI1, EI2, all.x = TRUE))
    EI$org_data = 1
    merge(M, EI, all.x = T)
}

##' @describeIn expire_status_by_inventory generalization of
##'     expire_status_by_inventory; the 'status' - now called 'state' - can be
##'     multivalued; 'usage' and 'overflow_at' can be variables in 'x'
##' @param null.state value assigned to expired state
##' @export
expire_state_by_inventory <- function(x, inventory = NULL,
                                      usage = NULL,
                                      overflow_at = NULL,
                                      null.state = "",
                                      slim = FALSE){
    .required_data_names(data.names = names(x),
                         required = c("id", "t", "state"))
    .required_properties(x = null.state,
                         class = c("character", "numeric"),
                         length = 1,
                         nm = "null.state")
    .required_properties(x = slim,
                         class = "logical",
                         length = 1,
                         nm = "slim")
    ## control usage
    if(is.null(usage)){
        .required_data_names(data.names = names(x),
                             required = c("usage"))
    } else {
        .required_properties(x = usage,
                             class = "numeric",
                             length = c(1, nrow(x)),
                             nm = "usage")
        x$usage <- usage
    }
    if(any(x$usage <= 0)){
        stop("'usage' must be (strictly) positive")
    }
    ## control overflow_at
    if(is.null(overflow_at)){
        .required_data_names(data.names = names(x),
                             required = c("overflow_at"))
    } else {
        .required_properties(x = overflow_at,
                             class = "numeric",
                             length = c(1, nrow(x)),
                             nm = "overflow_at")
        x$overflow_at <- overflow_at
    }
    if(any(x$overflow_at <= 0)){
        stop("'overflow_at' must be (strictly) positive")
    }
    ## control inventory
    if(is.null(inventory)){
        .required_data_names(data.names = names(x),
                             required = c("inventory"))
    } else {
        .required_properties(x = inventory,
                             class = "numeric",
                             length = c(1, nrow(x)),
                             nm = "inventory")
        x$inventory <- inventory
    }
    if(any(x$inventory <= 0)){
        stop("'inventory' must be (strictly) positive")
    }
    ## warn
    m <- length(unique(paste(as.character(x$id), as.character(x$t))))
    if(nrow(x) != m){
        warning("duplicated times (per id) will be eliminated")
    }
    order.x <- order(x$id, x$t)
    OX <- x[order.x, c("id", "t", "state", "inventory", "usage", "overflow_at")]
    s <- split(OX, f = OX$id)
    ## X = s[[1]]
    foo <- function(X, reduce = slim){
        X <- X[!duplicated(X$t),]
        n.X <- nrow(X)
        diff <- as.numeric(X$t[n.X] - X$t[1] + 1 +
                           sum(ceiling(X$inventory / X$usage)))
        tt <- X$t[1] + 0:diff
        n <- length(tt)
        times_index <- which(tt %in% X$t)
        lengths_index <- c(times_index[-1], n+1) - times_index
        vv <- rep(-X$usage, lengths_index)
        vv[times_index] <- X$inventory - X$usage
        oa <- rep(X$overflow_at, lengths_index)
        ss <- rep(X$state, lengths_index)
        cbin <-cluster.by.incl.next(incl.next = c(ss[1:(n-1)] == ss[2:n], NA))
        ## data.frame(tt, vv, oa, ss, cbin)
        c_vv <- unlist(lapply(X = split(x = data.frame(vv, oa), f = cbin),
                              FUN = function(X){
                                  cumsum_bounded(X$vv, low = 0, high = X$oa[1])
                              }), use.names = FALSE)
        zeros_index <- which(c(FALSE, c_vv[1:(n-1)] > 0 &
                                             c_vv[2:n] == 0)) + 1 ## XK added +1
        keep_index <- sort(unique(c(times_index, zeros_index)))
        Z <- data.frame(id = X$id[1], t = tt, state = ss, remains = c_vv)[keep_index, ]
        Z$state <- ifelse(Z$remains == 0, null.state, Z$state)
        ## Z <- merge(X, tmp, all = TRUE)
        ## Z$status <- ifelse(Z$remains > 0, 1, )
        ## Z$remains <- NULL
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

if(FALSE){ ## MANUAL TEST of expire_state_by_inventory
    x = data.frame(
        id = rep(1:2, c(4, 4)),
        t = rep(c(0, 1, 7, 15), 2),
        state = LETTERS[c(1, 1, 2, 1, 1, 1, 2, 1)],
        inventory = c(5, 5, 10, 5, 10, 10, 5, 10),
        usage = 1,
        overflow_at = c(20, 20, 15, 20, 20, 20, 15, 20)
    )
    expire_state_by_inventory(x, slim = FALSE)
    expire_state_by_inventory(x, slim = TRUE)

}
