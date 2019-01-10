##' @title document filtering
##' @description stores filtering documentation in dm_filter in dm_envir
##' @param f logical vector
##' @param name character
##' @param comment description of filtering
##' @return nuffin
##' @export
dmf <- function(f, name, comment){
    if(any(is.na(f))) stop("no NA:s, please")
    n <- length(f)
    inc <- sum(f)
    fp <- fperc(inc/n)
    cat("includes ", fp, " (", inc, "/", n, " rows)\n", sep = "")
    L <- list(
        'filter' = f,
        'name' = name,
        'comment' = comment,
        'n' = n,
        'rows' = inc,
        'perc' = fp
    )
    dm_filter_set(name = name, value = L)
    invisible(NULL)
}

##' @title create filter
##' @description use dmf-created list to make overall filter
##' @param x an object from \code{dmf}
##' @return logical vector
##' @export
dmf_create <- function(x = NULL){
    if(is.null(x)) x <- dm_filter()
    if(length(x) == 0){
        message("no filtering documentation")
        return(invisible(NULL))
    }
    fs <- as.data.frame(lapply(x, function(z) z$filter))
    Reduce(f = `&`, x = fs, init = rep(TRUE, nrow(fs)), accumulate = FALSE)
}

## helper function
fperc <- function(p){
    if(p > 1 | p < 0) "impossible"
    else if(p == 1) "100 perc."
    else if(p > .999) ">99.9 perc."
    else if(p < 0.001) "<.1 perc."
    else paste0(round(100*p, 1), " perc.")
}

##' @title print 'dm_filter' object
##' @description prints a data frame version of selected info in a 'dm_filter'
##'     object or returns that data frame
##' @param x a 'dm_filter' object
##' @param ... arguments passed to print.data.frame
##' @param print if \code{FALSE} then an data frame is returned
##' @param seq order in which to look at the filters
##' @param id id-vector for units, necessarily in the same order as filter
##' @param verbose logical, want funtion to provide messages?
##' @return possibly a data frame
##' @export
print.dm_filter <- function(x, ..., print = TRUE, seq = NULL, id = NULL, verbose = TRUE){
    if(length(x) == 0){
        message("no filtering documentation")
        return(invisible(NULL))
    }
    def_seq <- seq_along(x)
    if(is.null(seq)) seq <- def_seq
    if(any(!seq %in% def_seq)){
        stop("bad seq, try a permutation of ", paste0(def_seq, collapse = ","))
    }
    if(!setequal(seq, def_seq)){
        warning("seq doesn't cover all filters")
    }
    fs <- as.data.frame(lapply(x[seq], function(z) z$filter))
    R <- Reduce(f = `&`, x = fs, init = rep(TRUE, nrow(fs)), accumulate = TRUE)
    if(is.null(id)){
        mess <- paste0("## Filter applied to rows:")
        N <- nrow(fs)
        inc <- c(N, unlist(lapply(fs, sum)))
        exc <- N - inc
        seqinc <- unlist(lapply(R, sum))
        seqexc <- c(0, abs(diff(seqinc, differences = 1)))
        X <- data.frame(
            criteria = c('Population', names(x)[seq]),
            incl = inc,
            excl = exc,
            seq.incl = seqinc,
            seq.excl = seqexc,
            row.names = NULL
        )
    } else {
        mess <- paste0("## Filter applied to units:")
        id.N <- length(unique(id))
        id.inc <- c(id.N, unlist(lapply(x[seq], function(z) length(unique(id[z$filter])))))
        id.exc <- id.N - id.inc
        id.seqinc <- unlist(lapply(R, function(z) length(unique(id[z]))))
        id.seqexc <- c(0, abs(diff(id.seqinc, differences = 1)))
        X <- data.frame(
            criteria = c('Population', names(x)[seq]),
            incl = id.inc,
            excl = id.exc,
            seq.incl = id.seqinc,
            seq.excl = id.seqexc,
            row.names = NULL
        )
    }
    if(print){
        if(verbose) message(mess)
        print(X, ...)
        invisible(NULL)
    } else X
}


if(FALSE){
    set.seed(20190109)
    n = 100
    df <- data.frame(
        id = sample(1:(n/2), n, TRUE),
        x = runif(n),
        y = sample(letters[1:4], n, TRUE),
        z = rbinom(n, 1, .1),
        stringsAsFactors = FALSE
    )
    x = list(
        list(filter = df$x > 0.5),
        list(filter = df$y == "c"),
        list(filter = df$z == 0)
    )
    A <- df[x[[1]][[1]], ]
    B <- df[x[[2]][[1]], ]
    AB <- df[x[[1]][[1]] & x[[2]][[1]] , ]
    C <- df[x[[3]][[1]], ]
    ABC <- df[x[[1]][[1]] & x[[2]][[1]] & x[[3]][[1]], ]
    printfilt(x)
    ## -----
    nrow(df)
    nrow(A)
    nrow(B)
    nrow(C)
    ## -----
    nrow(df)
    nrow(A)
    nrow(AB)
    nrow(ABC)
    ## ----------
    printfilt(x, id = df$id)
    ## -----
    length(unique(df$id))
    length(unique(A$id))
    length(unique(B$id))
    length(unique(C$id))
    ## -----
    length(unique(df$id))
    length(unique(A$id))
    length(unique(AB$id))
    length(unique(ABC$id))
    rm(n, df, x, A, B, AB, C, ABC)
}

##' @title filtering as latex list
##' @description ...
##' @param f dm_filter object or similar
##' @return NULL and side effects
##' @export
dm_filter2latexlist <- function(f = NULL){
    if(is.null(f)) f <- dm_filter()
    if(length(f) == 0){
        message("no filtering documentation")
        return(invisible(NULL))
    }
    ns <- unlist(lapply(f, function(x) x$n))
    if(!all(ns == ns[1])) stop("n differ")
    bar <- function(x){
        paste0("\\texttt{",
               gsub("_", "\\_", x$name, fixed = TRUE),
               "} '", x$comment, "'",
               " includes ",
               gsub(">", "$>$", x$perc, fixed = TRUE),
               " (", x$rows, " rows)")
    }
    l <- unlist(lapply(f, bar))
    texList <- paste0("\\begin{itemize}\n",
                      paste0("  \\item ", l),
                      "\\end{itemize}\n")
    fs <- as.data.frame(lapply(f, function(x) x$filter))
    N <- sum(rowSums(fs) == length(f))
    conc <- paste0("\\noindent", N, " of ", ns[1], " are included.\n")
    cat(texList, conc)
    invisible(NULL)
}

##' @title filter distances
##' @description some kind of metric on the filters
##' @details this needs thinking about, but the idea was that sequantially
##'     looking at inclusion/exclusion of filters can be very misleading
##' @param f a dm_filter
##' @param plot logical; plot cluster dendogram?
##' @param m 1 or 2; method (needs documentation)
##' @export
dm_filter2dist <- function(f = NULL, plot = TRUE, m = 1){
    if(is.null(f)) f <- dm_filter()
    if(length(f) == 0) stop("no filtering documentation")
    x <- as.data.frame(lapply(f, function(x) x$filter))
    if(any(is.na(x))) stop("no NA in filter, please")
    n <- ncol(x)
    inc <- rowSums(x) == n
    d <- function(i, j, M = m){
        indx <- switch(M,
                       "1" = indx <- !inc,
                       "2" = indx <- !(x[[i]] == 1 & x[[j]] == 1))
        sum(x[indx, i] != x[indx, j]) / sum(indx)
    }
    M <- matrix(NA, nrow = n, ncol = n)
    for(i in 2:n){
        for(j in 1:(i-1)){
            M[i, j] <- d(i, j)
        }
    }
    D <- structure(
        M[!is.na(M)],
        class = 'dist',
        Size = as.integer(n),
        Labels = names(x),
        Diag = FALSE,
        Upper = FALSE,
        method = 'complete',
        call = ''
    )
    if(plot & n > 2){
        yl <- if(m == 1) "among excluded"
              else  "among excluded (by pairwise criteria)"
        plot(stats::hclust(D),
             ylab = paste("Difference", yl),
             xlab = "Criteria", sub = "",
             main = "Cluster dendogram for difference in critera")
    } else if(plot & n <= 2) message("no plot (need > 2 variables)")
    D
}
