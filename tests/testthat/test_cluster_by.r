test_that("'cluster.by' works", code = {

    df <- data.frame(
        grouping.wanted = factor(LETTERS[c(1, 1, 2, 2, 3, 4, 5)]),
        next.incl = c(T, F, T, F, F, F, F)
        )
    df$create <- cluster.by.incl.next(incl.next = df$next.incl)
    ## df ## visually inspect

    expect_equal(
        cluster.by.incl.next(df$next.incl),
        as.numeric(df$grouping.wanted)
    )

    v <- c(T,T,F,T,F)
    expect_equal(
        cluster.by.incl.next(v),
        c(1,1,1,2,2)
    )
    v2 <- v; v2[5] <- !v[5]
    expect_equal(
        cluster.by.incl.next(v),
        cluster.by.incl.next(v2)
    )
    v2[5] <- NA
    expect_equal(
        cluster.by.incl.next(v),
        cluster.by.incl.next(v2)
    )

    ## random test!
    seed <- sample(1:1000000, 1)
    set.seed(seed)
    n <- 10000
    m <- 15
    start <- sample(seq(1, n, by = 10), size = m, replace = FALSE)
    lag_start <- c(start[2:m], n+1)
    stop <- ifelse(rbinom(m, 1, 0.6),
                   yes = lag_start,
                   no = lag_start - 5)
    next.incl <- stop == lag_start
    r <- rep(NA, m)
    r[1] <- 1
    for(i in 2:m){
        if(start[i] == stop[i-1]){
            r[i] <- r[i-1]
        } else {
            r[i] <- r[i-1] + 1
        }
    }
    expect_equal(
        cluster.by.incl.next(incl.next = next.incl),
        r
    )
    data.frame(start, stop, r,
               calc = cluster.by.incl.next(incl.next = next.incl))

})
