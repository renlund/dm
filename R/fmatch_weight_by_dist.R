##' matching distances
##'
##' wrapper for \code{matched.distances}
##' @param matchobj matching object, \code{fullmatch} or \code{pairmatch} (from
##'     the optmatch package)
##' @param distance distance matrix created by \code{match_on}
##' @param data data frame
##' @export
##' @importFrom optmatch matched.distances
optmatch_distances<- function(matchobj, distance, data){
    data <- as.data.frame(data)
    data$cl <- as.character(matchobj)
    DIST <- matched.distances(matchobj = matchobj,
                              distance = distance,
                              preserve.unit.names = FALSE)
    unlist_d <- unlist(DIST)
    nms <- names(unlist_d)
    split_nms <- strsplit(nms, split = ".", fixed = TRUE)
    foo <- function(s) paste0(s[1], ".", s[2])
    bar <- function(s) s[3]
    row <- as.numeric(unlist(lapply(split_nms, bar)))
    ic <- unlist(lapply(split_nms, foo))
    data$dist <- rep(NA_real_)
    data$dist[row] <- as.numeric(unlist_d)
    data$.internal.control <- rep(NA_character_)
    data$.internal.control[row] <- ic
    if(!all(data$.internal.control[row] == data$cl[row])){
       stop("\n------------\n| failure! |\n------------\n")
    }
    data$.internal.control <- NULL
    data
}

##' ATT weights rescaled by matching distance
##'
##' assign (possibly) non-uniforms weights within matching cluster based on the
##'     intercluster distances
##' @param tr name of treatment variable
##' @param data data frame
##' @param cl name of cluster variable
##' @param dist name of distance variable
##' @param id name of id variable
##' @param trv value for treated in treatment variable
##' @param method name of method
##' @param method.params parameters (as list) passed to method
##' @export
fmatch_weight_by_dist <- function(tr, data, cl = "cl", dist = "dist", id = "id",
                                  trv = 1, method = "test",
                                  method.params = NULL){
    message("'fmatch_weight_by_dist' is still an experimental function!")
    if(!all(c(cl,dist) %in% names(data))){
        stop("specified cl and/or dist not in data")
    }
    D <- data[, c(id, tr, cl, dist)]
    names(D) <- c("id", "tr", "cl", "dist")
    ## av.c2t <- mean(table(D$cl), na.rm = TRUE)
    ## ds <- D$dist
    if(method == "test"){
        TEST <- D
        mp <- if(!is.null(method.params)){
                  method.params
              } else {
                  list(l = .025, h = .975, S = 5)
              }
        a <- quantile(TEST$dist, mp$l, na.rm = TRUE)
        b <- quantile(TEST$dist, mp$h, na.rm = TRUE)
        if(b == a) stop("strange weight distribution!")
        TEST$tmp <- ifelse(TEST$dist <= a, a, ifelse(TEST$dist >= b, b, TEST$dist))
        TEST$iw <- ( mp$S * b - a - (mp$S - 1)*TEST$tmp ) / (b - a)
        foo <- function(X){
            ref <- X$tr[is.na(X$dist)]
            if(ref[1] == 1){
                X$w <- X$iw / (sum(X$iw, na.rm = TRUE))
                X$w[is.na(X$w)] <- 1
                X
             } else {
                X$w = 1
                X$w[is.na(X$dist)] <- nrow(X) - 1
                X
            }
        }
        DC <- do.call(rbind, lapply(split(TEST, f = TEST$cl), foo))
        DC$tmp <- NULL
        DC$iw <- NULL
        R <- merge(TEST, subset(DC, TRUE, select = c("id", "w")),
                   all.x = TRUE, by = "id")
        R
    } else {
        message("no method but 'test' is implemented")
        invisible(as.data.frame(NULL))
    }
}

if(FALSE){
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(optmatch)
    library(descripteur)
    library(dm)

    nt <- 1000
    nc <- 5*nt
    n <- nc + nt
    df <- tibble(
        id = sprintf("id%s", 1:n),
        gr = rep(0:1, c(nc, nt)),
        grChr = if_else(gr==0, "control", "case"),
        age = case_when(
            gr == 0 ~ round(runif(n, 35, 85) + rnorm(n, 0, 10), 1),
            TRUE ~ round(runif(n, 45, 80) + rnorm(n, 3, 10))
        ),
        gender = case_when(
            gr == 1 ~ sample(c("M", "F"), n, TRUE, prob = c(3,1)),
            TRUE ~ sample(c("M", "F"), n, TRUE, prob = c(2,1))
        ),
        biomX = case_when(
            gr == 0 ~ round(rexp(n, 1/50), 1),
            TRUE ~ round(rexp(n, 1/95), 1)
        ),
        biomY = round(case_when(
            gender == "M" ~ runif(n, 30, 90) + rexp(n, 1/25),
            TRUE  ~ runif(n, 35, 90)
        ) + case_when(
                gr == 1 ~ rnorm(n, 5, 0),
                TRUE ~ 0
            ), 1),
        weight = round(case_when(
            gender == "M" ~ runif(n, 60, 90) + rexp(n, 1/5),
            TRUE  ~ runif(n, 60, 90) + rexp(n, 1/5)
        ) + case_when(
                gr == 1 ~ -rnorm(n, 5, 0),
                TRUE ~ 0
            ), 1),
        preSjuk = case_when(
            gr == 0 ~ sample(c(0,1), n, TRUE, prob = c(4,1)),
            TRUE ~ sample(c(0,1), n, TRUE, prob = c(1,1))
        )
    )
    dtables(df, glist = "grChr") %>%
        dtable_prune(rm = c("variable", "info"))

    model <- glm(gr ~ age + biomX + biomY + weight + gender + preSjuk,
        family = "binomial", data = df)
    df$ps <- predict(model, type = "response", newdata = df)

    ## mahalanobis matching
    mo <- match_on(gr ~ age + biomX + biomY + weight + gender + preSjuk,
                   data = df, method=  "mahalanobis")
    fm <- fullmatch(mo, data = df)
    tmp <- optmatch_distances(fm, mo, df)

    ## ps matching
    mo2 <- match_on(gr ~ ps, data = df, method = "euclidean")
    fm2 <- fullmatch(mo2, data = df)
    tmp2 <- optmatch_distances(fm2, mo2, df) %>%
        transmute(id, cl2 = cl, dist2 = dist)

    ## join
    df2 <- left_join(tmp, tmp2)

    ## get weights
    df3 <- match_weight(tr = "gr", cl = "cl", id = "id", data = df2) %>%
        select(id, att.weight)

    df4 <- fmatch_weight_by_dist (tr = "gr", data = df2) %>%
        select(id, d.weight = w)

    df5 <- fmatch_weight_by_dist (tr = "gr", data = df2,
                                  method.params = list(l = 0.01, h = 0.99, S = 10)) %>%
        select(id, d.weight2 = w)

    df6 <- match_weight(tr = "gr", cl = "cl2", id = "id", data = df2) %>%
        select(id, ps.weight = att.weight)

    df7 <- fmatch_weight_by_dist(tr = "gr", data = df2, cl = "cl2", dist = "dist2",
                                  method.params = list(l = 0.01, h = 0.99, S = 10)) %>%
        select(id, ps.weight2 = w)


    D <- df %>% left_join(df3) %>% left_join(df4) %>% left_join(df5) %>%
        left_join(df6) %>% left_join(df7)

    g <- dtable_guide(D, unit.id = "id", elim.set = c("gr", "d.weight", "att.weight"))

    (d2 <- dtables(D, guide = g, glist = "grChr") %>%
        dtable_prune(rm = c("variable", "info")))

    (d3 <- dtables(D, guide = g, glist = "grChr", w = "att.weight") %>%
        dtable_prune(rm = c("variable", "info")))

    (d4 <- dtables(D, guide = g, glist = "grChr", w = "d.weight") %>%
        dtable_prune(rm = c("variable", "info")))

    (d5 <- dtables(D, guide = g, glist = "grChr", w = "d.weight2") %>%
        dtable_prune(rm = c("variable", "info")))

    (d6 <- dtables(D, guide = g, glist = "grChr", w = "ps.weight") %>%
         dtable_prune(rm = c("variable", "info")))

    (d7 <- dtables(D, guide = g, glist = "grChr", w = "ps.weight2") %>%
         dtable_prune(rm = c("variable", "info")))


    foo <- function(d, s){
        D <- d %>% as.data.frame() %>%
            subset(Variables %in% c('age', 'biomX', 'biomY', 'weight',
                                    'gender: M', 'preSjuk: 1'),
                   select = c("Variables", "Std"))
        names(D)[2] <- s
        D
    }

    (DD <- foo(d2, "baseline") %>%
         left_join(foo(d3, "att")) %>%
         left_join(foo(d4, "test1")) %>%
         left_join(foo(d5, "test2")) %>%
         left_join(foo(d6, "ps")) %>%
         left_join(foo(d7, "ps2")))

    lev = DD %>% arrange(baseline) %>% pull(Variables)
    DD2 <- gather(DD, key = "type", value = "std", -Variables) %>%
        mutate(Variables = factor(Variables, levels = lev))


    theme_set(theme_bw())
    ggplot(DD2, aes(Variables, std, color = type, shape = type)) +
        geom_point(size = 4, alpha = 1/2) +
        coord_flip()


    DD2 %>% group_by(type) %>% summarise(m = mean(std)) %>% arrange(m)

}
