##' forgiving rbind version
##'
##' rbind where columns do not necessarily are equal
##' @param x a data frame
##' @param y another data frame
##' @param in.both if TRUE, only columns present in both 'x' and 'y' are rbinded
##' @export
rbind_kind <- function(x, y, in.both = FALSE){
    x.nm <- names(x)
    y.nm <- names(y)
    all.nm <- unique(c(x.nm, y.nm))
    common.nm <- intersect(x.nm, y.nm)
    x.notin.y <- setdiff(x.nm, y.nm)
    y.notin.x <- setdiff(y.nm, x.nm)
    for(v in x.notin.y){
        y[[v]] <- NA
    }
    for(v in y.notin.x){
        x[[v]] <- NA
    }
    if(in.both){
        rbind(x[,common.nm], y[,common.nm])
    } else {
        rbind(x[,all.nm], y[,all.nm])
    }
}

if(FALSE){
    x <- data.frame(x = 1:2, y = letters[1:2], z = c(TRUE, FALSE))
    y <- data.frame(x = 3:4, y = letters[3:4], u = c("foo", "bar"))
    rbind_kind(x, y, in.both = FALSE)
    rbind_kind(x, y, in.both = TRUE)
    y$y <- factor(y$y)
    str(rbind_kind(x, y, in.both = TRUE))
}
