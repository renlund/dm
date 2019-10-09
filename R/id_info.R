##' id info
##'
##' some info on uniqueness and missingness of an id varible
##' @param id id variable
##' @export
id_info <- function(id){
    m <- sum(is.na(id))
    n <- length(stats::na.omit(unique(id)))
    N <- length(id)
    p1 <- 100*n/N
    p1t <- if(p1 < 0.1){
               "< 0.1%"
           } else if(p1 < 100 & p1 > 99.9){
               "> 99.9%"
           } else {
               paste0(round(p1, 1), "%")
           }
    t1 <- paste0("variable has ", n, ", i.e. ", p1t,", unique (non-missing) values.")
    p2 <- 100*m/N
    p2t <- if(p2 < 0.1){
              "< 0.1%"
          } else{
              paste0(round(p2, 1), "%")
          }
    t2 <- paste0("\nThere are ", m, ", i.e. ", p2t, ", missing values.")
    t <- paste0(t1, if(m > 0) t2 else NULL, "\n")
    cat(t)
    invisible(NULL)
}
