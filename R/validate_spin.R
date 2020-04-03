##' validate SPIN
##'
##' test if a Swedish Personal Identity Number is plausible by checking the
##'     control number
##' @param spin a swedish person identity number of the form "XXXXXX-XXXX" or
##'     "XXXXXXXX-XXXX"
##' @return logical
##' @export
validate_spin <- function(spin){
    foo <- function(arg){
        n <- nchar(arg)
        if(n == 13){
            arg <- substr(arg, start = 3, stop = 13)
        }
        sep <- substr(arg, start = 7, stop = 7)
        s1 <- gsub(pattern = sep, replacement = "", arg, fixed = TRUE)
        s2 <- unlist(strsplit(s1, split = ""))
        ctrl <- as.numeric(s2[10])
        s3 <- as.numeric(s2[1:9])
        mult <- c(2,1,2,1,2,1,2,1,2)
        prod <- s3 * mult
        Sum <- sum(as.numeric(unlist(strsplit(as.character(prod), split = ""))))
        sig1 <- signif(Sum, 1)
        if(sig1 < Sum) sig1 <- signif(Sum + 5, 1)
        ctrl_check <- sig1 - Sum
        ctrl_check == ctrl
    }
    unlist(lapply(spin, foo))
}

if(FALSE){
    validate_spin(spin = "790424-1473")
    validate_spin("19790424-1473")
    validate_spin("080608-7649")
    validate_spin(c("110425-7314", "790424-1473"))
    validate_spin("800428-2946")
    validate_spin("121212-1212")
    validate_spin("121212.7777")
}
