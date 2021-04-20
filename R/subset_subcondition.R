##' subset by subcondition
##'
##' this function subsets by some condition, but returns all information
##'     associated with those values of the meta variable 'meta.var' that would
##'     be in the standard subset returned by the same condition.
##' @param data the data, a data frame or some such thing
##' @param meta.var the meta variable
##' @param ... condition for subsetting passed to \code{subset}
##' @return a data frame
##' @examples
##' d <- data.frame(id = rep(LETTERS[1:2], each = 2), z = 1:4)
##' ## 'subset' returns the rows satisfying the condition
##' subset(d, z == 4)
##' ## 'subset_subcondition' returns all rows such that the 'meta.var' value is
##' ## in the standard subset returned, i.e. a (possibly) bigger set
##' subset_subcondition(d, meta.var = "id", z == 4)
##' @export
subset_subcondition <- function(x, meta.var = NULL, ... , select = NULL){
    .required_properties(x = x, class = "data.frame")
    .required_properties(x = meta.var, class = "character", length = 1)
    .required_properties(x = select, class = c("NULL", "character"))
    if(is.null(select)) select <- names(x)
    set <- subset(x = x, ..., select = meta.var, drop = TRUE)
    x[x[[meta.var]] %in% set, select, drop = FALSE]
}

if(FALSE){

    df <- data.frame(
        id = rep(LETTERS[1:4], each = 3),
        x = 1:12
    )
    subset_subcondition(data = df, meta.var = "id", x >= 9)

    d <- data.frame(id = rep(LETTERS[1:2], each = 2), z = 1:4)
    subset(d, z == 4)
    subset_subcondition(d, meta.var = "id", z == 4)
    subset_subcondition(d, meta.var = "id", z == 4 | id == "A")

}
