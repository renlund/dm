#' @title Turn all recoded tables in doc into latex
#' @description ...
#' @param doc the dm documentation (if \code{NULL} we look in \code{dm_doc()})
#' @param file 'file' argument for \code{Hmisc::latex} (default = "")
#' @param lab.prefix prefix for labels for LaTeX tables (default =
#'     "tab:recode_") which concatenates with the variable name
#' @param where LaTeX indicator of position for float (Default: "htb")
#' @param clearpage how many floats before a LaTeX clearpage? Recycled if
#'     necessary
#' @param ... arguments passed to \code{Hmisc::latex}.
#' @export
dm_recode2latex <- function(doc = NULL, file = "",
                            lab.prefix = "tab:recode_",
                            where = "htb", clearpage = NULL, ...){
    if(is.null(doc)) doc <- dm_doc()
    if(length(doc) == 0) stop("doc empty")
    dummy <- 0
    if(!is.null(clearpage)) clearpage <- cumsum(rep(clearpage, length(doc)))
    for(k in seq_along(doc)){
        X <- doc[[k]]
        if(is.null(recode <- X$recode)){
            next
        } else {
            if(is.null(L <- X$recode_table)){
                tmp_var <- get(X$where)[[X$var]]
                L <- table(
                    tmp_var,
                    recode(x = tmp_var, L = recode),
                    dnn = c(X$var, X$name),
                    useNA="ifany"
                )
            }
        }
        var <- texify(X$where)
        if(requireNamespace("Hmisc")){
            Hmisc::latex(object = L,
                         file = file,
                         where = where,
                         append = TRUE,
                         title = "old $\\downarrow$ new $\\rightarrow$",
                         caption = paste0("Recoding of data base ", texify(X$db),
                                          " entry ", texify(X$var)," into ",texify(X$name),"."),
                         label = paste0(lab.prefix, X$name),
                         ...)
        } else {
            cat("Error-ish\n")
            warning("package Hmisc not available")
        }
        dummy <- dummy + 1
        if(dummy %in% clearpage) cat("\n\\clearpage\n")
    }
    invisible(NULL)
}

texify <- function(s, f = "\\texttt"){
    if(length(s)) {
        gsub("_", "\\_",
             paste0(f, "{", s, "}"),
             fixed=TRUE )
    } else ""
}

##  #' @title Recoded variables
##  #' @description Get the names of the variables that have been recoded
##  #' @param doc the 'data_man' doc
##  #' @export

## dm_recoded <- function(doc){
##     if(is.null(doc)) doc <- dm_doc()
##     if(length(doc) == 0) stop("[dm_create] doc empty")
##     names(doc)[unlist(lapply(doc, function(x) !is.null(x$recode)))]
## }
