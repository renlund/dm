#' @title Create data frame from documentation info
#' @description use the information stored (by default accessibly in
#'     \code{dm_doc()}) to create a data frame
#' @param set vector of id's
#' @param id.name identifier across data bases or a list of such, with names
#'     equal to the associated data base
#' @param doc if \code{NULL}, we look at \code{dm_doc()}
#' @export
dm_create <- function(set, id.name, doc = NULL){
    if(is.null(doc)) doc <- dm_doc()
    if(length(doc) == 0) stop("[dm_create] doc empty")
    pdoc <- print.dm_doc(doc, print = FALSE)
    all_db <- unique(unlist(lapply(doc, function(x) x$db)))
    missing_db <- all_db[!all_db %in% ls(envir = .GlobalEnv, all.names = TRUE)]
    if(length(missing_db) > 0) {
        stop("can't find:\n",
             paste0(missing_db, collapse = ", "),
             "\n            in global enviroment")
    }
    identicalid <- length(id.name) == 1
    the_id <- id.name[[1]]
    DF <- data.frame(set)
    names(DF) <- the_id
    for(indx in seq_along(doc)){ # indx <- 1
        cat(paste0("Fixing variable no.", indx, ": ", names(doc)[indx], "\n"))
        X <- doc[[indx]]
        var <- X$var
        name <- X$name
        df <- X$db
        tmp <- get(df)[[var]]
        if(!is.null(f <- X$transf.fnc)) tmp <- f(tmp)
        if(!is.null(recode <- X$recode)) tmp <- recode(x=tmp, L=recode)
        loc.df <- data.frame(
            tmp,
            get(df)[[if(identicalid) the_id else id.name[df][[1]]]]
        )
        names(loc.df) <- c(name, the_id)
        DF <- merge(x = DF, y = loc.df, by.x = the_id, all.x = TRUE)
    }
    for(nm in names(DF)){
        lab <- pdoc$label[pdoc$name == nm]
        if(length(lab) == 0 || lab == "") next else attr(DF[[nm]], "label") <- lab
    }
    DF
}
