#' @title Data Management Tool (interactive-ish)
#' @description This creates a list of data management information (defaults to
#'     a 'dm_doc' object called 'documentation' in the environment 'dm_envir'
#'     which can be retrieved with \code{dm_doc()}). This information can then
#'     be used to extract and recode (factors and dates) for (or towards) an
#'     analytical data base.
#' @param var character, database/data frame entry
#' @param name character, (new) name of variable, will be 'var' if unspecified
#' @param db character, name of data frame, if \code{NULL}, will look for
#'     default setting in \code{opts_dm$get('default_db')}.
#' @param recode recode \code{L} argument for recoding
#' @param transf function, for transformation
#' @param comment character, a comment or some such extra information
#' @param label character, something to be stored as label attribute for this
#'     variable
#' @param keep.label logical, if \code{var} has a label in \code{db}, should it
#'     be kept?  (only if \code{label} is \code{NULL})
#' @export
dm <- function(var, name = var, db = NULL, recode = NULL, transf = NULL,
               comment = NULL, label = NULL, keep.label = TRUE){
    ## check that data base exists --------------------------------------------
    if(is.null(db)) db <- dm_get("default_db")
    if(is.null(db)){
        stop("'db' not given and no 'default_db' in options")
    } else {
        if(!is.character(db)){
            stop(paste0("data base should be specified as a character ",
                        "(the name of an object in the global workspace)"))
        }
        tmp <- ls(envir = .GlobalEnv, pattern = paste0("^", db, "$"))
        if(length(tmp) == 0){
            stop(paste0("'", db, "' not found in specified environment"))
        }
    }
    ## get variable and label --------------------------------------------------
    x <- get(db, envir = .GlobalEnv)[[var]]
    transf.txt <- NULL
    transf.char <- ""
    if(!is.null(transf)){
        if(!is.function(transf)){
            warning("transf is not a function and will be ignored")
        } else {
            transf.char <- paste(as.character(substitute(transf)),
                                 collapse = " ")
        }
        x <- transf(x)
        transf.txt <- paste0(" transformed by function:\n",
                             "      ", transf.char, "\n")
    }
    if(is.null(x)){
        warning(paste0("no variable '", var, "' in data base '", db, "'."))
        return(invisible(NULL))
    }
    class_x <- class(x)
    attrib <- names(attributes(x))
    attr_text <- if(is.null(attrib)) NULL
                 else paste0("\n    with attributes: ",
                             paste0(attrib, collapse = ", "))
    label_db <- attr(x, "label")
    label_text <- if(is.null(label_db)) NULL
                  else paste0("\n          and label: '", paste0(label_db), "'")
    label_x <- if(!is.null(label)){
                   label
               } else if(keep.label){
                   label_db
               } else NULL
    ## print general info 1----------------------------------------------------
    txt <- paste0(
        paste(rep("-", max(options("width")[[1]]-12, 5)), collapse=""), "\n",
        "Adding data base '", db,"' entry '", var, "' ", transf.txt,
        "as variable '", name, "'\n",
        "A variable of class: ", paste(class_x, collapse = ";"),
        attr_text, label_text, "\n"
    )
    cat(txt)
    ## print general info 2----------------------------------------------------
    n_miss <- sum(is.na(x))
    perc_miss <- signif(100 * n_miss / length(x), 2)
    x2 <- x[!is.na(x)]
    n_unique <- length(unique(x2))
    perc_unique <- signif(100 * n_unique / length(x2), 2)
    cat(paste0("There are ", n_miss, " (", perc_miss, " percent) missing ",
               "\n      and ", n_unique, " (", perc_unique,
               " percent) unique values\n"))
    if(n_unique<20){
        cat(paste0("Since there are less than 20 unique vales",
                   " we tabulate them: \n\n\n"))
        print(table(x, useNA="always", dnn = NULL))
    } else if(!any(class_x %in% c("numeric", "integer", "Date", "POSIXct"))){
        y <- x2[!grepl("^ *$", x2)]
        n_unique2 <- length(unique(y))
        if(n_unique2 ==0){
            cat("There are only NA, empty or space values!\n")
        } else if(n_unique<20){
            cat(paste0("There are ", n_unique2, "non-NA, non-empty, or ",
                       "non-space values, tabulated: \n\n\n"))
            print(table(y, useNA="no", dnn = NULL))
        } else {
            a <- paste0(utils::head(y, n = 10), collapse = ", ")
            b <- paste0(utils::tail(y, n = 10), collapse = ", ")
            cat(paste0("\nThe first and last (at most 10 each) non-NA, ",
                       " non-empty, and non-space values are:\n   ",
                       a, "\n      and\n   ", b, ",\n  respectively."))
        }
    }
    ## print info if numerical------------------------------------------------
    if(any(class_x %in% c("numeric", "integer"))){
        txt <- paste0(
            "\nSummary of numeric variable:",
            "\n    min:    ", signif(min(x, na.rm=TRUE), 3),
            "\n    max:    ", signif(max(x, na.rm=TRUE), 3),
            "\n    mean:   ", signif(mean(x, na.rm=TRUE), 3),
            "\n    median: ", signif(stats::median(x, na.rm=TRUE), 3), "\n"
        )
        cat(txt)
    }
    ## print info if date------------------------------------------------------
    if(any(class_x %in% c("Date", "POSIXct"))){
        cat("\nDates span from min =", as.character(min(x, na.rm = TRUE)),
            "to max = ", as.character(max(x, na.rm = TRUE)), "\n")
    }
    ## display recoding--------------------------------------------------------
    if(!is.null(recode)){
        cat("\nCross-tabulating the recoding: \n\n")
        print(
            recode_table <- table(
                x,
                recode(x = x, L = recode),
                dnn = c(var, name),
                useNA="ifany"
            )
        )
    }
    ## assign new value to dm_doc----------------------------------------------
    L <- list(
        name = name,
        var = var,
        db = db,
        transf = transf.char,
        comment = comment,
        label = label_x,
        transf.fnc = transf,
        recode = recode,
        recode_table = if(!is.null(recode)) recode_table else NULL
    )
    dm_doc_set(name = name, value = L)
    invisible(NULL)
}

##' print 'dm_doc' object
##'
##' prints a data frame version of selected info in a 'dm_doc' object or returns
##'     that data frame
##' @param x a 'dm_doc' object
##' @param ... arguments passed to print.data.frame
##' @param print if \code{FALSE} then an data frame is returned
##' @return possibly a data frame
##' @export
print.dm_doc <- function(x, ..., print = TRUE){
    X <- Reduce(rbind,
                lapply(dm_doc(), function(x){
                    y <- x[c('name', 'var', 'db', 'transf', 'label', 'comment')]
                    z <- lapply(y, function(x) if(is.null(x)) "" else x)
                    as.data.frame(z)
                }))
    X[] <- lapply(X, function(x) as.character(x))
    if(print) {
        print(X, ...)
        invisible(NULL)
    } else X
}

##' @title documentation as LaTeX
##' @description print information from documentation in LaTeX code
##' @param doc documentation
##' @param file argument for \code{Hmisc::latex}, default "" (empty string)
##' @param where argument for \code{Hmisc::latex}, default "htb"
##' @param rowname argument for \code{Hmisc::latex}, default \code{NULL}
##' @param ... passed to \code{Hmisc::latex}
##' @param which columns to print
##' @param code.key formatting code key
##' @export
dm_doc2latex <- function(doc = NULL,
                         file = "", where = "htb", rowname = NULL, ...,
                         which = c('name', 'var', 'db', 'label', 'comment'),
                         code.key = c('name' = '\\texttt',
                                      'var'  = '\\texttt',
                                      'db'   = '\\textbf',
                                      'comment' = '\\emph')){
    if(is.null(doc)){
        doc <- print(dm_doc(), print = FALSE)
    } else if(is.list(doc)) doc <- print(doc, print = FALSE)
    if(length(doc) == 0) stop("[dm_create] doc empty")
    i <- which %in% names(doc)
    v <- which[i]
    if(length(v) == 0) stop("failure")
    d <- subset(doc, select = v)
    for(K in names(d)){
        if(K %in% names(code.key)){
            d[[K]] <- texify(d[[K]], f = code.key[K])
        } else next
    }
    Hmisc::latex(d, file = file, where = where, rowname = rowname, ...)
}
