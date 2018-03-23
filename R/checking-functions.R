.required_properties <- function(x, class = NULL, length = NULL, nm = NULL){
    if(is.null(nm)) nm <- as.character(substitute(x))
    if(!is.null(class)){
        s <- paste0("Argument '", nm, "' fails to be in class: ",
                    paste0(class, collapse = ", or"))
        if(!any(class(x) %in% class)) stop(s)
    }
    if(!is.null(length)){
        s <- paste0("Argument '", nm, "' fails to be have length: ",
                    paste0(length, collapse = ", or"))
        if(!length(x) %in% length) stop(s)
    }
}

.required_data_names <- function(data.names, required){
    badname.indx <- which(!required %in% data.names)
    if(length(badname.indx) > 0){
        stop("Some variable names required (specifically: ",
             paste(required[badname.indx], collapse = ", "),
             ") does not exist in data.")
    }
    invisible(NULL)
}

.not_allowed_names <- function(nm, no){
    badname.indx <- which(nm %in% no)
    if(length(badname.indx) > 0){
        stop("Name conflict, don't want ",
             paste0(nm, collapse = "; "), " and ",
             paste0(no, collapse = "; "), " to intersect.")
    }
    invisible(NULL)
}

.rename_if_in <- function(nm, compare, prefix = '.', suffix = NULL,
                          all = FALSE, limit = 10, verbose = TRUE){
    if(length(prefix) > 1 | length(suffix) > 1) stop("bad prefix or suffix")
    nm.org <- nm
    rename <- which(nm %in% compare)
    if(length(rename) == 0){
        NULL
    } else {
        if(verbose) message(" (!) I will try to alter some variable names")
        dummy <- 0
        while(length(rename) > 0 & dummy < limit){
            if(all){
                nm <- paste0(prefix, nm, suffix)
            } else {
                nm[rename] <- paste0(prefix, nm[rename], suffix)
            }
            rename <- which(nm %in% compare)
            dummy <- dummy + 1
        }
        if(length(rename) > 0) {
            stop("Renaming of conflicting variables failed.")
        }
        if(verbose){
            i <- which(nm.org != nm)
            x <- paste(paste0(nm.org[i], " -> ", nm[i]), collapse = ", ")
            message("     Name changes: ", x)
        }
        nm
    }
}
