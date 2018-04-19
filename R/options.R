## These function creates and handles overall dm options
## The options are stored in an environment 'dm_envir'

.dm_doc_empty <- structure(list(), class = c("dm_doc", "list"))
.dm_filter_empty <- structure(list(), class = c("dm_filter", "list"))
.dm_derive_empty <- structure(list(), class = c("dm_derive", "list"))

## @title dm_envir
## @description an environment
dm_envir <- new.env(parent = getNamespace("dm"))

# @title dm_restore
# @description this function restores the default dm settings
dm_restore <- function(){
    assign(x = "defaults",
           value = list("default_db" = NULL,
                        "documentation" = "dm_doc",
                        "filter" = "dm_filter",
                        "derive" = "dm_derive"),
           envir=dm_envir)
    assign(x = "values",
           value = names(get(x="defaults", envir=dm_envir)),
           envir = dm_envir)
    invisible(NULL)
}

# @title dm_set
# @description this function sets the dm settings
# @param ... the names and values you want set
dm_set <- function(...){
   if(length(ls(envir=dm_envir))==0) dm_restore()
   dots <- list(...)
   value <- get("values", dm_envir)
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- dm_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=dm_envir)
   invisible(NULL)
}

# @title dm_get
# @description this function retrieves the dm settings
# @param name name of desc setting variable
dm_get <- function(name){
   if(length(ls(envir=dm_envir))==0) dm_restore()
   defaults <- get("defaults", envir=dm_envir)
   if (missing(name))
      defaults
   else {
      L <- as.list(NULL)
      for(k in name){
         L[[k]] <- defaults[[k]]
      }
      if(length(L) == 0){
          message("this value has not been set")
          NULL
      }
      else if(length(L) == 1) L[[1]]
      else L
   }
}

#' @title desc options
#' @description This list tries to mimic the behavior of opts_chunk from knitr.
#' Currently these values are maintained with the functions in (the list)
#' \code{opts_desc}:
#' \itemize{
#' \item get - get the current values
#' \item set - set new values
#' \item restore - restore default values
#' }
#' @export
opts_dm <- list(
   "get" = dm_get,
   "set" = dm_set,
   "restore" = dm_restore
)

## @title variable dm_doc
## @description a list keeping the documentation of the data
##    management done by dm
assign(x = dm_get('documentation'),
       value = .dm_doc_empty,
       envir = dm_envir)

## @title variable dm_filter
## @description a list keeping the documentation of the filtering
assign(x = dm_get('filter'),
       value = .dm_filter_empty,
       envir = dm_envir)

## @title variable dm_derive
## @description a list keeping the documentation of the derived variables
assign(x = dm_get('derive'),
       value = .dm_derive_empty,
       envir = dm_envir)


##' get or reset documentation for dm
##'
##' get or reset the list that has documentet the data management process done by dm
##' @param kill if \code{TRUE} deletes all documentation
##' @param prompt if \code{TRUE} prompts before deleting
##' @export
dm_doc <- function(kill = FALSE, prompt = TRUE){
    if(kill){
        reset <- TRUE
        if(prompt){
            p <- paste0("WARNING: do you want to delete all dm documentation?\n",
                        "         ('y' for yes, anything else for no)\n\n",
                        "                                                     ")
            reset <- readline(prompt = p) == "y"
        }
        if(reset){
            assign(x = dm_get('documentation'),
                   value = .dm_doc_empty,
                   envir = dm_envir)
            cat(" ~ dm_doc has been nullified!\n")
        } else {
            cat(" ~ dm_doc is intact\n")
        }
        invisible(NULL)
    } else {
        get(x = dm_get('documentation'),
            envir = dm_envir)
    }
}

## add to dm documentation
##
## add list element to dm documentation (internal use)
## @param name name of new list element
## @param value value of new list element
dm_doc_set <- function(name, value){
    L <- dm_doc()
    L[[name]] <- value
    assign(x = dm_get('documentation'),
           value = L,
           envir = dm_envir)
    invisible(NULL)
}

##' get or reset documentation for filtering
##'
##' get or reset the list that has documentet a filtering process done by dmf
##' @param kill if \code{TRUE} deletes all documentation
##' @param prompt if \code{TRUE} prompts before deleting
##' @export
dm_filter <- function(kill = FALSE, prompt = TRUE){
    if(kill){
        reset <- TRUE
        if(prompt){
            p <- paste0("WARNING: do you want to delete all filter documentation?\n",
                        "         ('y' for yes, anything else for no)\n\n",
                        "                                                     ")
            reset <- readline(prompt = p) == "y"
        }
        if(reset){
            assign(x = dm_get('filter'),
                   value = .dm_filter_empty,
                   envir = dm_envir)
            cat(" ~ dm_filter has been nullified!\n")
        } else {
            cat(" ~ dm_filter is intact\n")
        }
        invisible(NULL)
    } else {
        get(x = dm_get('filter'),
            envir = dm_envir)
    }
}

## add to filter documentation
##
## add list element to filter documentation (internal use)
## @param name name of new list element
## @param value value of new list element
dm_filter_set <- function(name, value){
    L <- dm_filter()
    L[[name]] <- value
    assign(x = dm_get('filter'),
           value = L,
           envir = dm_envir)
    invisible(NULL)
}

##' get or reset documentation for derivation
##'
##' get or reset the list that has documented a derivation process
##' @param kill if \code{TRUE} deletes all documentation
##' @param prompt if \code{TRUE} prompts before deleting
##' @export
dm_derive <- function(kill = FALSE, prompt = TRUE){
    if(kill){
        reset <- TRUE
        if(prompt){
            p <- paste0("WARNING: do you want to delete all derive documentation?\n",
                        "         ('y' for yes, anything else for no)\n\n",
                        "                                                     ")
            reset <- readline(prompt = p) == "y"
        }
        if(reset){
            assign(x = dm_get('derive'),
                   value = .dm_derive_empty,
                   envir = dm_envir)
            cat(" ~ dm_derive has been nullified!\n")
        } else {
            cat(" ~ dm_derive is intact\n")
        }
        invisible(NULL)
    } else {
        get(x = dm_get('derive'),
            envir = dm_envir)
    }
}


## add to derive documentation
##
## add list element to derive documentation (internal use)
## @param name name of new list element
## @param value value of new list element
dm_derive_set <- function(name, value){
    L <- dm_derive()
    L[[name]] <- value
    assign(x = dm_get('derive'),
           value = L,
           envir = dm_envir)
    invisible(NULL)
}
