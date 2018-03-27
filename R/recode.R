#' @title manipulate factor levels
#' @description This functions enables relabeling, reordering and merging of factor levels.
#' @param x factor or character vector to be relabelled or reordered
#' @param L a list specifying the relabeling/reordering, list names specify a
#'     new (or old) name and the list elements are vectors of (old) values that
#'     are to be assigned that name. The order of L is the order of the levels
#'     (with old values 'outside' the list either after of before, depending on
#'     the \code{newFirst} value). List elements \code{=NULL} just means that
#'     this name is to be unchanged but be placed in that place (order-wise).
#' @param asFactor if \code{TRUE} then a factor is returned (default \code{TRUE})
#' @param newFirst if \code{TRUE} then added levels will be placed first in the list of levels (default \code{TRUE})
#' @param ... dcode arguments passed to recode
#' @examples
#' x <- LETTERS[2:1]
#' factor(x) ## levels in alphabetical order
#' recode(x, L = list('B' = NULL)) ## makes 'B' be the first level
#' x <- c('', LETTERS[1:5], NA)
#' factor(x)
#' L <- list('new_BC_or_NA' = c('B', 'C', NA),
#'           'E' = NULL,
#'           'new_D_(level_after_E)' = 'D',
#'           'blank' = '')
#' y <- recode(x, L, newFirst = FALSE) ## all unspecified values comes first
#' levels(y)
#' table(y, x, useNA = 'ifany')
#' @seealso \code{\link{factor}}, \code{\link{relevel}}
#' @export
recode <- function(x, L, asFactor = TRUE, newFirst = TRUE){
   if( !(is.numeric(x) | is.character(x) | is.factor(x) | is.logical(x)) ){
      stop("[recode] 'x' is neither factor, character, numeric or logical")
   }
   if(!is.logical(asFactor)) stop("[recode] 'asFactor' should be logical")
   if(!is.logical(newFirst)) stop("[recode] 'newFirst' should be logical")
   char_x <- as.character(x)
   fact_x <- if(is.factor(x)) x else factor(char_x)
   lev <- levels(fact_x)
   if(missing(L)) return(if(asFactor) fact_x else char_x)
   if(!is.list(L)) stop("[recode] 'L' needs to be a list.")
   L_names <- names(L)
   L_entries <- unlist(L, use.names = FALSE)
   if(any(duplicated(L_names))) warning("[recode] duplicated names in 'L'")
   if(any(duplicated(L_entries))) warning("[recode] duplicated entries in 'L'")
   ignored <- setdiff(lev, c(L_names, L_entries)) ## what happens to NA?
   copy_x <- char_x
   for(nm in L_names){
       if(is.null(L[[nm]])) next
       copy_x[char_x %in% L[[nm]]] <- nm
   }
   if(asFactor){
       new_lev <- if(newFirst) c(L_names, ignored) else c(ignored, L_names)
       factor(copy_x, levels = new_lev)
   } else {
       copy_x
   }
}

##' @describeIn recode 'dcode' is an alias for 'recode' since the latter is in
##'     use in some proper R package
##' @export
dcode <- function(...) recode(...)

## below is a code chunk for checking things related to the recoding, but
## it seems to be superfluous

   ## if(check){ ## think this is NOT necessary
   ##     if(any(L_names %in% L_entries)){
   ##         message("[recode] non-empty intersection of names and entries in 'L")
   ##     }
   ##     indx <- unlist(lapply(L, is.null))
   ##     L_null <- L_names[indx]
   ##     if(length(L_null)>0){
   ##         if(!all(L_null %in% lev)){
   ##             message("[recode] some elements of L are superfluous")
   ##         }
   ##     }
   ##     if(!is.null(L_entries)){
   ##         e_nona <- L_entries[!is.na(L_entries)]
   ##         if(!all(e_nona %in% lev)){
   ##             overload <- setdiff(e_nona, lev)
   ##             indx <- unlist(lapply(L, function(x) all(x %in% overload)))
   ##             w2 <- if(any(indx)){
   ##                       paste0("L elements: ",
   ##                              paste0(L_names[indx], collapse = ", "),
   ##                              ", only contain entries not in x.")
   ##                   } else ""
   ##             w1 <- paste0("Elements: ", paste0(overload, collapse = ", "),
   ##                          " (from L) are not in x. ")
   ##             message("[recode] L is overloaded. ", w1, w2)
   ##         }
   ##     }
   ## }
