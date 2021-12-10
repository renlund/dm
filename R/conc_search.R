##' pattern helper (specific)
##'
##' get the pattern for correct (simple) searches, characterized by providing the
##' initial sequence of the string wanted, where the code searched is
##' concatenated. Note: this function mainly serves as an example for the author
##' to remember how to do this! Be aware if your search strings are more
##' complicated as the author is no expert in regular expression.
##' @param s string to match initial part of code
##' @param code.sep the string that servers as separator in the concatenation
##' @examples
##' x <- c("ABC", "", "FFF XXX", "GGG  ABC FOO",
##'        "BAR  ABC", "FOO BAR ABB", " ", ";")
##' s.term = c("ABC", "X")
##' grepr(pattern = pattern_conc_search(s.term), x = x)
##' ## sample example, different separator:
##' y <- gsub(pattern = " ", replacement = ";", x)
##' grepr(pattern = pattern_conc_search(s.term, code.sep = ";"), x = y)
##' @export
pattern_conc_search <- function(s, code.sep = " "){
    paste0(sprintf(paste0("(%s%s)|(^%s)"), code.sep, s, s), collapse = "|")
}


##' first match from \code{pattern_conc_search}
##'
##' use \code{pattern_conc_search} but also extract the match from within the
##' concatenation. Note to self: the use of \code{pattern_conc_search} is
##' superfluous within this function as it applies searches to the string which
##' is split at the \code{code.sep}, but makes it easier to use search terms
##' defined as variables
##' @param x (concatenated) search string
##' @param s string to match initial part of code
##' @param code.sep the string that servers as separator in the concatenation
##' @param reduce if \code{FALSE}, always return string of same length as x
##' @param s.fixed set to \code{TRUE} if 's' was created by
##'     \code{pattern_conc_search}
##' @examples
##' x <- c("ABC", "", "FFF XXX", "GGG  ABC FOO",
##'        "BAR  ABC", "FOO BAR ABB", " ", ";")
##' s.term = c("AB(C|B)", "X")
##' ## extract only the (first) matches
##' extract_match1_conc_search(x, s = s.term, reduce = TRUE)
##' ## extract but do not reduce:
##' data.frame(x = x,
##'            extr.m1 = extract_match1_conc_search(x, s = s.term,
##'                                                 reduce = FALSE))
##' ## same example, different separator:
##' y <- gsub(pattern = " ", replacement = ";", x)
##' data.frame(y = y,
##'            extr = extract_match1_conc_search(y, s = s.term,
##'                                              code.sep = ";", reduce = FALSE))
##' @export
extract_match1_conc_search <- function(x, s, code.sep = " ", reduce = FALSE,
                                       s.fixed = FALSE){
    x1 <- strsplit(x = x, split = code.sep)
    p <- if(s.fixed) s else pattern_conc_search(s = s, code.sep = code.sep)
    foo <- function(z){
        g <- grepr(pattern = p, x = z)[1]
        if(is.null(g) & !reduce) NA else g
    }
    unlist(lapply(X = x1, FUN = foo))
}
