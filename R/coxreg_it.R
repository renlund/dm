##' iterated use of \code{rms::cph}
##'
##' run the same (RHS) formula for multiple outcomes and record models, effect,
##'     residuals and continuous effects
##' @param data a data frame
##' @param surv outcomes, as surv objects or specification of the outcome names
##'     where the time- and event components differ only in prefix
##' @param fRHS right hand side of formula (do not include '~')
##' @param cont names of variables (included in fHRS) for which the actual
##'     effects are wanted (typically continuous ones, the others are recorded anyway)
##' @param w weights
##' @param prefix prefix consistently used for time- and event components of
##'     (what is to become) Surv objects (only needed when surv does not
##'     indicated Surv variables already)
##' @return a list with components
##' \itemize{
##'  \item{"model"}{list of all models created, named from surv argument}
##'  \item{"HR"}{data frame containing all model effects}
##'  \item{"resid"}{list of \code{cox.zph} objects, named from surv argument}
##'  \item{"effect"}{data frame of (continuous) effects}
##' }
##' @export
cph_it <- function(data, surv, fRHS, cont = NULL, w = NULL,
                   prefix = c("time" = "t.", "event" = "ev.")){
    if(is.null(options("datadist")$datadist)){
        m <- paste0("this function requires the usual rms setup ",
                    "including setting > options('datadist')\n")
        stop(m)
    }
    ## nm_surv <- if(is.null(names(surv))) surv else names(surv)
    surv <- as.character(surv)
    if(is.null(w)) w <- rep(1, nrow(data))
    if(class(data[[surv[1]]]) != "Surv"){
        for(i in seq_along(surv)){
            tnm <- paste0(prefix['time'], surv[i])
            enm <- paste0(prefix['event'], surv[i])
            data[[surv[i]]] <- survival::Surv(time = tnm, event = enm)
        }
    }
    M <- R <- as.list(NULL)
    C <- P <- NULL
    for(i in seq_along(surv)){ ## i = 1
        utf <- surv[i]
        ## calculate model
        mod <- rms::cph(stats::formula(paste0(utf, " ~ ", fRHS)), data = data,
                        x = TRUE, y = TRUE, weights = w)
        M[[utf]] <- mod
        ## get model coefficients
        mdf <- cph2df(mod)
        mdf$outcome <- utf
        C <- if(is.null(C)) mdf else rbind(C, mdf)
        ## get Schoenfeldt residuals
        R[[utf]] <- survival::cox.zph(mod)
        ## get spline effects
        pred <- NULL
        for(j in seq_along(cont)){ ## j = 1
            p <- as.data.frame(do.call(what = rms::Predict,
                                       args = list('x' = mod, cont[j])))
            attr(p, "out.attrs") <- NULL
            attr(p, "info") <- NULL
            p$outcome <- utf
            p$term <- cont[j]
            p$HR <- exp(p$yhat)
            p$conf.low <- exp(p$lower)
            p$conf.high <- exp(p$upper)
            pred <- if(is.null(pred)) p else rbind(pred, p)
        }
        P <- if(is.null(P)) pred else rbind(P, pred)
    }
    list('model' = M,
         'HR' = C,
         'resid' = R,
         'effect' = P)
}

if(FALSE){

    ## data = readRDS("ignore/coxreg_it-data.Rds")
    ## tmp <- data; tmp[] <- lapply(data, function(x){if(class(x) == "Surv") NULL else x})
    ## dd <- datadist(tmp)
    ## options(datadist = "dd")
    ## surv = c("All-cause mortality" = "ACM", "MyoInf" = "MI")
    ## fRHS = "rcs(oid.age) + rcs(calendar) + male + Diabetes"
    ## w = NULL
    ## cont = c("Calendar time" = "calendar", "Age" = "oid.age")
    ## r <- cph_it(data = data,
    ##             surv = c("All-cause mortality" = "ACM", "MyoInf" = "MI"),
    ##             fRHS = "rcs(oid.age) + rcs(calendar) + male + Diabetes",
    ##             w = NULL,
    ##             cont = c("Calendar time" = "calendar", "Age" = "oid.age"))

}

##' cph to data frame
##'
##' convert a \code{rms::cph} object to a data frame
##' @param cph an object created by \code{rms::cph}
##' @param useg logical; if \code{TRUE} the function will try harder to add
##'     info from \code{anova(cph)} onto info from summary(cph)
##' @param ... arguments passed to \code{summary}
##' @return a data.frame
##' @importFrom stats anova
##' @export
cph2df <- function(cph, useg = TRUE, ...){
    if(!any(grepl("package:rms", search()))){
        message("might want to 'library(rms)' at this point\n")
    }
    s <- summary(cph, ...)
    a <- anova(cph)
    adf <- as.data.frame(a)
    adf$term <- rownames(adf)
    rownames(adf) <- NULL
    dn1 <- dimnames(s)[[1]]
    hri <- dn1 == " Hazard Ratio"
    sdf <- as.data.frame(s[hri, c("Low", "High", "Effect", "Lower 0.95",
                                  "Upper 0.95")])
    names(sdf) <- c("low", "high", "HR", "conf.low", "conf.high")
    rownames(sdf) <- NULL
    r0 <- cbind(data.frame(term = dn1[!hri]), sdf)
    if(useg){
        r0$tmp <- unlist(lapply(r0$term,
                                function(x) strsplit(x, split = " ")[[1]][1]))
        ut <- merge(r0, adf, all.x = TRUE, by.x = "tmp", by.y = "term")
        ut$tmp <- NULL
        ut
    } else{
        merge(r0, adf, all.x = TRUE, by = "term")
    }
}

##' @describeIn cph_it example  of code to loop out results  into a rnw document
##'     (requires subdirectory 'saved-figure' but this is easily modified)
##' @export
cph_it_display_code <- function(){
cat("
           ###############################
           ##   COPY-PASTE and MODIFY   ##
           ###############################

CI ## <- object created by coxreg_it
pref ## <- prefix to distinguish filenames and labels

## helper functions
gtxt <- function(file, caption, label){
    paste0(\"\n\\begin{center}\\begin{figure}[htb]\n\",
           \"\\includegraphics{\", file, \"}\n\",
           \"\\caption{\", caption, \"}\n\",
           \"\\label{fig:\", label, \"}\n\",
           \"\\end{figure}\\end{center}\n\")
}
zph2df <- function(z, long = TRUE){
    n <- dim(z$y)[2]
    r <- cbind(data.frame(x = z$x), as.data.frame(z$y))
    if(long){
        reshape(r,
                varying = 2:(n+1),
                v.names = \"Residual\",
                timevar = \"Variable\",
                times = names(r)[2:(n+1)],
                direction = \"long\")
    } else {
        r
    }
}
## preliminaries
outcSet <- names(CI$model)
eff <- CI$effect
term <- unique(eff$term)
## loop for results
cat(\"\n\\clearpage\n\")
for(i in seq_along(outcSet)){
    outc <- outcSet[i]
    cat(\"\n\\subsection{Outcome \", outc, \"}\n\")
    ## summary of model  -----------------------------------------------
    s <- summary(CI$model[[outc]])
    Hmisc::latex(s, file = \"\", where = \"htb\",
                 label = paste0(\"tab:\", pref, \"-sum-\", outc))
    ## plot of covariate HR  -------------------------------------------
    hr <- subset(CI$HR, outcome == outc)
    p <- ggplot(hr, aes(y = term, x = HR, xmin = conf.low, xmax = conf.high)) +
        geom_vline(xintercept = 1, lty = 2) +
        geom_errorbarh() +
        labs(y = NULL, x = \"Hazard Ratio\") +
        geom_point()
    fn <- paste0(\"saved-figure/\", pref, \"-HR-\", outc, \".pdf\")
    lab <- paste0(\"fig:\", pref, \"-HR-\", outc)
    cap <- paste0(\"HR in analysis on outcome \", outc, \".\")
    ggsave(plot = p, filename = fn)
    cat(gtxt(file = fn, caption = cap, label = lab))
    ## continuous effects plot -----------------------------------------
    EFF <- NULL
    for(j in seq_along(term)){
        t <- term[j]
        d <- subset(eff, term == t & outcome == outc,
                    select = c(t, \"term\", \"HR\", \"conf.low\", \"conf.high\"))
        names(d)[1] <- \"variable\"
        EFF <- if(is.null(EFF)) d else rbind(EFF, d)
    }
    p <- ggplot(EFF, aes(variable, HR, ymax = conf.high, ymin = conf.low)) +
        geom_ribbon(alpha = 1/2) +
        geom_line(size = 1) +
        labs(x = \"Covariate value\", y = \"Log relative scale\") +
        facet_wrap( ~ term, scales = \"free\")
    fn <- paste0(\"saved-figure/\", pref, \"-eff-\", outc, \".pdf\")
    lab <- paste0(\"fig:\", pref, \"-eff-\", outc)
    cap <- paste0(\"Relative effects of numeric variables on outcome \", outc, \".\")
    ggsave(plot = p, filename = fn)
    cat(gtxt(file = fn, caption = cap, label = lab))
    ## residual plot ----------------------------------------------------
    res <- CI$resid[[outc]]
    z <- zph2df(res)
    p <- ggplot(z, aes(x, Residual)) +
        geom_point(alpha = 1/10) +
        geom_smooth(color = \"red\", method = \"gam\") +
        facet_wrap( ~ Variable, scales = \"free\")
    fn <- paste0(\"saved-figure/\", pref, \"-res-\", outc, \".png\")
    lab <- paste0(\"fig:\", pref, \"-res-\", outc)
    cap <- paste0(\"Schoenfeld residuals for outcome \", outc, \".\")
    ggsave(plot = p, filename = fn, height = 9, dpi = 6*72)
    cat(gtxt(file = fn, caption = cap, label = lab))
    cat(\"\n\\clearpage\n\")
}
")
}
