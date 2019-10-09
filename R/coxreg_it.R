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
##' @return a data.frame
##' @importFrom stats anova
##' @export
cph2df <- function(cph){
    if(!any(grepl("package:rms", search()))){
        message("might want to 'library(rms)' at this point\n")
    }
    s <- summary(cph)
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
    r <- merge(r0, adf, all.x = TRUE, by = "term")
}
