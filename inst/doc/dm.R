## ----'Setup', cache=FALSE, echo=FALSE, include=FALSE---------------------
rm(list=ls())
library(dm)
## .load_only <- TRUE
## if(.load_only){
##     devtools::load_all()
## } else {
##     library(dm)
## }
set.seed(19790424)
if(FALSE){ ## create document
    knitr::knit2pdf('dm.rnw')
}

## ----"no-eval-opts", echo = TRUE, eval = FALSE---------------------------
#  opts_dm$set('default_db' = 'MyDataBase')

## ----"no-eval-dm", echo = TRUE, eval = FALSE-----------------------------
#  dm(var = 'gEndEr', name = 'gender', label = "Perceived Gender")
#      ## is followed by information being printed

## ----"no-eval-dm2", echo = TRUE, eval = FALSE----------------------------
#  dm('gEndEr', 'gender', label = "Biological Gender") ## overwrites
#      ## the 'gender' entry

## ----"no-eval-dm3", echo = TRUE, eval = FALSE----------------------------
#  dm_doc(kill = TRUE, prompt = FALSE) ## or possibly kill only this
#    ## entry dm:::dm_doc_set('gender', NULL)

## ----"no-eval-doc", echo = TRUE, eval = FALSE----------------------------
#  myDoc <- dm_doc()
#  print(myDoc)  ## N.B not all information is printed

## ----"no-eval-create", echo = TRUE, eval = FALSE-------------------------
#  id_key = c('MyDataBase' = 'id', 'Other1' = 'ID', 'Other2' = 'idno')
#  ADB <- dm_create(set = MyDataBase$id, id.name = id_key)

## ----"no-eval-tables", echo = TRUE, eval = FALSE-------------------------
#  lapply(dm_doc(), FUN = function(x) x$recode_table)

## ----"create-data"-------------------------------------------------------
n <- 200
BL <- data.frame(
    id = structure(sprintf("id%d", 1:n),
                   label = "identification"),
    aalder = structure(round(rnorm(n, 50, 10)),
                       label = "Age at some time",
                       foo = 'whatever'),
    vikt = rpois(n, 50),
    gr = sample(c('A1', 'A2', 'B1', 'c', 'unknown'), n, TRUE),
    koon = structure(sample(c('M', 'K'), n, T),
                     label = "The Sex"),
    nar = as.Date("2001-01-01") + runif(n, 0, 3650),
    stringsAsFactors = FALSE
)
BL$vikt[sample(1:n, 15)] <- NA
BL$gr[sample(1:n, 10)] <- NA
m <- .9*n
COMP <- data.frame(
    ID = structure(sample(BL$id, m),
                   label = "identification"),
    foo = rbinom(m, 1, .2),
    bar = structure(rexp(m, 1/150),
                    label = "Time passed")
)

## ----"find"--------------------------------------------------------------
db_info(BL) ## prints names and 'label' attributes
dm_find(pattern = 'time') ## looks in names and labels

## ----"default"-----------------------------------------------------------
opts_dm$set('default_db' = 'BL')

## ----"add-gender"--------------------------------------------------------
dm('koon', 'Gender',
   recode = list('Male' = 'M', 'Female' = 'K'))

## ----"add-rest", echo = TRUE, results = 'hide'---------------------------
dm('aalder', 'Age')
dm('nar', 'When', comment = "wtf?")
dm('foo', 'event', db = 'COMP',
   recode = list('No' = '0', 'Yes' = 1),
   label = "An event at some time")
dm('bar', 'time', db = 'COMP', transf = log)
## 'gr' will be recoded in a more complex way
L <- list('A' = c('A1', 'A2'),
          'BC' = c('B1', 'c'),
          'Unknown' = c('unknown', NA))
dm('gr', recode = L, label = 'Group')

## ----"create"------------------------------------------------------------
ADB <- dm_create(set = BL$id,
                 id.name = c('BL' = 'id', 'COMP' = 'ID'))
db_info(ADB)

## ----"get-doc"-----------------------------------------------------------
## myDoc <- dm_doc()
dm_doc() ## only prints partial information in the doc

## ----"dm_doc2latex", results = 'asis'------------------------------------
dm_doc2latex(caption = "Variables and their origin.")

## ----"recoded", results = 'asis'-----------------------------------------
dm_recode2latex()

## ----"variable-overview",eval = FALSE------------------------------------
#  d <- print(dm_doc(), print = FALSE)
#  lapply(dm_doc(), FUN = function(x) x$recode_table)

