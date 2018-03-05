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
    gr = sample(c('A', 'A2', 'B', 'C', 'D', 'd1', 'unknown'),
                n, TRUE),
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
## 'gr' will be recoded in a more complex way
L <- list('A' = 'A2',
          'B' = NULL, ## placeholder to get the order right
          'CD' = c('C', 'D', 'd1'),
          'Unknown' = c('unknown', NA))
## # this would also work:
## L <- list('A' = c('A', 'A2'),
##           'B' = 'B',
##           'CD' = c('C', 'D', 'd1', 'something not in data'),
##           'Unknown' = c('unknown', NA))
dm('gr', recode = L, label = 'Group')

## ----"add-rest", echo = TRUE, results = 'hide'---------------------------
dm('aalder', 'Age')
dm('nar', 'When', comment = "wtf?")
dm('foo', 'event', db = 'COMP',
   recode = list('No' = '0', 'Yes' = 1),
   label = "An event at some time")
dm('bar', 'time', db = 'COMP', transf = log)
dm('koon', 'Gender',
   recode = list('Male' = 'M', 'Female' = 'K'))

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

## ----'generate-data'-----------------------------------------------------
POP <- data.frame(
    id = c('Anna', 'Barry', 'Christina',
           'David', 'Esteban'),
    enter = as.Date(c('2010-01-01', '2010-02-01', '2010-03-01',
                      '2010-04-01', '2010-05-01'))
)
RECORDS <- data.frame(
    identity = c('Barry', 'Christina', 'David',
           'David', 'David', 'Esteban',
           'Other', 'Other'),
    what1 = c('headache', 'bar type I', 'nausea',
             'percutaneous foo', 'quuz', '',
             'other foo', 'other bar'),
    what2 = c('mild foo', 'bar type II', 'severe bar',
             'subcutaneous foo', NA, 'bar-ish',
             'yet other foo', 'yet other bar'),
    what.date = as.Date(c('2010-01-07', '2010-07-23', '1998-06-27',
                          '1996-10-12', '2011-01-18', '2011-05-03',
                          '1999-12-01', '2010-06-01'))
)[sample(1:8),]
options('knitr.kable.NA' = '')

## ----'show-POP'----------------------------------------------------------
POP
RECORDS

## ----'find-mh'-----------------------------------------------------------

searchString <- c('Foo' = 'foo', 'Bar' = 'bar', 'Quuz' = 'quuz')

tm <- grepict(
    pattern = searchString, ## what to search for
    x = c('what1', 'what2'), ## search variables in 'data'
    data = RECORDS, ## data set to search in
    id = 'identity', ## name of id variable in 'data'
    date = 'what.date', ## name of date variable in 'data'
    units = POP, ## data set, or vector, containing individuals
    units.id = 'id', ## name of id variable in 'units'
    begin = NULL, ## earliest date to search from
    end = 'enter', ## name of lates date to search,
    ## long = TRUE, ## long output format is default
    ## stack = TRUE, ## stacked results are default
    verbose = FALSE ## give calculation progress info?
)


## ----'show-mh', results = 'asis'-----------------------------------------
tm[, c('id', 'event', 'alias', 'match', 'match.in', 'first.id')]

## ----'mh-fix'------------------------------------------------------------
tmp <- subset(tm, first.id == 1, select = c('id', 'event', 'alias'))
(medhist <- reshape(tmp, idvar = 'id', timevar = c('alias'), direction = 'wide'))
names(medhist) <- gsub("event", "prior", names(medhist), fixed = TRUE)

## ----'show-mh-fixed'-----------------------------------------------------
medhist

## ----'find-outcomes'-----------------------------------------------------

POP$endofstudy <- POP$enter + 365
tm2 <- grepict(pattern = searchString, x = c('what1', 'what2'),
                  data = RECORDS, id = 'identity', date = 'what.date',
                  units = POP, units.id = 'id',
                  begin = 'enter', ## earliest date to search from
                  end = 'endofstudy', ## name of lates date to search,
                  verbose = FALSE)


## ----'show-outcomes'-----------------------------------------------------
tm2[, c('id', 'event', 'time', 'alias', 'match', 'match.in')]

## ----'outcomes-fix'------------------------------------------------------
tmp2 <- subset(tm2, first.id == 1, select = c('id', 'event', 'time', 'alias'))
(outcomes <- reshape(tmp2, idvar = 'id', timevar = c('alias'), direction = 'wide'))
names(outcomes) <- gsub("event", "ev", names(outcomes), fixed = TRUE)
names(outcomes) <- gsub("time", "t", names(outcomes), fixed = TRUE)

## ----'show-outcomes-fixed'-----------------------------------------------
outcomes

## ----'other'-------------------------------------------------------------
tm3 <- grepict(pattern = searchString, x = c('what1', 'what2'),
                  data = RECORDS, id = 'identity', date = 'what.date',
                  units = POP, units.id = 'id', begin = 'enter',
                  end = 'endofstudy',
                  long = FALSE, ## wide output format
                  stack = TRUE, ## stack
                  verbose = FALSE
)
str(tm3)

## ----'other-tab'---------------------------------------------------------
val <- c('id', 'alias', 'event', 'time', 'events', 'matches.info')
tm3[, val]

## ----'other2'------------------------------------------------------------
tm4 <- grepict(pattern = searchString, x = c('what1', 'what2'),
                  data = RECORDS, id = 'identity', date = 'what.date',
                  units = POP, units.id = 'id', begin = 'enter',
                  end = 'endofstudy',
                  long = FALSE, ## wide output format
                  stack = FALSE, ## don't stack
                  verbose = FALSE
)
str(tm4)

## ----'other2-tab'--------------------------------------------------------
val <- c('id', names(tm4)[grepl("event|time", names(tm4))])
tm4[, val[!grepl("Foo", val)]]

## ----"cdate"-------------------------------------------------------------
cdate(x = c("20010101", "20010100", "20010000"))
cdate(x = c("20010101", "20010100", "20010000"),
      low.bound = as.Date(c("1999-01-01", "2001-01-21", "2001-06-20")))

