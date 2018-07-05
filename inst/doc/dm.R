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
#  dmv(var = 'gEndEr', name = 'gender', label = "Perceived Gender")
#      ## is followed by information being printed

## ----"no-eval-dm2", echo = TRUE, eval = FALSE----------------------------
#  dmv('gEndEr', 'gender', label = "Biological Gender") ## overwrites
#      ## the 'gender' entry

## ----"no-eval-dm3", echo = TRUE, eval = FALSE----------------------------
#  dm_doc(kill = TRUE, prompt = FALSE) ## or possibly kill only this
#    ## entry dm:::dm_doc_set('gender', NULL)

## ----"no-eval-doc", echo = TRUE, eval = FALSE----------------------------
#  myDoc <- dm_doc()
#  print(myDoc)  ## N.B not all information is printed

## ----"no-eval-create", echo = TRUE, eval = FALSE-------------------------
#  id_key = c('MyDataBase' = 'id', 'Other1' = 'ID', 'Other2' = 'idno')
#  CDB <- dm_create(set = MyDataBase$id, id.name = id_key)

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
dmv('gr', recode = L, label = 'Group', group = 'Foo things')

## ----"add-rest", echo = TRUE, results = 'hide'---------------------------
dmv('aalder', 'Age', group = 'Bar stuff')
dmv('nar', 'When', comment = "wtf?", group = 'Bar stuff')
dmv('foo', 'event', db = 'COMP',
   recode = list('No' = '0', 'Yes' = 1),
   label = "An event at some time", group = 'Foo things')
dmv('bar', 'time', db = 'COMP', transf = log, group = 'Bar stuff')
dmv('koon', 'Gender',
   recode = list('Male' = 'M', 'Female' = 'K'), group = 'Bar stuff')

## ----"create"------------------------------------------------------------
CDB <- dm_create(set = BL$id,
                 id.name = c('BL' = 'id', 'COMP' = 'ID'))
db_info(CDB) ## look at what we've created

## ----"get-doc"-----------------------------------------------------------
## myDoc <- dm_doc()
dm_doc() ## only prints partial information in the doc

## ----"variable-overview", eval = FALSE-----------------------------------
#  pdoc <- print(dm_doc(), print = FALSE)
#  rtables <- lapply(dm_doc(), FUN = function(x) x$recode_table)

## ----"dm_doc2latex", results = 'asis'------------------------------------
## dm_doc2latex(doc = myDoc)
dm_doc2latex(caption = "Variables and their origin.")

## ----"recoded", results = 'asis'-----------------------------------------
## dm_recode2latex(doc = myDoc)
dm_recode2latex()

## ----"dmf"---------------------------------------------------------------
dmf(f = CDB$gr != 'Unknown', name = 'crit_knowngr',
    comment = "group must be known")
dmf(f = CDB$Age >= 20 & CDB$Age <= 80, name = "crit_age",
    comment = "ages between 20 and 80")
dmf(f = CDB$When >= as.Date("2002-01-01") &
        CDB$When <= as.Date("2009-12-31"),
    name = "crit_date",
    comment = "study period 2002-2009")

## ----"dmf_doc"-----------------------------------------------------------
dm_filter()

## ----"dmf_doc2"----------------------------------------------------------
print(dm_filter(), seq = c(2,3,1))

## ----"dmf-list", results = 'asis'----------------------------------------
dm_filter2latexlist()

## ----"dmf-clust", fig.cap = "Test of cluster description"----------------
dm_filter2dist(plot = TRUE)

## ----"dmd"---------------------------------------------------------------
ADB <- subset(CDB, dmf_create())
ADB$age.gr <- dmd('age.gr',
                  expr = cut(x = ADB$Age, breaks = c(0, 65, Inf),
                             labels = c('young', 'old')),
                  dmd = 'Age groups, below 65 is young, else old.')
ADB$score <- with(ADB,
                  expr = dmd('score',
                             ifelse(gr %in% c('A', 'B') & Gender == 'Male', 1, 0),
                             dmd = 'A score, no sense required.'))
foo <- function(X){
    X$OrderInGroup <- dmd('OrderInGroup', expr = order(X$Age),
                          dmd = "The order of age within subgroup 'gr'.")
    X
}
ADB <- do.call(rbind, lapply(split(ADB, ADB$gr), foo))

## ----"dmd-show"----------------------------------------------------------
dm_derive()

## ----'generate-data'-----------------------------------------------------
POP <- data.frame(
    id = c('Anna', 'Barry', 'Christina',
           'David', 'Esteban'),
    enter = as.Date(c('2010-01-01', '2010-02-01', '2010-03-01',
                      '2010-04-01', '2010-05-01'))
)
RECORDS <- data.frame(
    identity = c('Barry', 'Barry',
                 'Christina',
                 'David', 'David', 'David',
                 'Esteban', 'Esteban',
                 'Other', 'Other'),
    what1 = c('headache', 'foo X',
              'bar type I',
              'nausea', 'percutaneous foo', 'quuz',
              '', 'enui',
              'other foo', 'other bar'),
    what2 = c('mild foo', NA,
              'bar type II',
              'severe bar', 'subcutaneous foo', NA,
              'bar-ish', 'foo Y',
              'yet other foo', 'yet other bar'),
    what.date = as.Date(c('2010-01-07', '2010-02-01',
                          '2010-07-23',
                          '1998-06-27', '1996-10-12', '2011-01-18',
                          '2011-05-03', '2010-05-01',
                          '1999-12-01', '2010-06-01'))
)
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
    end = 'enter', ## name of lates date to search
    include = c(TRUE, FALSE), ## include lower bound but not upper
    ## long = TRUE, ## long output format is default
    ## stack = TRUE, ## stacked results are default
    verbose = FALSE ## give calculation progression info?
)


## ----'show-mh'-----------------------------------------------------------
tm[, c('id', 'event', 'alias', 'match', 'match.in', 'first.id')]

## ----'mh-fix'------------------------------------------------------------
tmp <- subset(tm, first.id == 1, select = c('id', 'event', 'alias'))
(medhist <- reshape(tmp, idvar = 'id',
                    timevar = c('alias'), direction = 'wide'))
names(medhist) <- gsub("event", "prior", names(medhist),
                       fixed = TRUE)

## ----'show-mh-fixed'-----------------------------------------------------
medhist

## ----'find-mh-wide-unstacked'--------------------------------------------
tmwu <- grepict(
    pattern = searchString, ## what to search for
    x = c('what1', 'what2'), ## search variables in 'data'
    data = RECORDS, ## data set to search in
    id = 'identity', ## name of id variable in 'data'
    date = 'what.date', ## name of date variable in 'data'
    units = POP, ## data set, or vector, containing individuals
    units.id = 'id', ## name of id variable in 'units'
    begin = NULL, ## earliest date to search from
    end = 'enter', ## name of lates date to search
    include = c(TRUE, FALSE), ## include lower bound but not upper
    long = FALSE, ## use wide output
    stack = FALSE, ## do not stack output
    verbose = FALSE ## give calculation progression info?
)
## tmwu contains 33 variables, only some of which are of interest
tmwu[, names(tmwu)[grepl('(id|event)', names(tmwu))]]

## ----'find-outcomes'-----------------------------------------------------

POP$endofstudy <- POP$enter + 365
tm2 <- grepict(pattern = searchString, x = c('what1', 'what2'),
               data = RECORDS, id = 'identity',
               date = 'what.date', units = POP, units.id = 'id',
               begin = 'enter', ## earliest date to search from
               end = 'endofstudy', ## name of latest date
               include = c(FALSE, TRUE), ## include upper but not lower bound
               verbose = FALSE)


## ----'show-outcomes'-----------------------------------------------------
tm2[, c('id', 'event', 'time', 'alias', 'match', 'match.in')]

## ----'outcomes-fix'------------------------------------------------------
tmp2 <- subset(tm2, first.id == 1,
               select = c('id', 'event', 'time', 'alias'))
(outcomes <- reshape(tmp2, idvar = 'id', timevar = c('alias'),
                     direction = 'wide'))
names(outcomes) <- gsub("event", "ev", names(outcomes), fixed = TRUE)
names(outcomes) <- gsub("time", "t", names(outcomes), fixed = TRUE)

## ----'show-outcomes-fixed'-----------------------------------------------
outcomes

## ----'find-outcomes-wide-unstacked'--------------------------------------

POP$endofstudy <- POP$enter + 365
tm2wu <- grepict(pattern = searchString, x = c('what1', 'what2'),
               data = RECORDS, id = 'identity',
               date = 'what.date', units = POP, units.id = 'id',
               begin = 'enter', ## earliest date to search from
               end = 'endofstudy', ## name of latest date
               include = c(FALSE, TRUE), ## include upper but not lower bound
               long = FALSE, ## use wide output
               stack = FALSE, ## do not stack
               verbose = FALSE)
tm2wu[, names(tm2wu)[grepl('(id|event[^s]|time)', names(tmwu))]]


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
cdate(x = c("2001/01/01", "2001/01/00", "2001/00/00"), sep = "/")
cdate(x = c("20010101", "20010100", "20010000"),
      low.bound = as.Date(c("1999-01-01", "2001-01-21",
                            "2001-06-20")))

