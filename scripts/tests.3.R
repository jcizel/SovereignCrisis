undebug(procExpand)

procExpand(data = dt,
           by = 'iso3',
           keepvars = 'date',
           convert =
               list(
                   'zscorepd50,zscorepd75,zscorepd90' = 'shift(lag=-1,dif = FALSE)'
               )) %>>% (~ dt2)

procExpand(data = dt,
           by = 'iso3',
           keepvars = c('year'),
           convert =
               list(
                   'ratingnum' = "(x ~ as.numeric(zoo:::rollmean(x,4,fill=NA,align='right')))"
               )) %>>%
(~ dt2) %>>%
(? dt ~ dt[,table(ratingnum)]) %>>%
(? dt ~ dt[ratingnum < -7])

## dt %>>%
## (? df ~ summary(df)) %>>%
## (df ~ procExpand(data = df,
##                  by = 'date',
##                  keepvars = 'iso3',
##                  convert =
##                      list('_NUMERIC_'=c("winsorize(method = 'PERC',fraction = 0.05)")))) %>>%
## (? df ~ summary(df)) %>>%
## invisible



## CORRELATIONS BETWEEN BENCHMARK MEASURES AND BOTTOM-UP SCORES

crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()

alt     <- getAltmanZscore()
imf     <- getIMFIFS()
bs      <- getAggregatedBankscope()

q <- WorldBankAPI:::queryWorldBankVariableList('tax revenue')
taxrev <- WorldBankAPI:::getWorldBankDataSeries(indicators = q$id)     


dt <- augmentBenchmarkDataset(
    crisisdb = crisis1,
    dtList =
        list(alt)
)



