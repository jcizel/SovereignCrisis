## -------------------------------------------------------------------------- ##
## ALL COUNTRIES                                                              ##
## -------------------------------------------------------------------------- ##
countries <- getWorldBankCountries()
countries[, iso3 := id]

save(countries, file = './data/countries.RData')

## -------------------------------------------------------------------------- ##
## LOAD BOTTOM-UP RISK MEASURES                                               ##
## -------------------------------------------------------------------------- ##
bench <- getSovBenchmarks()

zscore <-
    fread(input = './inst/extdata/Altman PDs/ZSCORE.csv')

zscore[, DATE := {
    x <- DATE
    year =
        gsub("^([0-9]+):([0-9])",
             "\\1",
             x) %>>% as.numeric
    qtr =
        gsub("^([0-9]+):([0-9])",
             "\\2",
             x) %>>% as.numeric
    as.Date(sprintf("%s-%s-%s",
                    year,
                    qtr * 3,
                    1))
}]

.vars <- c('PU60_US_SEL3','PR60_US_SEL3')
zscore[, (.vars) := lapply(.SD,function(x){
    (x-mean(x,na.rm = T))/sd(x,na.rm = T)
}), .SDcols = .vars]


## BANK PDS

bankpd <-
    fread(input = './inst/extdata/Altman PDs/BANKPD.csv')

## replace iso2 with iso3
bankpd[, fic := {
    x = CTRYCODE
    .l <- data.table(countries[,list(iso2Code,id)],key = 'iso2Code')
    .l[x][[2L]]
}]

bankpd[, DATE := as.Date(sprintf("%s-%s-%s",
                                 YEAR,
                                 12,
                                 31))]

.vars <- c('SC_CLOSURE_ALL','SC_OBR_EU')
bankpd[, (.vars) := lapply(.SD,function(x){
    (x-mean(x,na.rm = T))/sd(x,na.rm = T)
}), .SDcols = .vars]


## AGGREGATIONS
zscore[,c("DATE", "fic", "PU60_US_SEL3", "PR60_US_SEL3", 
          "PD_PU60_US_SEL3", "PD_PR60_US_SEL3"), with = FALSE] %>>%
GeneralUtilities::procUnivariate(
    drop = c("DATE","fic"),
    by = c("DATE","fic")
) -> zscore.agg
names(zscore.agg) %>>% stringr::str_trim() -> names(zscore.agg)

zscore[!is.na(PU60_US_SEL3),c("MCAP_USD","DATE", "fic", "PU60_US_SEL3", "PR60_US_SEL3", 
          "PD_PU60_US_SEL3", "PD_PR60_US_SEL3"), with = FALSE] %>>%
GeneralUtilities::procUnivariate(
    drop = c("DATE","fic"),
    by = c("DATE","fic"),
    weight = "MCAP_USD"
) -> zscore.agg.w
names(zscore.agg.w) %>>% stringr::str_trim() -> names(zscore.agg.w)

GeneralUtilities::procUnivariate(
    data = bankpd[!is.na(SC_CLOSURE_ALL),list(INDEX,fic,DATE,TOTASSUSD,SC_CLOSURE_ALL,SC_OBR_EU)],
    drop = c("fic","DATE","INDEX"),
    by = c("fic","DATE")) -> bankpd.agg

GeneralUtilities::procUnivariate(
    data = bankpd[!is.na(SC_CLOSURE_ALL),list(INDEX,fic,DATE,TOTASSUSD,SC_CLOSURE_ALL,SC_OBR_EU)],
    drop = c("fic","DATE","INDEX"),
    by = c("fic","DATE"),
    weight = "TOTASSUSD"
) -> bankpd.agg.w

## -------------------------------------------------------------------------- ##
## SAVE THE DATASETS                                                          ##
## -------------------------------------------------------------------------- ##
crisis <- loadCrisisDB()

save(crisis,bench,zscore,bankpd,zscore.agg,zscore.agg.w,bankpd.agg,bankpd.agg.w,file = './data/bottomup.RData')


## -------------------------------------------------------------------------- ##
##                     PREPARE BENCHMARK MEASURES                              ##
## -------------------------------------------------------------------------- ##
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()


## RATINGS
ids = ratings$iso3 %>>% unique
dates = .fCrDates(begin="1960-01-01",end="2014-07-31", frequency=apply.weekly)[[2L]]

.index <- CJ(iso3 = ids,date = dates)

rat <- ratings[,list(ratingnum),key=c('iso3','date')][.index[,key = c('iso3', 'date')], roll = TRUE]

rat %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'ratingnum',
                 na.rm = TRUE)  %>>%
( dt ~ xts(dt[,.SD,.SDcols = -c("date")], order.by = dt$date) ) -> ratm

## CDS
ids = cds$iso3 %>>% unique
dates = .fCrDates(
    begin = cds$date %>>% min,
    end = cds$date %>>% max,
    frequency = apply.weekly
)[[2L]]

.index <- CJ(iso3 = ids,date = dates)
cds <-
    (data.table(cds,key = c('iso3','date'))%>>%unique)[data.table(.index, key = c('iso3','date')), roll = TRUE]

cds %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'cds',
                 na.rm = TRUE) %>>%
( dt ~ xts(dt[,.SD,.SDcols = -c('date')], order.by = dt$date) ) -> cdsm


## SPREAD
ids = spreads$iso3 %>>% unique
dates = .fCrDates(
    begin = spreads$date %>>% min,
    end = spreads$date %>>% max,
    frequency = apply.weekly
)[[2L]]

.index <- CJ(iso3 = ids,date = dates)
spreads <-
    (data.table(spreads,key = c('iso3','date'))%>>%unique)[data.table(.index, key = c('iso3','date')), roll = TRUE]

spreads %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'spread',
                 na.rm = TRUE) %>>%
( dt ~ xts(dt[,.SD,.SDcols = -c('date')], order.by = dt$date) ) -> spreadsm

save(ratm, cdsm, spreadsm, file = './data/benchmarks.RData')


