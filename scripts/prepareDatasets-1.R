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
    - (x-mean(x,na.rm = T))/sd(x,na.rm = T) #NOTICE MINUS SIGN!!!
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


## -------------------------------------------------------------------------- ##
## DEBT DATA                                                                  ##
## -------------------------------------------------------------------------- ##
rrdebt <- getRR.debt()
rrdebt[, date := as.Date(sprintf("%s-%s-%s",
                  year,
                  12,
                  31))]

save(rrdebt, file = './data/otherdata.RData')


## -------------------------------------------------------------------------- ##
##                     GENERAL MACRO DATA                                     ##
## -------------------------------------------------------------------------- ##
## LOAD MACRO
load(file = './inst/extdata/rdata/macro-data.RData')
macro.l <- attributes(macro)$lookup[, list(name = varcode,
                                           label = label)]


oecd.debt <- fread('./inst/extdata/OECD/GOV_DEBT/DATATABLE.csv')
oecd.bank <- fread('./inst/extdata/OECD/BPF1/DATATABLE.csv')
oecd.rev <- fread('./inst/extdata/OECD/REV/DATATABLE.csv')
oecd.revla <- fread('./inst/extdata/OECD/RSLACT/DATATABLE.csv')
oecd.nacc <- fread('./inst/extdata/OECD/NAAG/DATATABLE.csv')
oecd.govt <- fread('./inst/extdata/OECD/SNA_TABLE12/DATATABLE.csv')


oecd.debt.l  <- fread('./inst/extdata/OECD/GOV_DEBT/LOOKUPTABLE.csv')
oecd.bank.l  <- fread('./inst/extdata/OECD/BPF1/LOOKUPTABLE.csv')
oecd.rev.l   <- fread('./inst/extdata/OECD/REV/LOOKUPTABLE.csv')
oecd.revla.l <- fread('./inst/extdata/OECD/RSLACT/LOOKUPTABLE.csv')
oecd.nacc.l  <- fread('./inst/extdata/OECD/NAAG/LOOKUPTABLE.csv')
oecd.govt.l  <- fread('./inst/extdata/OECD/SNA_TABLE12/LOOKUPTABLE.csv')



list(oecd.debt.l, 
     oecd.bank.l, 
     oecd.rev.l,  
     oecd.revla.l,
     oecd.nacc.l,
     oecd.govt.l) %>>%
list.map(. %>>%
         (df ~ df[, list(name = IDnew,
                         label = LABEL)])) %>>%
rbindlist -> lookup.l

macrodatalookup <- rbindlist(list(macro.l, lookup.l))


macrodata <-
    list(
        macro,
        oecd.debt,
        oecd.bank,
        oecd.rev,
        oecd.revla,
        oecd.nacc,
        oecd.govt        
    ) %>>% joinDatasetList()

macrodata %>>%
summarizeDataAvailability() %>>%
data.table(key = 'variable') %>>% unique -> macrodata.availability

(data.table(macrodatalookup,key = 'name') %>>% unique)[macrodata.availability] ->
    macrodata.availability

save(macrodata,macrodatalookup,macrodata.availability,
     file = './data/macrodata.RData')



## -------------------------------------------------------------------------- ##
## HOUSE PRICES                                                               ##
## -------------------------------------------------------------------------- ##

#,------------------------------------------------------------------------------
#| Housing prices (BIS database)
#`------------------------------------------------------------------------------
housestatic <- fread("./inst/extdata/BIS House Prices/BIS static selection.csv")


fread("./inst/extdata/BIS House Prices/BIS House Price DB.csv") %>>%
setnames(
    old = names(.),
    new = stringr:::str_trim(names(.))
) %>>%
(dt ~ dt[, date := as.Date(as.character(Period), format = "%d.%m.%Y")]) %>>%
(dt ~ dt[, Period := NULL ]) %>>%
data.table:::melt.data.table(
    id.vars = 'date'
) %>>%
(dt ~ dt[, variable := gsub("(Q)(.+)","\\2", variable)]) %>>%
data.table(key = 'variable') %>>%
merge(y = data.table(housestatic[, list(
          variable = Code,
          iso3 = ISO3
)], key = 'variable'),
      by = 'variable') %>>%
setnames("value","houseix") %>>%
subset(!is.na(houseix)) %>>%
(dt ~ dt[, variable := NULL ]) %>>%
(dt ~ dt[, houseix := as.numeric(houseix)])-> house

## -------------------------------------------------------------------------- ##
## SAVE THE DATASETS                                                          ##
## -------------------------------------------------------------------------- ##
crisis <- loadCrisisDB()

save(crisis,bench,house,zscore,bankpd,zscore.agg,zscore.agg.w,bankpd.agg,bankpd.agg.w,file = './data/bottomup.RData')








