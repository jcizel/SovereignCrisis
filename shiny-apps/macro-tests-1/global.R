require(devtools)
require(shiny)
library(shinydashboard)

library(data.table)
library(pipeR)
library(rlist)
library(plyr)
library(xlsx)
library(xts)
library(dygraphs)
library(rCharts)

source('./helpers.R')
source('./plotters.R')

## -------------------------------------------------------------------------- ##
## IMPORT ALL SOURCES FROM THE PACKAGE                                        ##
## -------------------------------------------------------------------------- ##
list.files('./R',
           pattern = "\\.R$",
           full.names = TRUE) %>>%
list.map(source(.))


## -------------------------------------------------------------------------- ##
## LOAD DATA                                                                  ##
## -------------------------------------------------------------------------- ##
load(file = './data/macro-data.RData')

macro <- macro
lookup <- attributes(macro)[['lookup']]

lookupSelections <- local({
    setkey(lookup,varcode)
    l <- lookup %>>% unique
    
    o <- l$varcode %>>% as.character
    names(o) <- l$label %>>% as.character 
    o
})

## -------------------------------------------------------------------------- ##
## LOAD BOTTOM-UP RISK MEASURES                                               ##
## -------------------------------------------------------------------------- ##
alt     <- getAltmanZscore()
pd      <- getAggregatedBankscopePDs()



## -------------------------------------------------------------------------- ##
## SUMMARY OF CRISIS EVENTS
## -------------------------------------------------------------------------- ##
crisis <- loadCrisisDB()
crisis[, year := year(date)]
crisisSummary <-
    tabulateCrises(
        crisisDT = crisis,
        crisisTypes = c(
            'Stock Market Crash',
            'Currency Crisis',
            'Inflation Crisis',
            'Foreign Sov Debt',
            'Domestic Sov Debt',
            'Banking Crisis'
        ),
        min.time = 1960,
        idCol = "iso3",
        timeCol = "year",
        outfile = './inst/RESULTS/crisisdb2.tex'
    )

## -------------------------------------------------------------------------- ##
## ALL COUNTRIES                                                              ##
## -------------------------------------------------------------------------- ##
countries <- getWorldBankCountries()
countries[, iso3 := id]

## -------------------------------------------------------------------------- ##
## DATA AVAILABILITY                                                          ##
## -------------------------------------------------------------------------- ##
bench <- getSovBenchmarks()

merge(
    bench,
    countries,
    by = 'iso3'
) -> bench


lookup.table <- rbindlist(
    list(
        data.table(name = 'ratingnum',
                   label = 'S&P LT Foreign Issuer Sovereign Rating'),
        data.table(name = 'cds',
                   label = '5-Year Sovereign CDS Spread (Source: Bloomberg)'),
        data.table(name = 'spread',
                   label = 'Treasury Bond Spread above U.S. Treasury Yield (in b.p)')
    )
)

vars <- c('cds','rating','spread')

.res <- list()
for (x in vars){
    bench[!is.na(get(x)),
          sprintf("%.0f-%.0f",
                  min(date) %>>% year,
                  max(date) %>>% year),
          keyby = c("iso3")] -> .res[[x]]
    
    .res[[x]] %>>% setnames('V1',x)
}
bench.availability <- Reduce(function(...) merge(...,by = 'iso3', all = TRUE),.res)



## -------------------------------------------------------------------------- ##
##                     PLOT BENCHMARK MEASURS                                 ##
## -------------------------------------------------------------------------- ##
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()

library(dygraphs)


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

