## install.packages('devtools')
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


## devtools::install_github('jcizel/GeneralUtilities')
## devtools::install_github('bthieurmel/highchartsUtils')
require(GeneralUtilities)
## require(highchartsUtils)

source('./helpers.R')
source('./plotters.R')
source('./R/prepareDataset.R')

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


load(file = './data/benchmarks.RData')
load(file = './data/bottomup.RData')
load(file = './data/otherdata.RData')
load(file = './data/macrodata.RData')


bankpd[['NAME']]  %>>% iconv(from='latin1',to="ASCII",sub = "") -> bankpd[['NAME']]


## -------------------------------------------------------------------------- ##
## SUMMARY OF CRISIS EVENTS
## -------------------------------------------------------------------------- ##
## crisis <- loadCrisisDB()

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
        outfile = './crisisdb2.tex'
    )

## -------------------------------------------------------------------------- ##
## ALL COUNTRIES                                                              ##
## -------------------------------------------------------------------------- ##
load(file = './data/countries.RData')


## -------------------------------------------------------------------------- ##
## DATA AVAILABILITY                                                          ##
## -------------------------------------------------------------------------- ##
## bench <- getSovBenchmarks()

merge(
    bench,
    countries,
    by = 'iso3'
) -> bench


lookup.table <- rbindlist(
    list(
        data.table(name = 'ratingnum',
                   label = 'S&P LT Foreign Issuer Sovereign Rating'),
        data.table(name = 'cds,
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



