require(devtools)
load_all()

## -------------------------------------------------------------------------- ##
## SUMMARY OF CRISIS EVENTS
## -------------------------------------------------------------------------- ##
crisis <- loadCrisisDB()
out <- 
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
        idCol = "ISO3",
        timeCol = "Year",
        outfile = './inst/RESULTS/crisisdb.tex'
    )


## -------------------------------------------------------------------------- ##
## DATA AVAILABILITY                                                          ##
## -------------------------------------------------------------------------- ##
bench <- getSovBenchmarks()
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
r <- 
    tabulateDataAvailability(dt = bench,
                             outfile = "./inst/RESULTS/availability.tex",
                             lookup.table = lookup.table,
                             selCols = c("label", "Availability"))


## -------------------------------------------------------------------------- ##
## STUDY CORRELATIIONS                                                        ##
## -------------------------------------------------------------------------- ##

alt     <- getAltmanZscore()
bs      <- getAggregatedBankscopePDs()

dtList <-
    list(alt,bs)

dt <- 
    augmentBenchmarkDataset(crisisdb = alternativeCrisisDB(),
                            dtList = dtList)


r1 <- analyseCorrelationsOverTime(data = dt, xvar = 'zscorepd50')
r2 <- analyseCorrelationsOverTime(data = dt, xvar = 'zscorepd75')
r3 <- analyseCorrelationsOverTime(data = dt, xvar = 'zscorepd90')
r4 <- analyseCorrelationsOverTime(data = dt, xvar = 'SC_CLOSURE_ALL.Q3.')
r5 <- analyseCorrelationsOverTime(data = dt, xvar = 'SC_CLOSURE_ALL.P90.') 
r6 <- analyseCorrelationsOverTime(data = dt, xvar = 'SC_OBR_EU.Q3.')
r7 <- analyseCorrelationsOverTime(data = dt, xvar = 'SC_OBR_EU.P90.') 




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



varsel <- c('ratingnum','cds','spread','zscorepd75')
dt[, paste0(varsel,'.dif') := lapply(.SD, function(x) {
    o <- winsorize(x, method = 'IQR', k = 2, trim = FALSE)
    o <- shift(o, dif = TRUE, relative = FALSE)
    o
})
   , by = c('iso3'),
   , .SDcols = varsel]



sel <- 'zscorepd75'
vars <- c('cds','ratingnum','spread')

sel <- 'zscorepd75.dif'
vars <- c('cds.dif','ratingnum.dif','spread.dif')

dt[, {
    o <- 
        foreach(x = vars) %do% {
            cor(get(x), get(sel),
                use = 'pairwise.complete.obs')
        }
    names(o) <- vars

    o
}
   , by = date]
