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


LaTeXTableGems:::createLatexTableHeader(outfile = './inst/RESULTS/tabulateCorrelations-head.tex')
t1 <- tabulateCorrelations(outfile = './inst/RESULTS/tabulateCorrelations.tex')
t2 <- tabulateCorrelations(dif = TRUE,
                           lag = -1,
                           outfile = './inst/RESULTS/tabulateCorrelations-dif.tex')

t3 <- tabulateCorrelations(method = 'spearman',
                           outfile = './inst/RESULTS/tabulateCorrelations-spearman.tex')
t4 <- tabulateCorrelations(method = 'spearman',
                           dif = TRUE,
                           lag = -1,
                           outfile = './inst/RESULTS/tabulateCorrelations-spearman-dif.tex')




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



