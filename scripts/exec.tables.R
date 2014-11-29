require(devtools)
require(pipeR)
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


LaTeXTableGems:::createLatexTableHeader(
    outfile = './inst/RESULTS/tabulateCorrelations-head.tex')

t1 <- tabulateCorrelations(outfile = './inst/RESULTS/tabulateCorrelations.tex',
                           convert = '`*`(1)')

t2 <- tabulateCorrelations(outfile = './inst/RESULTS/tabulateCorrelations-dif.tex',
                           convert = 'shift(lag=-1,dif = TRUE)')

t3 <- tabulateCorrelations(method = 'spearman',
                           convert = '`*`(1)',
                           outfile = './inst/RESULTS/tabulateCorrelations-spearman.tex')

t4 <- tabulateCorrelations(method = 'spearman',
                           convert = 'shift(lag=-1,dif = TRUE)',
                           outfile = './inst/RESULTS/tabulateCorrelations-spearman-dif.tex')




## -------------------------------------------------------------------------- ##
##           RELATION BETWEEN TAX REVENUES AND SOVEREIGN DEFAULT              ##
## -------------------------------------------------------------------------- ##

## dt %>>%
## (df ~ procExpand(df,
##                  by = 'iso3',
##                  convert =
##                  list('ratingnum ~ shift(lag=-1,dif=TRUE)'))) %>>%
## (? df ~ table(df[[2]])) %>>%
## (df ~ df[[2]]) %>>%
## hist(40)

Pipe(mtcars)$
data.table(.)$
.(? df ~ summary(df))$
.(? ncol(.) -> n)$
.(? length(.[,carb]))$
.(~ .[,carb] %>>% hist(10))$
invisible()



