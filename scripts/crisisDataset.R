require(data.table)
options(max.print = 500)
options(width = 90)

load_all()

crisis <- loadCrisisDB()

## undebug(SovereignCrisis::createCrisisVariables)
crisis.2 <- 
    createCrisisVariables(
        crisisDT = crisis,
        crisisCol = 'Foreign Sov Debt',
        idCol = 'ISO3',
        timeCol = 'Year'
    )

undebug(tabulateCrises)

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

undebug(eventCounter)
tabulateCrises(
    crisisDT = crisis,
    crisisTypes = c('Stock Market Crash'
                    ),
    min.time = 1990,
    idCol = "ISO3",
    timeCol = "Year"
)


## BENCHMARKS
bench <- getSovBenchmarks()


## -------------------------------------------------------------------------- ##
## ALTERNATIVE CRISISDB                                                       ##
## -------------------------------------------------------------------------- ##
crisis <- alternativeCrisisDB()
bench <- getSovBenchmarks()

dt <- prepareCrisisBenchmarkDataset(crisisdb = alternativeCrisisDB())
## undebug(plotDensityAroundCrisisEvents)
plotDensityAroundCrisisEvents(crisisdb = crisis,                              
                              crisisType = "ratingdrop",
                              adjust = TRUE,
                              groups =
                                  list("[-4:-1]"=expression(COUNTDOWN %between% c(-4,-1)),
                                       "[0]"=expression(COUNTDOWN == 0)))
