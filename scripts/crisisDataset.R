require(data.table)
options(max.print = 500)
options(width = 90)

load_all()

crisis <- fread(input = './inst/extdata/SOVEREIGN/RR-crisis.dataset.csv',
                header = TRUE)

## undebug(SovereignCrisis::createCrisisVariables)
crisis.2 <- 
    SovereignCrisis::createCrisisVariables(
        crisisDT = crisis,
        crisisCol = 'Foreign Sov Debt',
        idCol = 'ISO3',
        timeCol = 'Year'
    )

undebug(tabulateCrises)
tabulateCrises(
    crisisDT = crisis,
    crisisTypes = c(
        'Stock Market Crash',
        'Foreign Sov Debt',
        'Domestic Sov Debt',
        'Banking Crisis'
                    ),
    min.time = 1990,
    idCol = "ISO3",
    timeCol = "Year"
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