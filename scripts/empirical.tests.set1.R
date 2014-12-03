 
###############################################################################
## TESTING OF THE FUNDAMENTAL-BASED VIEW OF SOVEREIGN DEFAULT RISK           ##
##                                                                           ##
## TEST SPECIFICATIONS OF THE FORM:                                          ##
##                                                                           ##
## delta(y,{t,t+2}) ~ delta(x, {t-1,t}) | z, year                            ##
##                                                                           ##
## - y = {rating, cds, spread},                                              ##
##                                                                           ##
## - x={revenue, unanticipated expenditure, financial sector health score,   ##
## non-financial sector health score}.                                       ##
##                                                                           ##
## - z = {debt, rating, income classification, geographical classification}  ##
###############################################################################

bench   <- getSovBenchmarks()
crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
alt     <- getAltmanZscore()
pd      <- getAggregatedBankscopePDs()
macro   <- createQueriedMacroDataset(test = FALSE)


dtList <-
    list(
        bench,
        crisis1,
        crisis2,
        alt,
        pd,
        macro
    ) 

