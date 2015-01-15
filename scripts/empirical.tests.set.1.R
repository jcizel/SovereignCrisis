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


dt <-
    list(
        bench,
        crisis1,
        crisis2,
        alt,
        pd,
        copy(macro)
    ) %>>% joinDatasetList



dt1[iso3 == 'GRC']
dt2[iso3 == 'GRC'] %>>% (df~qplot(data = df, x=date,y =C <- ratingnum <- D,geom = 'line'))

## COMPUTE PAIRWISE CORRELATIONS BY DECILES OF Z

undebug(tabulateCorrelationsByGroup)
tabulateCorrelationsByGroup(
    data = dt,
    group = 'spread',
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = +1, dif = TRUE);`*`(-1)',
    benchVars = c('cds','ratingnum','spread'),
    benchConvert = 'shift(lag = +1, dif = TRUE);`*`(-1)',
    method = 'spearman'
)

tabulateCorrelationsByGroup(
    data = dt,
    group = 'ratingnum',
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = +1, dif = TRUE);`*`(-1)',
    method = 'pearson'
)

