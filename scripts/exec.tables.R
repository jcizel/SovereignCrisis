require(devtools)

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
