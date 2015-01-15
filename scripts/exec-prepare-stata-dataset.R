crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
alt     <- getAltmanZscore()
pd      <- getAggregatedBankscopePDs()
macro   <- createQueriedMacroDataset(test = FALSE)
rating  <- getSPRatings()
cds     <- getBloombergSovCDS()
spread  <- getSovBondSpreads()
macro %>>% save(file = './inst/extdata/rdata/macro-data.RData')
macro.l <- attributes(macro)$lookup[, list(name = varcode,
                                           label = label)]


oecd.debt <- fread('../SDMXWrappers/inst/extdata/OECD/GOV_DEBT/DATATABLE.csv')
oecd.bank <- fread('../SDMXWrappers/inst/extdata/OECD/BPF1/DATATABLE.csv')
oecd.rev <- fread('../SDMXWrappers/inst/extdata/OECD/REV/DATATABLE.csv')
oecd.revla <- fread('../SDMXWrappers/inst/extdata/OECD/RSLACT/DATATABLE.csv')
oecd.nacc <- fread('../SDMXWrappers/inst/extdata/OECD/NAAG/DATATABLE.csv')
oecd.govt <- fread('../SDMXWrappers/inst/extdata/OECD/SNA_TABLE12/DATATABLE.csv')


oecd.debt.l  <- fread('../SDMXWrappers/inst/extdata/OECD/GOV_DEBT/LOOKUPTABLE.csv')
oecd.bank.l  <- fread('../SDMXWrappers/inst/extdata/OECD/BPF1/LOOKUPTABLE.csv')
oecd.rev.l   <- fread('../SDMXWrappers/inst/extdata/OECD/REV/LOOKUPTABLE.csv')
oecd.revla.l <- fread('../SDMXWrappers/inst/extdata/OECD/RSLACT/LOOKUPTABLE.csv')
oecd.nacc.l  <- fread('../SDMXWrappers/inst/extdata/OECD/NAAG/LOOKUPTABLE.csv')
oecd.govt.l  <- fread('../SDMXWrappers/inst/extdata/OECD/SNA_TABLE12/LOOKUPTABLE.csv')


lookup.l <-
    list(oecd.debt.l, 
         oecd.bank.l, 
         oecd.rev.l,  
         oecd.revla.l,
         oecd.nacc.l,
         oecd.govt.l) %>>%
list.map(. %>>%
         (df ~ df[, list(name = IDnew,
                         label = LABEL)])) %>>%
rbindlist

lookup <- rbindlist(list(macro.l, lookup.l))


dt <-
    list(
        rating,
        spread,
        cds,
        crisis1,
        crisis2,
        alt,
        pd,
        macro,
        oecd.debt,
        oecd.bank,
        oecd.rev,
        oecd.revla,
        oecd.nacc,
        oecd.govt        
    ) %>>% joinDatasetList()


## Propagate ratings forward
modCols <- c('rating','ratingnum')
dt[
  , paste(modCols) := lapply(.SD, na.locf, na.rm = FALSE)
  , by = "iso3"
  , .SDcols = modCols]


countries <- WorldBankAPI::getWorldBankCountries() %>>%
(dt ~ dt[, list(iso3 = id,
                region = region.id,
                income = incomeLevel.id,
                income.name = incomeLevel.value)])


dt <- 
    dt %>>%
    merge(countries, by = "iso3")


convertToStata(
    data = dt,
    lookup = lookup,
    nameCol = "name",
    labelCol = "label"
)
