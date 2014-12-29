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

save(list = c('dt','lookup'),
     file = './inst/extdata/rdata/comprehensive-macro-dataset.RData')


countries <- WorldBankAPI::getWorldBankCountries() %>>%
(dt ~ dt[, list(iso3 = id,
                region = region.id,
                income = incomeLevel.id,
                income.name = incomeLevel.value)])


dt <- 
    dt %>>%
    merge(countries, by = "iso3")

## -------------------------------------------------------------------------- ##
##    CORRELATION TABLES                                                      ##
## -------------------------------------------------------------------------- ##

## debug(tabulateCorrelationsByTime)

## LEVELS
tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = '`*`(1)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = '`*`(1)',
    method = 'pearson',
    ## outfile = '~/Downloads/test.tex'
    outfile = './inst/RESULTS/tabulateCorrelations.tex'
)

## CHANGES - ALL
tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations-dif.tex'
)

## CHANGES - ALL

## OECD
tabulateCorrelationsByTime(
    data = dt[income.name == 'High income: OECD'],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations-dif-OECD.tex'
)

## High Income
tabulateCorrelationsByTime(
    data = dt[income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations-dif-HighIncome.tex'
)

## Low Income
tabulateCorrelationsByTime(
    data = dt[!income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations-dif-LowIncome.tex'
)




## -------------------------------------------------------------------------- ##
## SPEARMAN CORRELATIONS                                                      ##
## -------------------------------------------------------------------------- ##

tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert =  '`*`(1)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = '`*`(1)',
    method = 'spearman',
    outfile = './inst/RESULTS/tabulateCorrelations-spearman.tex'
)

tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    benchVars = c('ratingnum','spread','cds'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'spearman',
    outfile = './inst/RESULTS/tabulateCorrelations-spearman-dif.tex'
)



## -------------------------------------------------------------------------- ##
## CORRELATIONS WITH TAXATION VARIABLES                                       ##
## -------------------------------------------------------------------------- ##

dt <- 
    within(dt,{
        D_GDP_USD = NY.GDP.MKTP.CD
        D_GDP_LCU = NY.GDP.MKTP.CN
        R_REV_GDP = GC.TAX.TOTL.CN / D_GDP_LCU #revenue
        R_INT_GDP = GC.XPN.INTP.CN / D_GDP_LCU #interest 
        R_INT_COV = GC.TAX.TOTL.CN / GC.XPN.INTP.CN #interest coverage            
    })

## GFDD.DM.07 outstanding debt issues to GDP
## GFDD.DM.04
## GFDD.DM.06
## GFDD.DI.06



## LEVELS
tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = '`*`(1)',
    benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchConvert = '`*`(1)',
    method = 'pearson',
    ## outfile = '~/Downloads/test.tex'
    outfile = './inst/RESULTS/tabulateCorrelations_debt.tex'
)

## CHANGES - ALL
tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_debt-dif.tex'
)


## OECD
tabulateCorrelationsByTime(
    data = dt[income.name == 'High income: OECD'],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_debt-dif-OECD.tex'
)

## High Income
tabulateCorrelationsByTime(
    data = dt[income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),    
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_debt-dif-HighIncome.tex'
)

## Low Income
tabulateCorrelationsByTime(
    data = dt[!income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),    
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_debt-dif-LowIncome.tex'
)




## REVENUES

tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = '`*`(1)',
    ## benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchVars = c('R_REV_GDP', 'R_INT_GDP', 'R_INT_COV'),
    benchConvert = '`*`(1)',
    method = 'pearson',
    ## outfile = '~/Downloads/test.tex'
    outfile = './inst/RESULTS/tabulateCorrelations_revenue.tex'
)

## CHANGES - ALL
tabulateCorrelationsByTime(
    data = dt,
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    ## benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchVars = c('R_REV_GDP', 'R_INT_GDP', 'R_INT_COV'),
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_revenue-dif.tex'
)


## OECD
tabulateCorrelationsByTime(
    data = dt[income.name == 'High income: OECD'],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    ## benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchVars = c('R_REV_GDP', 'R_INT_GDP', 'R_INT_COV'),    
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_revenue-dif-OECD.tex'
)

## High Income
tabulateCorrelationsByTime(
    data = dt[income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    ## benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchVars = c('R_REV_GDP', 'R_INT_GDP', 'R_INT_COV'),    
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_revenue-dif-HighIncome.tex'
)

## Low Income
tabulateCorrelationsByTime(
    data = dt[!income.name %in% c('High income: OECD','High income: nonOECD')],
    xvar = c('zscorepd75','SC_CLOSURE_ALL.Q3.'),
    xvarConvert = 'shift(lag = -1, dif = TRUE)',
    ## benchVars = c('ratingnum','spread','cds'),
    ## benchVars = c('GFDD.DM.04','GFDD.DM.06','GFDD.DM.07'),
    benchVars = c('R_REV_GDP', 'R_INT_GDP', 'R_INT_COV'),    
    benchConvert = 'shift(lag = -1, dif = TRUE)',
    method = 'pearson',
    outfile = './inst/RESULTS/tabulateCorrelations_revenue-dif-LowIncome.tex'
)

