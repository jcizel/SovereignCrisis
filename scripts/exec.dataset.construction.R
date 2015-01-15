'require('WorldBankAPI')'
                                        ########################################
                                        # NEED THE FOLLOWING MACROECONOMIC     #
                                        # VARIABLES:                           #
                                        #                                      #
                                        # 1. GOVERNMENT REVENUE 2. GOVERNMENT  #
                                        # EXPENDITURE 3. EXTERNAL GOVERNMENT   #
                                        # DEBT 4. GDP 5. UNEMPLOYMENT 6.       #
                                        #                                      #
                                        ########################################

## GOVERNMENT REVENUE
options(width = 200)
queryMacroDatasets('balance of payments')
queryMacroDatasets('liquidity')
queryMacroDatasets('stock market')
queryMacroDatasets('performing')
queryMacroDatasets('tax')
queryMacroDatasets('crisis')
queryMacroDatasets('revenue')
queryMacroDatasets('expenditure')
queryMacroDatasets('debt')
queryMacroDatasets('external')
queryMacroDatasets('tax revenue')
queryMacroDatasets('house')
queryMacroDatasets('gross domestic product')
queryMacroDatasets('gdp ')
queryMacroDatasets('gross national income')
queryMacroDatasets('unemployment')
queryMacroDatasets('employ')
queryMacroDatasets('treasury')
queryMacroDatasets('yield')
queryMacroDatasets('spread')
queryMacroDatasets('distance')
queryMacroDatasets('probability')
queryMacroDatasets('score')
queryMacroDatasets('arrear')
queryMacroDatasets('repayment')
queryMacroDatasets('ppg ')
queryMacroDatasets('disbursement')
queryMacroDatasets('interest payment')
queryMacroDatasets('interest rate')
queryMacroDatasets('default')
queryMacroDatasets('failure')
queryMacroDatasets('bankrupt')
queryMacroDatasets('short.term')
queryMacroDatasets('imf')
queryMacroDatasets('capital')
queryMacroDatasets('foreign direct investment')
queryMacroDatasets('polit')
queryMacroDatasets('growth')
queryMacroDatasets('index')
queryMacroDatasets('trade')
queryMacroDatasets('export')
queryMacroDatasets('import')
queryMacroDatasets('embi')
queryMacroDatasets('output gap')
queryMacroDatasets('official')
queryMacroDatasets('official credit')
queryMacroDatasets('terms')
queryMacroDatasets('terms.of.trade')
queryMacroDatasets('ted spread')
queryMacroDatasets('balance')
queryMacroDatasets('public sector debt')
queryMacroDatasets('GFDD')
queryMacroDatasets('budget')
queryMacroDatasets('deficit')
queryMacroDatasets('Government Finance')
queryMacroDatasets('corporate')
queryMacroDatasets('bail')

## IMF Lending




## MANUALLY MARK THE CSV FILES CREATED IN THE ABOVE QUERIES, BY ADDING A
## `SELECT` THAT EQUALS 1 IF THE VARIABLE IS TO BE INCLUDED IN THE FINAL
## DATASET.

## -------------------------------------------------------------------------- ##
## CREATE ANALYSIS DATASET                                                    ##
## -------------------------------------------------------------------------- ##
crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
alt     <- getAltmanZscore()
pd      <- getAggregatedBankscopePDs()
macro   <- createQueriedMacroDataset(test = FALSE)
macro %>>% save(file = './inst/extdata/rdata/macro-data.RData')

oecd.debt <- fread('../SDMXWrappers/inst/extdata/OECD/GOV_DEBT/DATATABLE.csv')
oecd.bank <- fread('../SDMXWrappers/inst/extdata/OECD/BPF1/DATATABLE.csv')



dtList <-
    list(
        crisis1,
        crisis2,
        alt,
        pd,
        macro,
        oecd.debt,
        oecd.bank
    )


dt <- dtList %>>% joinDatasetList()




dt <- augmentBenchmarkDataset(
    crisisdb = crisis1,
    dtList =
        list(crisis2,
             alt,
             pd,
             macro
             )
)




lookup[varcode %like% "CN$"][,list(varcode,label)]

dt %>>%
within({
    GDP     = NY.GDP.MKTP.CN
    C.GDP.CN     = NY.GDP.MKTP.CN
    C.TAXREV.GDP = GC.TAX.TOTL.CN / C.GDP.CN
    C.INT.GDP    = GC.XPN.INTP.CN / C.GDP.CN
    C.INT.TAXREV = GC.XPN.INTP.CN / GC.TAX.TOTL.CN
}) -> dtmod

dtmod %>>%
(~ hist(.$C.TAXREV.GDP,100)) %>>%
(~ hist(.$C.INT.GDP,100)) %>>%
(~ hist(.$C.INT.TAXREV,100))


procExpand(
    data = dtmod,
    by = 'iso3',
    keepvars = c('date','GDP'),
    convert =
        list('
__CN$ ~ `/`(shift(GDP,lag = -1));
winsorize(method = "IQR")'),
    suffix = '.RATIO')


macro
%>>%
subset(date > '1960-01-01') %>>%
(? str(.)) %>>%
(df ~ df[, hist(DP.DOD.DECD.CR.GG.CD
               ,100)])



.cumprod <- function(x){
    x[is.na(x)] <- 1
    cumprod(x)
}

procExpand(
    data = dtmod,
    by = 'iso3',
    keepvars = c('date','GDP'),
    convert =
        ## list('GDP ~ shift(lag = -1, dif = TRUE, relative = TRUE)')
        list('__CN$ ~ `/`(shift(GDP,lag = -1));winsorize(method = "IQR")')
)

dtmod %>>%
procExpand(by = 'iso3',
           keepvars = 'date',
           convert =
               list('L62 ~ shift(lag = -1, dif = TRUE, relative = TRUE)')) %>>%
subset(iso3=='USA' & date > '1960-01-01') %>>%
qplot(x = date,
      y = L62,
      data = .,
      geom = 'line')

lookup <- attributes(macro)$lookup

convertToStata(data = dt,
               lookup = lookup)
