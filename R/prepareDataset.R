##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param benchMeasures 
##' @param crisisIndicators 
##' @param bottomupMeasuresNonFin 
##' @param weightedNonFin 
##' @param includeStatsNonFin 
##' @param bottomupMeasuresFin 
##' @param weightedFin 
##' @param includeStatsFin 
##' @param OtherMacroMeasures 
##' @param ##carryForwardVars 
##' @param ##outfile 
##' @param ##filetype 
##' @return 
##' @author Janko Cizel
prepareDataset <- function(
    
    ## BENCHMARK MEASURES
    benchMeasures = c("cds", "rating", "ratingnum", "bench", "spread"),
    
    ## CRISIS INDICATORS
    crisisIndicators = c( "RRCRISIS_CURR", "RRCRISIS_INFL", "RRCRISIS_SMCRASH",
        "RRCRISIS_DSDEBT", "RRCRISIS_FSDEBT", "RRCRISIS_BANK"),
    ## includeTTE = ,
    
    ## NON-FINANCIAL BOTTOM UP RISK MEASURE
    bottomupMeasuresNonFin = c("PU60_US_SEL3", "PR60_US_SEL3", "PD_PU60_US_SEL3",
        "PD_PR60_US_SEL3"),
    weightedNonFin = FALSE,
    includeStatsNonFin = c("N", "NOBS", "SUM", "MEAN", "SD", "0%", "10%",
        "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
    
    ## FINANCIAL SECTOR RISK MEASURE
    bottomupMeasuresFin = c('SC_CLOSURE_ALL','SC_OBR_EU'),
    weightedFin = weightedNonFin,
    includeStatsFin = "",
    
    ## OTHER MACRO INDICATORS
    GDPMeasures = c(),
    PublicDebtMeasures = c(),
    HousePriceMeasures = TRUE,
    DeficitMeasures = c(),
    TaxRevMeasures = c(),
    OtherMacroMeasures = c('AMT.A.PCT.P1')

    ## FINAL TOUCHES
    ## frequency = 'annual',
    ## carryForwardVars = 'ratingnum',
    ## winsorVars = c(),
    ## winsorLevel = 0.01,

    ## DOWNLOAD SPECIFICATION
    ## outfile = 'result.dta',
    ## filetype = c('stata','csv')
){
    load('./data/bottomup.RData')
    res <- list()

    ## ..bench.. <- getSovBenchmarks()
    ..bench.. <- bench
    
    ## BENCHMARK MEASURES
    ## benchMeasures = c("cds", "rating", "ratingnum", "bench", "spread")
    res[['benchMeasuresDT']] <-
        ..bench..[, .SD,
                  .SDcols =
                      c('iso3',
                        'date',
                        benchMeasures)]

    ## CRISIS INDICATORS
    ## crisisIndicators = c( "RRCRISIS_CURR", "RRCRISIS_INFL", "RRCRISIS_SMCRASH",
    ## "RRCRISIS_DSDEBT", "RRCRISIS_FSDEBT", "RRCRISIS_BANK"),
    ## includeTTE = ,
    ## ..rrcrisis.. <- loadCrisisDB()
    ..rrcrisis.. <- crisis
    res[['crisisIndicatorsDT']] <-
        ..rrcrisis..[, .SD,
                     .SDcols =
                         c('iso3','date',
                           names(..rrcrisis..)[names(..rrcrisis..) %in% crisisIndicators])]

    ## NON-FINANCIAL BOTTOM UP RISK MEASURE
    ## bottomupMeasuresNonFin = c("PU60_US_SEL3", "PR60_US_SEL3", "PD_PU60_US_SEL3",
    ## "PD_PR60_US_SEL3"),
    ## weighted = FALSE,
    ## includeStats = c("N", "NOBS", "SUM", "MEAN", "SD", "0%", "10%",a
    ## "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
    if (weightedNonFin == FALSE){
        ..zscore.. <- copy(zscore.agg)
    } else {
        ..zscore.. <- copy(zscore.agg.w)
    }

    ..zscore..[NAME %in% bottomupMeasuresNonFin &
              fic != "",
           c("fic","DATE","NAME",includeStatsNonFin), with = FALSE] %>>%
    data.table:::melt.data.table(id.vars = c('fic','DATE','NAME')) -> ..zscore..2

    ..zscore..2[, NEW := sprintf("%s_%s",NAME,variable)]
    
    ..zscore..2 %>>%
    dcast.data.table(fic + DATE ~ NEW,
                     value.var = 'value') -> ..zscore..3

    ..zscore..3 %>>% setnames(old = c('fic',"DATE"),
                          new = c("iso3","date"))

    res[['bottomupMeasuresNonFinDT']] <- ..zscore..3
    
    
    ## FINANCIAL SECTOR RISK MEASURE
    ## bottomupMeasuresFin = c('SC_CLOSURE_ALL','SC_OBR_EU'),
    ## weighted = FALSE,
    ## includeStats = c('MEAN'),
    load(file = './data/bottomup.RData')
    if (weightedFin == FALSE){
        ..bankpd.. <- copy(bankpd.agg)
    } else {
        ..bankpd.. <- copy(bankpd.agg.w)
    }

    ..bankpd..[NAME %in% bottomupMeasuresFin &
              fic != "",
           c("fic","DATE","NAME",includeStatsFin), with = FALSE] %>>%
    data.table:::melt.data.table(id.vars = c('fic','DATE','NAME')) -> ..bankpd..2

    ..bankpd..2[, NEW := sprintf("%s_%s",NAME,variable)]
    
    ..bankpd..2 %>>%
    dcast.data.table(fic + DATE ~ NEW,
                     value.var = 'value') -> ..bankpd..3

    ..bankpd..3 %>>% setnames(old = c('fic',"DATE"),
                          new = c("iso3","date"))

    res[['bottomupMeasuresFinDT']] <- ..bankpd..3

    ## OTHER MACRO INDICATORS
    ## GDPMeasures = c(),
    ## PublicDebtMeasures = c("E_GCG_E", "DE_GCG_E", "DE_GCG_G", "DE_GGG_G",
    ## "PP_GE_E", "PP_GE_G", "DE_G_GDP_COMP"),
    ## HousePriceMeasures = c(),
    ## DeficitMeasures = c(),
    ## TaxRevMeasures = c(),
    ## OtherMacroMeasures = c('AMT.A.PCT.P1')
    load(file = './data/otherdata.RData')
    ..rrdebt.. <- copy(rrdebt)
    res[['PublicDebtMeasuresDT']] <-
        ..rrdebt..[,.SD,
                   .SDcols =
                       c('iso3',
                         'date',
                         PublicDebtMeasures)]
    
    ## Other macro variables
    load(file = './data/macrodata.RData')
    macrodata[, UNITDENOM := 1]

    .keys <- macrodata[,
                       .SD,
                       .SDcols = c('iso3','date')]   

    .macrodata. <- macrodata[,.SD,
                             .SDcols = c(
                                 ## GDPMeasures,
                                 ## HousePriceMeasures,
                                 ## DeficitMeasures,
                                 ## TaxRevMeasures,
                                 OtherMacroMeasures)]
    
    .macrodata. <- cbind(.keys, .macrodata.)
    
    .macrodata. %>>%
    data.table(key = c('iso3','date')) %>>% unique -> res[['OtherMacroMeasuresDT']]


    ## House prices
    if (HousePriceMeasures == TRUE){
        res[['HousePriceMeasuresDT']] <- house
    }
    
    ## FINAL TOUCHES
    ## frequency = 'annual',
    ## carryForwardVars = 'ratingnum',
    ## winsorVars = c(),
    ## winsorLevel = 0.01,

    res %>>%
    joinDatasetList() -> finalDT

    ## ## DOWNLOAD SPECIFICATION
    ## outfile = 'result.dta', filetype = c('stata','csv')

    return(finalDT)    
}
