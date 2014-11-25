ctry <- WorldBankAPI:::getWorldBankCountries()

unique(ctry[,list(incomeLevel.id, incomeLevel.value)])

ctry[incomeLevel.id %in% c('NOC','OEC')]


## -------------------------------------------------------------------------- ##
## CONSTRUCT A LOOKUP TABLE WITH ALL AVAILABLE VARIABLES                      ##
## -------------------------------------------------------------------------- ##






## -------------------------------------------------------------------------- ##
## IMPORT BANKSCOPE AGGREGATES                                                ##
## -------------------------------------------------------------------------- ##
bs <- fread('./inst/extdata/Bank PDs/BSFIN_AGGREGATED.csv')
