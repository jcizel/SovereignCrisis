ctry <- WorldBankAPI:::getWorldBankCountries()

unique(ctry[,list(incomeLevel.id, incomeLevel.value)])

ctry[incomeLevel.id %in% c('NOC','OEC')]


## -------------------------------------------------------------------------- ##
## CONSTRUCT A LOOKUP TABLE WITH ALL AVAILABLE VARIABLES                      ##
## -------------------------------------------------------------------------- ##
