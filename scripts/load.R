require(devtools)
setwd('/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/R/PACKAGES/SovereignCrisis')

load_all()

devtools::document()
devtools::session_info()

require(WorldBankAPI)

WorldBankAPI:::getWorldBankSources()
ctry <- WorldBankAPI:::getWorldBankCountries()

ctry[, table(incomeLevel.id)]

ctry[incomeLevel.id == "OEC"][['name']]
ctry[incomeLevel.id == "NOC"][['name']]
ctry[incomeLevel.id == "UMC"][['name']]


