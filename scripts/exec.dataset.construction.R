require('WorldBankAPI')

# ---------------------------------------------------------------------------- #
# NEED THE FOLLOWING MACROECONOMIC VARIABLES:                                  #
#                                                                              #
# 1. GOVERNMENT REVENUE                                                        #
# 2. GOVERNMENT EXPENDITURE                                                    #
# 3. EXTERNAL GOVERNMENT DEBT                                                  #
# 4. GDP                                                                       #
# 5. UNEMPLOYMENT                                                              #
# 6.                                                                           #
#                                                                              #
# ---------------------------------------------------------------------------- #

## GOVERNMENT REVENUE
options(width = 200)


queryMacroDatasets('revenue')
queryMacroDatasets('expenditure')
queryMacroDatasets('debt') 
queryMacroDatasets('external')
queryMacroDatasets('tax revenue')
queryMacroDatasets('house')
queryMacroDatasets('gross domestic product')
queryMacroDatasets('gross national income')
queryMacroDatasets('unemployment')
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

## MANUALLY MARK THE CSV FILES CREATED IN THE ABOVE QUERIES, BY ADDING A
## `SELECT` THAT EQUALS 1 IF THE VARIABLE IS TO BE INCLUDED IN THE FINAL DATASET. 

dt <- createQueriedMacroDataset()




