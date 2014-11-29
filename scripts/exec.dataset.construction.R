require('WorldBankAPI')

WorldBankAPI::queryWorldBankVariableList("external debt") %>>%
(? .$name) %>>%
invisible


install_github('jcizel/SDMXWrappers')
require(SDMXWrappers)
GeneralUtilities::listAllFun('SDMXWrappers')
v <- SDMXWrappers::getListOfVariables(provider = 'ECB')
