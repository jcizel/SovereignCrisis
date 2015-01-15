/* -------------------------------------------------------------------------- */
/* REGRESSIONS                                                                */
/* -------------------------------------------------------------------------- */

cd ./inst/extdata/stata/

run macro-dataset-labels.do, nostop


/* EXPLAIN RATINGS, CDS, AND BOND SPREADS                                     */

gen year = year(date)
egen ID = group(iso3)
xtset ID year    
    
global vars ratingnum cds spread
li iso3 year ratingnum l(0/4)s1.(ratingnum) if ratingnum != .

pwcorr l(0/4)s1.($vars)


/* -------------------------------------------------------------------------- */
/* ESTIMATE                                                                   */
/* -------------------------------------------------------------------------- */
estimates drop _all
global if if year>2000


areg ratingnum l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(iso3)
est sto rat_CFE
estadd local effects "Country"

areg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(year)
est sto rat_YFE
estadd local effects "Year"

reg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if
est sto rat_pooled
estadd local effects ""

areg cds l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(iso3)
est sto cds_CFE
estadd local effects "Country"

areg s1.cds l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(year)
est sto cds_YFE
estadd local effects "Year"

reg s1.cds l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if
est sto cds_pooled
estadd local effects ""

areg spread l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(iso3)
est sto spread_CFE
estadd local effects "Country"

areg s1.spread l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if, a(year)
est sto spread_YFE
estadd local effects "Year"

reg s1.spread l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) $if
est sto spread_pooled
estadd local effects "None"



*EXPORT TABLE TO LATEX DOCUMENT
*
*Fragment options: export just the inner part of the table and edit the table
*title and notes in the latex file directly.
*
*Sources: http://www.jwe.cc/2012/03/stata-latex-tables-estout/

local filename "test2"
local dir "/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/Sovereign Distress/RESULTS"
    
local OUTPUT_FILE_NAME `filename'
local PATH_TABLES `dir'
local OPT_ESTOUT_COEF cells(b(star fmt(%9.3f) label("")) se(fmt(%9.3f) label("") par([ ])))
local OPT_ESTOUT_STATS stats(N r2 effects, fmt(%9.0f %9.3f) labels("Observations" "Adj. R2" "Effects") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{>{\centering\arraybackslash}p{1cm}}{@}"))
ocal OPT_ESTOUT_LABEL eqlabels(none) label nomtitles collabels(none) sub(_ \_) nonum


local OPT_ESTOUT_OUTPUT booktabs star(* 0.10 ** 0.05 *** 0.01) replace type
esttab rat* using "`PATH_TABLES'/`OUTPUT_FILE_NAME'-rat.tex", `OPT_ESTOUT_COEF' `OPT_ESTOUT_STATS' `OPT_ESTOUT_LABEL' `OPT_ESTOUT_OUTPUT' plain fragment `nolines' `lines'  `OPT_ESTOUT_G'
esttab cds* using "`PATH_TABLES'/`OUTPUT_FILE_NAME'-cds.tex", `OPT_ESTOUT_COEF' `OPT_ESTOUT_STATS' `OPT_ESTOUT_LABEL' `OPT_ESTOUT_OUTPUT' plain fragment `nolines' `lines'  `OPT_ESTOUT_G'
esttab spread* using "`PATH_TABLES'/`OUTPUT_FILE_NAME'-spread.tex", `OPT_ESTOUT_COEF' `OPT_ESTOUT_STATS' `OPT_ESTOUT_LABEL' `OPT_ESTOUT_OUTPUT' plain fragment `nolines' `lines'  `OPT_ESTOUT_G'
