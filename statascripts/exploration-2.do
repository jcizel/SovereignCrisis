
cd "/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/R/PACKAGES/SovereignCrisis"

do ./ado/benchmark.regressions.ado

use ./inst/extdata/stata/TEST-2015-01-15.dta, clear

format date %d

cap gen year = year(date)
cap egen ID = group(iso3)
xtset ID year

estimateBankDistressModel if year>2000, zscore(PU60_US_SEL3_50_) bscore(SC_CLOSURE_ALL_90_)
estimateBankDistressModel if year>2000, zscore(PU60_US_SEL3_10_) bscore(SC_CLOSURE_ALL_50_)


global zscore PU60_US_SEL3_50_
global bscore SC_CLOSURE_ALL_90_

areg s1.houseix l(0/2)s1.($zscore $bscore) if year > 2000, a(year)


reg s1.GC_TAX_TOTL_GD_ZS l(0/3)s1.($zscore $bscore), vce(cluster year)
reg s1.GC_TAX_TOTL_GD_ZS l1s3.($zscore $bscore), vce(cluster year
areg f1s1.GC_TAX_TOTL_GD_ZS s5.($zscore $bscore), a(year)
areg f1s1.GC_TAX_TOTL_GD_ZS s5.($zscore $bscore)

areg s1.DE_G_GDP_COMP s3.($zscore $bscore),a(year)



                                                     
areg f1s1.GC_TAX_TOTL_GD_ZS s5.($zscore $bscore), a(year)
areg f1s1.GC_TAX_TOTL_GD_ZS s5.($zscore $bscore)                                                     


areg ratingnum DE_G_GDP_COMP, a(ID)
areg s1.ratingnum l(0/2)s1.DE_G_GDP_COMP, a(year)
areg s1.ratingnum l(0/2)s1.houseix, a(year)

areg s1.ratingnum s1.(houseix DE_G_GDP_COMP), a(year)
areg s1.cds s1.(houseix DE_G_GDP_COMP), a(year)
areg s1.spread s1.(houseix DE_G_GDP_COMP), a(year)

areg s1.cds l(0/2)s1.GC_XPN_INTP_RV_ZS, a(year)

