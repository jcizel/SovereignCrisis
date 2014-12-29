cd ./inst/extdata/stata/

run macro-dataset-labels.do, nostop

* Debt
su GFDD_DM_01 GFDD_DM_02 GFDD_DM_03 GFDD_DM_04 GFDD_DM_05 GFDD_DM_06 GFDD_DM_07 GFDD_DM_08 GFDD_DM_09 GFDD_DM_10 if iso3 == "SVN"
li year  AMT_A_PCT_1 AMT_A_PCT_P1 AMT_A_PCT_P2 AMT_A_PCT_P3 if iso3=="NLD"

* Correlations between CS, ratings, and CDS
global vars ratingnum cds spread

li iso3 year ratingnum l(0/4)s1.(ratingnum) if ratingnum != .

areg  s1.ratingnum l(1/4)s1.(ratingnum), a(iso3)


areg  s1.ratingnum l(1/4)s1.(ratingnum), a(iso3)
areg  s1.cds l(1/4)s1.(cds), a(iso3)
areg  s1.SC_CLOSURE_ALL_P90_ l(1/4)s1.(SC_CLOSURE_ALL_P90_), a(iso3)
areg  s1.zscorepd90 l(1/4)s1.(zscorepd90), a(iso3)


var s1.(ratingnum cds spread)


pwcorr l(0/4)s1.($vars)

xtabond s1.ratingnum ls1.ratingnum zscorepd90 SC_CLOSURE_ALL_P90_

areg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)
areg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_), a(year)
reg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_)


areg s1.ratingnum l(0/2)s1.(zscorepd90 SC_CLOSURE_ALL_P90_) if year>2000, a(year)


areg ratingnum l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)
areg ratingnum l(1).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)
areg ratingnum l(2).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)


areg cds l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)
areg spread l(0/2).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)



areg cds l(0).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)
areg spread l(0).(zscorepd90 SC_CLOSURE_ALL_P90_), a(iso3)

