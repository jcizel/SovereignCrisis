crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()
alt     <- getAltmanZscore()
imf     <- getIMFIFS()
bs      <- getAggregatedBankscope()
pd      <- getAggregatedBankscopePDs()
select  <- createQueriedMacroDataset()


dt <- augmentBenchmarkDataset(
    crisisdb = crisis1,
    dtList =
        list(alt
             )
)

dt
dt[, `:=`(rating.dif = shift(ratingnum, dif = TRUE, relative = FALSE),
          cds.dif = shift(cds, dif = TRUE, relative = FALSE),
          spread.dif = shift(spread, dif = TRUE, relative = FALSE)), by = iso3]

varsel <- c('ratingnum','cds','spread','zscorepd75')
dt[, paste0(varsel,'.dif') := lapply(.SD, function(x) {
    o <- winsorize(x, method = 'IQR', k = 2, trim = FALSE)
    o <- shift(o, dif = TRUE, relative = FALSE)
    o
})
   , by = c('iso3'),
   , .SDcols = varsel]


p <- ggplot(data = dt)
p <- p + geom_point(aes_string(x = 'spread.dif',
                               y = 'zscorepd75.dif'))
p <- p + theme_bw()
p

## matrix
p <- ggpairs(data = dt[, paste0(varsel,'.dif'), with = FALSE])
p <- p + theme_bw()
p



## -------------------------------------------------------------------------- ##
## PLOT THE BENCHMARK TIME SERIES FOR AN INDIVIDUAL COUNTRY                   ##
## -------------------------------------------------------------------------- ##

plotDefinition =
    list('ratingnum' =
             list(data = ratings,
                  y = 'ratingnum',
                  ylabel = 'S&P Sovereign Credit Rating',
                  idCol = 'iso3'),
         'cds' =
             list(data = cds,
                  y = 'cds',
                  ylabel = '5-Year Sovereign CDS Spread',
                  idCol = 'iso3'),
         'spread' =
             list(data = spreads,
                  y = 'spread',
                  ylabel = 'Sovereign Bond Yield Spread',
                  idCol = 'iso3'),
         'bop' =
             list(data = imf,
                  y = 'L78CB&D',
                  ylabel = 'BOP',
                  idCol = 'iso3'),
         'alt' =
             list(data = pd,
                  y = 'SC_CLOSURE_ALL',
                  ylabel = 'Banking Score',
                  idCol = 'iso3'))

## undebug(plotSovBenchmarks)
plotSovBenchmarks(isoSel = "NLD",
                  crisisdb = alternativeCrisisDB(),
                  crisistype = 'ratingdrop',
                  limits = as.Date(c('1995-01-01','2013-12-01')),
                  filename = '~/Downloads/test.pdf',
                  width = 320,
                  height = 420,
                  plotDefinition = plotDefinition)


## -------------------------------------------------------------------------- ##
##                               DENSITY PLOTS                                ##
## -------------------------------------------------------------------------- ##

dtList =
    list(pd)

plotDefinition <- 
    list('ratingnum' =
             list(x = 'ratingnum',
                  xlabel = 'S&P Sovereign Credit Rating'),
         'cds' =
             list(x = 'cds',
                  xlabel = '5-Year Sovereign CDS Spread'),
         'spread' =
             list(x = 'spread',
                  xlabel = 'Sovereign Bond Yield Spread'),
         'pd' =
             list(x = 'SC_CLOSURE_ALL',
                  xlabel = ""))

groups <-
    list("[-4:-1]"=expression(COUNTDOWN %between% c(-4,-1)),
         "[0]"=expression(COUNTDOWN == 0),
         "[1:4]"=expression(COUNTDOWN %between% c(1,4)))    

plotDensityAroundCrisisEvents(crisisdb = loadCrisisDB(),                              
                              crisisType = "debtcrisis",
                              adjust = TRUE,
                              filename = './inst/RESULTS/plotSovereignBenchmarkDens.pdf',
                              dtList = dtList,
                              plotDefinition = plotDefinition,
                              groups = groups)

plotDensityAroundCrisisEvents(crisisdb = loadCrisisDB(),                              
                              crisisType = "debtcrisis",
                              adjust = TRUE,
                              ## filename = './inst/RESULTS/plotSovereignBenchmarkDens.pdf',
                              dtList = dtList,
                              plotDefinition = plotDefinition,
                              groups = groups)
                                  

## -------------------------------------------------------------------------- ##
##                               EVENT STUDY                                  ##
## -------------------------------------------------------------------------- ##
plotDefinition <- 
    list(                       
        ## list(y = 'value',
        ##      ylabel = unique(dt$indicator.value)),
        ## list(y = 'DE_G_GDP_COMP',
        ##      ylabel = 'Debt to GDP'),
        ## list(y = 'DT.DOD.DIMF.CD',
        ##      ylabel = 'Use of Funds'),
        list(y = 'SC_CLOSURE_ALL',
             ylabel = '')
    )

groups =
    list("[-4:-1]"=expression(COUNTDOWN %between% c(-4,-1)),
         "[0]"=expression(COUNTDOWN == 0),
         "[1:4]"=expression(COUNTDOWN %between% c(1,4)))
dtList =
    list('wb' = dt,
         'rrdebt' = getRR.debt(),
         'imf' = getIMFIFS(),
         'pd' = pd)

plotEventStudy(crisisdb = loadCrisisDB(),                              
               crisisType = "debtcrisis",
               adjust = TRUE,
               filename = '~/Downloads/test.pdf',
               dtList = dtList,
               plotDefinition = plotDefinition,
               groups = groups)
