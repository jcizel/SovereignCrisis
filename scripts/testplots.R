imf <- getIMFIFS()
attributes(imf)$colnames
o <-
    summarizeDataAvailability(dt = imf)

ratings <- getSPRatings()
cds <- getBloombergSovCDS()
spreads <- getSovBondSpreads()
alt <- getAltmanZscore()
summarizeDataAvailability(alt)

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
             list(data = alt,
                  y = 'zscorepd75',
                  ylabel = 'ZScore PD (75th perc.)',
                  idCol = 'iso3'))

undebug(plotSovBenchmarks)
plotSovBenchmarks(isoSel = "ESP",
                  crisisdb = alternativeCrisisDB(),
                  crisistype = 'ratingdrop',
                  limits = as.Date(c('1995-01-01','2013-12-01')),
                  filename = '~/Downloads/test.pdf',
                  width = 320,
                  height = 420,
                  plotDefinition = plotDefinition)


## -------------------------------------------------------------------------- ##
## CHECK DENSITY PLOTS AROUND EVENT                                           ##
## -------------------------------------------------------------------------- ##
alt <- getAltmanZscore()
crisis <- alternativeCrisisDB()
bench <- getSovBenchmarks()

dt <- augmentBenchmarkDataset()
## undebug(plotDensityAroundCrisisEvents)
plotDensityAroundCrisisEvents(crisisdb = loadCrisisDB(),                              
                              crisisType = "debtcrisis",
                              adjust = TRUE,
                              plotDefinition =
                                  list('ratingnum' =
                                           list(x = 'ratingnum',
                                                xlabel = 'S&P Sovereign Credit Rating'),
                                       'cds' =
                                           list(x = 'cds',
                                                xlabel = '5-Year Sovereign CDS Spread'),
                                       'spread' =
                                           list(x = 'spread',
                                                xlabel = 'Sovereign Bond Yield Spread'),
                                       'zscorepd75' =
                                           list(x = 'zscorepd75',
                                                xlabel = "")),
                              groups =
                                  list("[-4:-1]"=expression(COUNTDOWN %between% c(-4,-1)),
                                       "[0]"=expression(COUNTDOWN == 0),
                                       "[1:4]"=expression(COUNTDOWN %between% c(1,4))))


