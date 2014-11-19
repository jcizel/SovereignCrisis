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
