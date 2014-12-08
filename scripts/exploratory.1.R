crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
alt     <- getAltmanZscore()
pd      <- getAggregatedBankscopePDs()
macro   <- createQueriedMacroDataset(test = FALSE)
macro %>>% save(file = './inst/extdata/rdata/macro-data.RData')

oecd.debt <- fread('../SDMXWrappers/inst/extdata/OECD/GOV_DEBT/DATATABLE.csv')
oecd.bank <- fread('../SDMXWrappers/inst/extdata/OECD/BPF1/DATATABLE.csv')
oecd.rev <- fread('../SDMXWrappers/inst/extdata/OECD/REV/DATATABLE.csv')
oecd.revla <- fread('../SDMXWrappers/inst/extdata/OECD/RSLACT/DATATABLE.csv')
oecd.nacc <- fread('../SDMXWrappers/inst/extdata/OECD/NAAG/DATATABLE.csv')
oecd.govt <- fread('../SDMXWrappers/inst/extdata/OECD/SNA_TABLE12/DATATABLE.csv')

dt <-
    list(
        crisis1,
        crisis2,
        alt,
        pd,
        macro,
        oecd.debt,
        oecd.bank,
        oecd.rev,
        oecd.revla,
        oecd.nacc,
        oecd.govt        
    ) %>>% joinDatasetList()


plotDefinition <-
    list(
        list(data = dt,
             y = 'GRS.A.USD.1_1_1',
             ylabel = '',
             idCol = 'iso3'),
        list(data = dt,
             y = 'AMT.A.PCT.P1',
             ylabel = '',
             idCol = 'iso3'),
        list(data = dt,
             y = 'FED.1000.TAXGDP',
             ylabel = '',
             idCol = 'iso3'),
        list(data = dt,
             y = 'FED.AA.TAXGDP',
             ylabel = '',
             idCol = 'iso3'),
        list(data = dt,
             y = 'GD41P.GS13.C',
             ylabel = '',
             idCol = 'iso3')        
    )


plotSovBenchmarks(isoSel = "USA",
                  crisisdb = crisis1,
                  crisistype = 'debtcrisis',
                  limits = as.Date(c('1985-01-01','2013-12-01')),
                  filename = '~/Downloads/test.pdf',
                  width = 320,
                  height = 420,
                  plotDefinition = plotDefinition)



