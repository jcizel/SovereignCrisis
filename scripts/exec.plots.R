crisis1 <- loadCrisisDB()
crisis2 <- alternativeCrisisDB()
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()
alt     <- getAltmanZscore()
imf     <- getIMFIFS()
bs      <- getAggregatedBankscope()
pd      <- getAggregatedBankscopePDs()
## select  <- createQueriedMacroDataset(test = FALSE)


dt <- augmentBenchmarkDataset(
    crisisdb = crisis1,
    dtList =
        list(alt,
             select
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
         'GFDD.SI.01' =
             list(data = macro,
                  y = 'GFDD.DM.06',
                  ylabel = 'test',
                  idCol = 'iso3'),
         'GFDD.SI.02' =
             list(data = macro,
                  y = 'GFDD.DM.04',
                  ylabel = 'test',
                  idCol = 'iso3')         
         )

## undebug(plotSovBenchmarks)
plotSovBenchmarks(isoSel = "USA",
                  crisisdb = crisis1,
                  crisistype = 'debtcrisis',
                  limits = as.Date(c('1985-01-01','2013-12-01')),
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



## -------------------------------------------------------------------------- ##
##                     PLOT BENCHMARK MEASURS                                 ##
## -------------------------------------------------------------------------- ##
ratings <- getSPRatings()
cds     <- getBloombergSovCDS()
spreads <- getSovBondSpreads()

library(dygraphs)

ids = ratings$iso3 %>>% unique
dates = .fCrDates(begin="1960-01-01",end="2014-07-31", frequency=apply.weekly)[[2L]]

.index <- CJ(iso3 = ids,date = dates)

rat <- ratings[,list(ratingnum),key=c('iso3','date')][.index[,key = c('iso3', 'date')], roll = TRUE]

rat %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'ratingnum',
                 na.rm = TRUE) %>>%
( dt ~ xts(dt, order.by = dt$date) ) -> ratm

dygraph(ratm[,c('USA','SVN','GRC','ESP',"NLD","BEL")]) %>>%
dyHighlight(highlightCircleSize = 5,
                          highlightSeriesBackgroundAlpha = 0.2,
                          hideOnMouseOut = FALSE) %>>%
dyRangeSelector()

## CDS
ids = cds$iso3 %>>% unique
dates = .fCrDates(
    begin = cds$date %>>% min,
    end = cds$date %>>% max,
    frequency = apply.weekly
)[[2L]]

.index <- CJ(iso3 = ids,date = dates)
cds <-
    (data.table(cds,key = c('iso3','date'))%>>%unique)[data.table(.index, key = c('iso3','date')), roll = TRUE]

cds %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'cds',
                 na.rm = TRUE) %>>%
( dt ~ xts(dt[,.SD,.SDcols = -c('date')], order.by = dt$date) ) -> cdsm

## SPREAD
ids = spreads$iso3 %>>% unique
dates = .fCrDates(
    begin = spreads$date %>>% min,
    end = spreads$date %>>% max,
    frequency = apply.weekly
)[[2L]]

.index <- CJ(iso3 = ids,date = dates)
spreads <-
    (data.table(spreads,key = c('iso3','date'))%>>%unique)[data.table(.index, key = c('iso3','date')), roll = TRUE]

spreads %>>%
dcast.data.table(date ~ iso3,
                 value.var = 'spread',
                 na.rm = TRUE) %>>%
( dt ~ xts(dt[,.SD,.SDcols = -c('date')], order.by = dt$date) ) -> spreadsm


haireye = as.data.frame(HairEyeColor) %>>% data.table
n1 <-
    rCharts::nPlot(Freq ~ Hair,
                   group = "Eye",
                   data =  haireye[Sex == 'Male'],
                   type = 'multiBarChart')
n1

cars %>>% data.table -> cars
n2 <-
    rCharts::nPlot(
        speed ~ dist,
        data = cars,
        type = 'scatterChart',
        onlyCircles = TRUE
    )
n2


a <- rCharts::Highcharts$new()
a$chart(type = "spline", backgroundColor = NULL)
a$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
a$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 1), dashStyle = "shortdot")
a$legend(symbolWidth = 80)
a$set(height = 250)
a




data[1:4] %>>% RJSONIO::toJSON(.withNames = FALSE)

movieTable <- read.table("~/Downloads/movies.txt", header = T)

# Split the list into categories
movieSeries <- lapply(split(movieTable, movieTable$category), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    return(res)
})


# Create the chart object
a <- rCharts::Highcharts$new()
invisible(
    sapply(movieSeries, function(x) {
        a$series(data = x, type = "scatter", name = x[[1]]$category)
    })
)

a$chart(backgroundColor = NULL)

a$plotOptions(
    scatter = list(
        cursor = "pointer",
        marker = list(
            symbol = "circle",
            radius = 5
        )
    )
)

a$xAxis(title = list(text = "Critics Score"), labels = list(format = "{value} %"))
a$yAxis(title = list(text = "Audience Score"), labels = list(format = "{value} %"))

a$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")





## NEW EXAMPLE
MASS::survey %>>% data.table  -> test
test[, list(x = Pulse, y = Age)] %>>%
plyr::alply( 1, as.list) -> data
attributes(data) <- NULL
names(data) <- NULL

a <- rCharts::Highcharts$new()
a$series(data = data, type = 'scatter', name = 'Test')

a$chart(backgroundColor = NULL)

a$plotOptions(
    scatter = list(
        cursor = "pointer",
        marker = list(
            symbol = "circle",
            radius = 5
        )
    )
)

a$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")

a$xAxis(title = list(text = "Critics Score"), labels = list(format = "{value} %"))
a$yAxis(title = list(text = "Audience Score"), labels = list(format = "{value} %"))


## EXAMPLE 2
movies %>>% data.table -> movies
movies[, x:=rating]
movies[, y:=votes]

movies[1:1000] %>>%
plyr::alply(1,as.list) -> data
attributes(data) <- NULL
names(data) <- NULL

data %>>%
list.group(as.character(mpaa)) -> data


a <- rCharts::Highcharts$new()
data %>>% lapply(function(l){
    a$series(data = l, type = 'scatter', name = l$mpaa %>>% unique )
}) %>>% invisible

a <- rCharts::Highcharts$new()
data %>>%
list.map({
    a$series(data = ., type = 'scatter', name = .name)
}) %>>% invisible

a$chart(backgroundColor = NULL)

a$plotOptions(
    scatter = list(
        cursor = "pointer",
        marker = list(
            symbol = "circle",
            radius = 5
        )
    )
)

a$exporting(enabled = TRUE)

a$tooltip(useHTML = T, formatter = "#! function() { return this.point.title; } !#")

a$xAxis(title = list(text = "Critics Score"), labels = list(format = "{value} %"))
a$yAxis(title = list(text = "Audience Score"), labels = list(format = "{value} %"))

a


b <- Nvd3$new()

system.file(
  "/libraries/nvd3/layouts/multiChart.html",
      package = "rCharts"
    )



## -------------------------------------------------------------------------- ##
## RATINGS                                                                    ##
## -------------------------------------------------------------------------- ##
isoSel <- c('GRC')
rat[,x := as.numeric(as.POSIXct(date)) * 1000]
rat[,y := ratingnum]
rat[iso3 %in% isoSel & !is.na(ratingnum), list(x,y)] -> data
data %>>%
plyr::alply(1, as.list) -> data
attributes(data) <- NULL
names(data) <- NULL

p <- rCharts::Highcharts$new()
p$chart(zoomType = "xy", type = "line")

p$series(data = data, tooltip = list(valueDecimals = 2))
p$setTemplate(script="/Users/jankocizel/Downloads/highstock.html")
p$addAssets(
    js = c(
        "https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
        "https://code.highcharts.com/stock/highstock.js",
        "https://code.highcharts.com/modules/exporting.js"
    )
)
p$addParams(height = 400, width=1000, dom="highstock")
p$save(destfile = '~/Downloads/highstock-test.html')
