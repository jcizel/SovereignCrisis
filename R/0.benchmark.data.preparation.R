.trim <- function(str) gsub("^\\s+|\\s+$", "", str)

.condense <- function(str){
    str.out<- paste(
        {
            .x <- unique(.trim(str))
            .x[order(.x)]
        },
        collapse = "; "
    )
    return(str.out)
}

.toDate <- function(x){
    x      <- as.factor(x)
    levels <- levels(x)
    dates  <- as.Date(levels,format = "%Y-%m-%d")
    return(dates[x])
}

.updateISOCodes <- function(){
    ctry <- WorldBankAPI:::getWorldBankCountries()

    write.csv(x = ctry,
              file = './inst/extdata/countries.csv')
    return(NULL)
}

## .updateISOCodes()

.lookupISOCode <- function (query,
                            queryType = NULL) 
{
    .files <- list.files('./inst/extdata')

    if ('countries.csv' %in% .files){
        ctry <- fread(input = './inst/extdata/countries.csv')
    } else {
        ctry <- WorldBankAPI:::getWorldBankCountries()
    }
    
    isotype <- max(nchar(query))

    if (is.null(queryType)){
        if (isotype == 2){
            setkeyv(ctry, "iso2Code")
            out <- ctry[J(query)][['id']]
        } else {
            setkeyv(ctry, "id")
            out <- ctry[J(query)][['iso2Code']]        
        }
    } else if (queryType == 'name'){
        ctry[, country := toupper(name)]
        setkeyv(ctry, "country")
        out <- ctry[J(toupper(query))][['id']]                
    } else if (queryType == 'iso2') {
        setkeyv(ctry, "iso2Code")
        out <- ctry[J(query)][['id']]
    } else if (queryType == 'iso3') {
        setkeyv(ctry, "id")
        out <- ctry[J(query)][['iso2Code']]        
    }
    
    return(out)
}

.lookupVariable <- function(query,
                            lookup.table,
                            varNameCol,
                            return.cols = NULL){
    setkeyv(lookup.table, varNameCol)
    o <- lookup.table[J(query)]

    if (is.null(return.cols)){
        out <- o[[1]]
    } else {
        res <- list()
        for (x in return.cols){
            res[[x]] <- o[[x]]
        }
        
        out <- Reduce(function(x,y) paste0(as.character(x),"; ",as.character(y)), res)
    }
    return(out)
}

## .lookupISOCode(query = c("SVN","AUT"), queryType = 'iso3')


.fCrDates  <- function(begin = '2000-01-01', end = '2013-05-01',
                      frequency = apply.quarterly){
    dates <- seq(from = as.Date(begin), to=as.Date(end),by=1)
    dates <- xts(dates,order.by=dates)
    dates <- data.frame(date=frequency(dates, tail, n=1))
    dates <- data.table(dates)[,list(tix = seq_along(date), date = as.Date(date))]
    return(dates)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create a dataset of Sovereign CDS Spreads (source: Bloomberg) 
##' @return data.table with cds
##' @author Janko Cizel
getBloombergSovCDS <- function(){

    cds <- fread(input = './inst/extdata/SOVEREIGN/Bloomberg CDS/Bloomberg Sovereign CDS.csv')
    
    cds[, date := as.Date(as.character(DATE), format = "%m/%d/%Y")]
    cds[, DATE := NULL]

    cds <- data.table:::melt.data.table(cds,
                                        id.vars = c("date"),
                                        variable.name = "iso2",
                                        value.name = "cds"
                                        )
    
    cds[, iso3 := .lookupISOCode(as.character(iso2))]
    
    out <-
        cds[, list(date,
                   iso3,
                   cds)]
    
    return(out)
}

## cds <- getBloombergSovCDS()


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create a dataset of S&P sovereign credit ratings 
##' @return data.table with ratings
##' @author Janko Cizel
getSPRatings <- function(){
    ratings <- fread(input =  "./inst/extdata/SPRatingXpress/Sovereign Ratings Processed.csv")
    
    out <- 
        ratings[, list(iso3 = country,
                       date = as.Date(as.character(date), format = "%m/%d/%Y"),
                       rating = rating,
                       ratingnum = RATING_NUM)
                ][order(iso3, date)]

    return(out)
}

## ratings <- getSPRatings()
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create a IMF-IFS dataset 
##' @return data.table with resulting dataset (attribute 'colnames') contains
##' labeling information of variables.
##' @author Janko Cizel
getIMFIFS <- function(){
    imfifs <- fread(input = "./inst/extdata/IMF/IFS/IMF-IFS_annual.csv",
                    header = TRUE
                    )

    lookup <- read.xlsx2(file = './inst/extdata/IMF/IFS/IMF-IFS_variables.xlsx',
                         sheetIndex = 1 )
    l <- data.table(lookup)

    o1 <- 
        imfifs[!is.na(value) &
                   !is.na(iso3),
               list(iso3,
                    date = .toDate(date),
                    varcode,
                    value)]

    o2 <-
        data.table:::dcast.data.table(o1,
                                      iso3 + date ~ varcode,
                                      value.var = "value")

    attributes(o2)$colnames <- l

    return(o2)
}

## imf <- getIMFIFS()

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Get a dataset of sovereign bond spreads (source: World Bank and IMF) 
##' @return data.table of results
##' @author Janko Cizel
getSovBondSpreads <- function(){
    spread <- read.xlsx2(file = "./inst/extdata/World Bank/GEM/Sovereign Bond Interest Rate Spreads, basis points over US Treasuries.xlsx",
                       sheetName = "monthly",
                       header = TRUE,
                       check.names = FALSE
                       )[-1,]
    spread <- data.table(spread)

    spread[, date := {
        year = as.numeric(sapply(str_split(obs, "M"), function(x) x[[1]]))
        month = as.numeric(sapply(str_split(obs, "M"), function(x) x[[2]]))
        as.Date(paste0(year,"-",month,"-01"), format = "%Y-%m-%d")
    }]

    spread[, obs := NULL]
    
    o <- 
        data.table:::melt.data.table(spread, na.rm = T,
                                     id.vars = "date",
                                     variable.name = "country",
                                     value.name = "BOND.SPREAD")
    o[, iso3 := .lookupISOCode(
                 query = toupper(country),
                 queryType = 'name')]

    o[, spread := as.numeric(BOND.SPREAD)]
    o[, source := "wb"]
    
    ## FOR SOME COUNTRIES, ESPECIALLY DEVELOPED ONES, WORLD BANK DB DOESN NOT
    ## CONTAIN GOVERNMENT BOND YIELDS. FOR THESE COUNTRIES TAKE YIELDS FROM
    ## IMF-IFS INSTEAD

    ## of potential interest: L61, L60C, 
    imf <- getIMFIFS()[, list(iso3, date, yield = L60C)][!is.na(yield)]
    ## benchy <- getIMFIFS()[, list(iso3, date, yield = L61A)][iso3 == 'USA', list( date, bench = yield)]
    benchy <- imf[iso3 == 'USA', list( date, bench = yield)]

    setkey(imf, date)
    setkey(benchy, date)
    o2 <- benchy[imf, roll = TRUE][, list(iso3, date, spread = (yield - bench)*100)][!is.na(spread)]
    o2[, source := 'imf']

    
    out <-
        rbindlist(
            list(
                o[, list(iso3,
                         date,
                         spread,
                         source)],
                o2[, list(iso3,
                          date,
                          spread,
                          source)]
            )
        )

    ## IDENTIFY DUPLICATES
    out[, dup := {
        s <- unique(source)
        length(source) > 1
    }, by = list(iso3, year(date))]
    
    out <- out[ dup == FALSE
               | (dup == TRUE & source == 'wb')]

    setkey(out, date)
    setkey(benchy, date)
    out <- benchy[out, roll = TRUE]

    return(out[order(iso3, date)][!is.na(iso3)][, list(iso3, date, bench, spread, source)])
}

## spread <- getSovBondSpreads()
## spread[iso3=='DEU' & year(date) > 2000, qplot(date,spread,geom = 'line')]



getSovBenchmarks <- function(){
    data <- list()
    data[['cds']] <- getBloombergSovCDS()
    data[['sprating']] <- getSPRatings()
    data[['spreads']] <- getSovBondSpreads()

    ## Merge based on iso3 and date (make sure both are present in all datasets)

    ids = unique(as.character(unlist(lapply(data, function(x) unique(x$iso3)))))
    ids = ids[!is.na(ids) & ids!=""]
    dates = .fCrDates(begin="1960-01-01",end="2014-07-31", frequency=apply.yearly)[[2L]]

    out <- CJ(iso3 = ids,date = dates)

    for (x in names(data)){
        cat(x,"\n")
        b <- copy(data[[x]])
        b[, date := as.Date(date)]

        uniquecols <- setdiff(names(b),names(out))
        b <- b[,c("iso3","date",uniquecols), with = FALSE]
        
        setkey(out, iso3, date)
        setkey(b, iso3, date)
        out <- b[out, roll = 365]

        newcols <- setdiff(intersect(names(out), names(b)),c("iso3","date"))
        oldcols <- setdiff(names(out),newcols)

        ## cat(newcols,"\n",oldcols,"\n")
        setcolorder(out,c(oldcols,newcols))
    }

    return(out)
}

## bench <- getSovBenchmarks()
## summarizeDataAvailability(bench)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Summarize data availability for each cross-sectional unit over time
##' @param dt data.table containing the data
##' @param idCol column with cross-sectional unit identifiers
##' @param timeCol column with time-unit identifiers
##' @return data.table with a summary of data availability
##' @author Janko Cizel
summarizeDataAvailability <- function(dt,
                                      idCol = 'iso3',
                                      timeCol = 'date'){

    long <- data.table:::melt.data.table(dt,
                                         id.vars = c(idCol,timeCol))[!is.na(value)]

    out <- 
        long[,
             {
                 d <- get(timeCol)
                 d.min <- min(year(d))
                 d.max <- max(year(d))

                 if (d.min == d.max){
                     o <- paste0(d.min)
                 } else {
                     o <- paste0(d.min,"-",d.max)
                 }
                 list(period = o)
             }, keyby = c('variable',idCol)][,
                    {
                        o <- period
                        o <- paste(
                            paste0(get(idCol)," (",period,")"),
                            collapse = "; "
                        )
                        list(Availability = o)
                    }
                  , keyby = 'variable']

    return(out)
}

## o <- 
##     summarizeDataAvailability(dt = imf,
##                               idCol = 'iso3',
##                               timeCol = 'date')

## x <- 
##     .lookupVariable(query = o$variable,
##                     lookup.table = attributes(imf)$colnames,
##                     varNameCol = 'Variable',
##                     return.cols = c('Table','Concept', 'scale_desc')
##                     )

## o[, label := .lookupVariable(query = variable,
##               lookup.table = attributes(imf)$colnames,
##               varNameCol = 'Variable',
##               return.cols = c('Table','Concept', 'scale_desc')
##                              )]

## setcolorder(o, c("variable", "label", "Availability"))


## write.csv(x = o[order(nchar(Availability))],
##           file = "~/Downloads/test.csv")



## o <- 
##     summarizeDataAvailability(dt = ratings,
##                               idCol = 'iso3',
##                               timeCol = 'date')

## o <- 
##     summarizeDataAvailability(dt = cds,
##                               idCol = 'iso3',
##                               timeCol = 'date')

## o <- 
##     summarizeDataAvailability(dt = spread,
##                               idCol = 'iso3',
##                               timeCol = 'date')

## spread[iso3 == 'KOR', qplot(date, spread, geom = 'line')]
