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

## .lookupISOCode(query = c("SVN","AUT"), queryType = 'iso3')

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

## getBloombergSovCDS()

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

## getSPRatings()

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

    out <-
        o[, list(iso3,
                 date,
                 spread)]
    return(out)
}

## spread <- getSovBondSpreads()
## spread[iso3=='ARG', qplot(date,spread,geom = 'line')]
