.updateISOCodes <- function(){
    ctry <- WorldBankAPI:::getWorldBankCountries()

    write.csv(x = ctry,
              file = './inst/extdata/countries.csv')
    return(NULL)
}

## .updateISOCodes()

.lookupISOCode <- function (query) 
{
    .files <- list.files('./inst/extdata')

    if ('countries.csv' %in% .files){
        ctry <- fread(input = './inst/extdata/countries.csv')
    } else {
        ctry <- WorldBankAPI:::getWorldBankCountries()
    }
    
    isotype <- max(nchar(query))
    
    if (isotype == 2){
        setkeyv(ctry, "iso2Code")
        out <- ctry[J(query)][['id']]
    } else {
        setkeyv(ctry, "id")
        out <- ctry[J(query)][['iso2Code']]        
    }
    
    return(out)
}

## .lookupISOCode(query = c("SVN","AUT"))

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
