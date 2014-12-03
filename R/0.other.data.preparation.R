getAggregatedBankscopeDatabase <- function(stat = '_MEDIAN_'){
    bs <- fread('./inst/extdata/Bank PDs/BSFIN_AGGREGATED.csv')
    bs[, iso3 := .lookupISOCode(CTRYCODE)]
    bs[, date := as.Date(as.character(DATE), format = "%Y%m%d")]

    setnames(bs,
             names(bs),
             gsub('[[:punct:]]','.',names(bs)))

    stat <- gsub('[[:punct:]]','.',stat)
    
    .f <- paste0('iso3 + date ~ .NAME.')
    o <- data.table:::dcast.data.table(
        data = bs[!is.na(iso3)],
        formula = as.formula(.f),
        value.var = stat
    )
    return(o)
}

getAggregatedBankscopePDs <- function(
    vars = c('SC_CLOSURE_ALL','SC_OBR_EU'),
    stats = c('_MEDIAN_','_Q3_','_P90_')
)
{
    bs <- fread('./inst/extdata/Bank PDs/BANK_PD_DATASET.csv')
    bs[, iso3 := .lookupISOCode(CTRYCODE)]
    bs[, date := as.Date(as.character(DATE), format = "%Y%m%d")]
    setnames(bs,
             names(bs),
             gsub('[[:punct:]]','.',names(bs)))

    bs <- bs[.NAME. %in% vars]

    dt.list <- 
        foreach(x = stats) %do% {
            .s <- gsub('[[:punct:]]','.',x)
            
            .f <- paste0('iso3 + date ~ .NAME.')
            o <- data.table:::dcast.data.table(
                data = bs[!is.na(iso3)],
                formula = as.formula(.f),
                value.var = .s
            )
            
            cols <- setdiff(names(o),c('iso3','date'))
            setnames(o,
                     cols,
                     paste0(cols,.s))
            return(copy(o))
        }

    out <-
        Reduce(function(...) merge(..., by = c('iso3','date')), dt.list)
    
    return(out)
}

## pd <- getAggregatedBankscopePDs()

getAltmanZscore <- function(){

    alt <- fread(input = './inst/extdata/Altman PDs/ZSCORE_AGGREGATED.csv')


    alt[, date := {
        d <- DATE
        year = as.numeric(sapply(stringr:::str_split(d, ":"), function(x) x[[1]]))
        quarter = as.numeric(sapply(stringr:::str_split(d, ":"), function(x) x[[2]]))
        as.Date(paste0(year,"-",quarter*3,"-30"), format = "%Y-%m-%d")
    }]
    
    
    alt[, iso3 := fic]
    
    out <-
        alt[, list(date,
                   iso3,
                   zscorepd50 =`_MEDIAN_`,
                   zscorepd75 =`_Q3_`,
                   zscorepd90 =`_P90_`)][iso3!=""]
    
    return(out)
}

augmentBenchmarkDataset <-
    function(crisisdb = loadCrisisDB(),
             dtList =
                 list("alt" = getAltmanZscore())){

        dt <- prepareCrisisBenchmarkDataset(crisisdb = crisisdb)
        setkey(dt, iso3, date)

        if (length(dtList)>0){
            for (x in 1:length(dtList)){
                .d <- copy(dtList[[x]])
                setkey(.d, iso3, date)
                dt <- .d[dt, roll = TRUE]
            }
        }
        return(dt)
    }

getBISHousePrices <- function(){
    dt <- fread(input = "inst/extdata/BIS House Prices/BIS House Price DB.csv")
    s <- fread(input = "inst/extdata/BIS House Prices/BIS static selection.csv")

    setnames(dt, names(dt), stringr:::str_trim(names(dt)))
    dt[, date := as.Date(as.character(Period), format = "%d.%m.%Y")]
    dt[, Period := NULL]

    dt <- data.table:::melt.data.table(dt,
                                       id.vars = c("date"),
                                       variable.name = "ixname",
                                       value.name = "dtix"
                                       )

    s[, ixname := paste0("Q",Code)]

    dtsel <- dt[ixname %in% s$ixname]

    dtsel <-
        merge(dtsel,
              s[, list(ixname,
                       country = `Reference area`,
                       iso3 = ISO3)],
              by = "ixname")

    out <-
        dtsel[, list(
            iso3,
            date,
            houseix = as.numeric(dtix))][!is.na(houseix)]
    
    return(out)
}


getWB.WDI <- function(){
    wdi <- fread("./inst/extdata/World Bank/WDI World Development Indicators.csv",
                 header = TRUE
                 )

    vars.keep <- c("Country Code",
                   "Indicator Code",
                   grep("[[:digit:]]{4}", names(wdi), value = TRUE))

    wdi <-
        wdi[, vars.keep, with = FALSE]

    setnames(wdi, c("Country Code","Indicator Code"), c("iso3","variable"))

    wdi <- data.table:::melt.data.table(wdi,
                                        id.vars = c("iso3","variable"),
                                        variable.name = "year")

    wdi[, year := as.numeric(as.character(year))]
    wdi[, date := .toDate(paste0(year,"-01-01"))]

    wdi <- 
        data.table:::dcast.data.table(wdi,
                                      iso3 + date ~ variable,
                                      value.var = "value")
    return(wdi)
}

## wdi <- getWB.WDI()


getWB.GEM <- function(){
    wb.gem <- fread(input ="./inst/extdata/World Bank/Source_GlobalEconomicMonitor.csv",
                    header = TRUE)

    wb.gem[, date := {
        as.Date(paste0(as.numeric(date),"-12-01"), format = "%Y-%m-%d")
    }]

    wb.gem <-
        wb.gem[, list(iso2 = country.id,
                      date,
                      variable = indicator.id,
                      value)]

    wb.gem <- wb.gem[!is.na(value) & value != "" & iso2 != ""]

    wb.gem <- wb.gem[!is.na(date)]

    wb.gem[, value:= as.numeric(value)]
    wb.gem[, dup:=.N, by = list(iso2,date,variable)]

    wb.gem[dup>1][order(iso2,variable,date)]

    wb.gem <- 
        data.table:::dcast.data.table(wb.gem,
                                      iso2 + date ~ variable,
                                      value.var = "value")

    wb.gem <- 
        merge(wb.gem,
              isotran,
              by = "iso2")

    wb.gem[, iso2:=NULL]

    return(wb.gem)
}

getRR.debt <- function(){
    debt <- fread("./inst/extdata/Reinhart and Rogoff DB/Government Debt/Debt Variables.csv",
                  header = TRUE
                  )

    debt[, date := as.Date(paste0(year,"-01-01"), format = "%Y-%m-%d")]

    debtfinal <- debt[, .SD
                    , .SDcols = -"country"]
    
    return(debtfinal)
}

## rrdebt <- getRR.debt()


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

    return(o2)
}

## imf <- getIMFIFS()


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Join a list of data.tables with common keys 
##' @param dtList list of data.tables
##' @param id name of the cross-sectional unit column 
##' @param date name of the time-series unit column
##' @return merged data.table
##' @author Janko Cizel
joinDatasetList <- function(dtList,
                            id = 'iso3',
                            date = 'date'){

    ids = unique(as.character(unlist(lapply(dtList, function(x) unique(x[[id]])))))
    ids = ids[!is.na(ids) & ids!=""]
    dates = .fCrDates(begin="1960-01-01",end="2014-12-31", frequency=apply.yearly)[[2L]]

    out <- CJ(iso3 = ids,date = dates)

    for (x in 1:length(dtList)){
        cat(x,"\n")
        b <- copy(dtList[[x]])
        b[, date := as.Date(date)]

        uniquecols <- setdiff(names(b),names(out))
        b <- b[,c(id,date,uniquecols), with = FALSE]
        ## test <- copy(b)
        ## test[,dup := .N,by = c(id,date)]
        ## test[,table(dup)]
        ## test[dup>1]
        
        setkeyv(out, c(id, date))
        setkeyv(b, c(id, date))
        out <- unique(b)[out, roll = 365]

        newcols <- setdiff(intersect(names(out), names(b)),c(id,date))
        oldcols <- setdiff(names(out),newcols)

        setcolorder(out,c(oldcols,newcols))
    }
    
    return(out)
}


## o <- getECBListOfVariables()
## o[TITLE_COMPL %like% 'revenue']

## imflist <- getIMFListOfVariables(update = TRUE)
## imflist[label %like2% 'government']

