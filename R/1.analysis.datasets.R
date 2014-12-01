## -------------------------------------------------------------------------- ##
## CHOOSE VARIABLES                                                           ##
## -------------------------------------------------------------------------- ##

queryMacroDatasets <- function(pattern,
                               source = NULL){

    
    if (is.null(source)){
        r <- list()
        r[['imf']] <- try({
            queryIMFVariableList(pattern)[, list(varcode,
                                                 label,
                                                 numiso,
                                                 yearmin,
                                                 yearmax,
                                                 source = 'imf')]})
        
        r[['wb']] <- try({
            WorldBankAPI::queryWorldBankVariableList(pattern) %>>%
            (df ~ df[, list(varcode = id,
                            label = name,
                            source = 'wb')])})

        r[['ecb']] <- try({
            queryECBVariableList(pattern) %>>% compressECBVariableList %>>%
            (df ~ df[, list(varcode = query,
                            label = label,
                            numiso = numiso,
                            yearmin = yearmin,
                            yearmax = yearmax,
                            source = 'ecb')])})

        r <- Filter(function(x) !inherits(x,'try-error'), r)
        out <- rbindlist(r, fill = TRUE)

        out %>>% unique %>>% write.csv(sprintf('./inst/extdata/queries/%s.csv',pattern))
        
    } else {
        ## TODO
        NULL
    }

    return(out)
}


createListOfSelectedVariables <- function(dir.path = './inst/extdata/queries/'){
    .files <- list.files(dir.path,
                         pattern = "^[^~].+xlsx$")

    codes <- 
        foreach (x = .files,
                 .errorhandling = 'remove') %do% {
            read.xlsx2(file = paste0(dir.path,x),
                       sheetIndex = 1) %>>%
            data.table %>>%
            (df ~ df[select == 1, list(varcode, label, source)])
        } %>>%
    rbindlist %>>%
    split(f = .$source)
    
    return(codes)
}

## vars <- createListOfSelectedVariables()

createQueriedMacroDataset <- function(){
    lookup <- createListOfSelectedVariables()

    vars <- lookup %>>%
    list.map(unique(as.character(varcode)))

    result <- list()
    
    if (length(vars$imf) > 0){
        imf <- getIMFIFS()[,c('iso3','date',vars$imf), with = FALSE] 
        
        imf[,is.na(.SD),.SDcols = vars$imf] %>>%
        apply(1, all) %>>%
        (~ drop.rows.imf) %>>% invisible

        result[['imf']] <- imf[!drop.rows.imf]
    }

    if (length(vars$wb) > 0){
        .wb <- WorldBankAPI::getWorldBankDataSeries(indicators = vars$wb)
        wb <- WorldBankAPI::createWorldBankDataset(.wb)

        cols <- names(wb)[names(wb) %in% vars$wb]

        wb[,is.na(.SD),.SDcols = cols] %>>%
        apply(1, all) %>>%
        (~ drop.rows.wb) %>>% invisible
        
        wb[, iso3 := .lookupISOCode(country.id)]
        result[['wb']] <- wb[!drop.rows.wb][!is.na(iso3)][,c('iso3','date',cols),with = FALSE]
    }

    if (length(vars$ecb) > 0){
        result[['ecb']] <- constructECBDataset(pattern = vars$ecb)
    }
    
    ## MERGE
    ids = unique(as.character(unlist(lapply(result, function(x) unique(x$iso3)))))
    ids = ids[!is.na(ids) & ids!=""]
    dates = .fCrDates(begin="1960-01-01",end="2015-01-01", frequency=apply.yearly)[[2L]]

    out <- CJ(iso3 = ids,date = dates)

    for (x in 1:length(result)){
        cat(x,"\n")
        b <- copy(result[[x]])
        b[, date := as.Date(date)]

        uniquecols <- setdiff(names(b),names(out))
        b <- b[,c("iso3","date",uniquecols), with = FALSE]

        setkey(out, iso3, date)
        setkey(b, iso3, date)
        b <- unique(b)
        out <- b[out, roll = 365]

        newcols <- setdiff(intersect(names(out), names(b)),c("iso3","date"))
        oldcols <- setdiff(names(out),newcols)
        setcolorder(out,c(oldcols,newcols))
    }

    lookupFinal <- 
        merge.data.frame(x = lookup %>>% rbindlist %>>% (? str(.)),
                         y = out %>>% summarizeDataAvailability %>>% (? str(.)),
                         by.x = 'varcode',
                         by.y = 'variable',
                         sort = FALSE) %>>% data.table %>>% (? str(.))
    
    lookupFinal %>>% unique %>>%
    write.csv('./inst/extdata/queries/Queried-Macro-Dataset-Lookup.csv')

    out <-
        structure(out,
                  lookup = lookupFinal)
    
    return(out)
}



