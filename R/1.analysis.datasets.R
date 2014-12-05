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

        ## r[['ecb']] <- try({
        ##     queryECBVariableList(pattern) %>>% compressECBVariableList %>>%
        ##     (df ~ df[, list(varcode = query,
        ##                     label = label,
        ##                     numiso = numiso,
        ##                     yearmin = yearmin,
        ##                     yearmax = yearmax,
        ##                     source = 'ecb')])})

        r[['OECD']] <- try({
            oecd <- fread('../SDMXWrappers/inst/extdata/OECD-VariableList.csv')
            oecd %>>%
            (df ~ df[, list(varcode = VARNAME,
                            label = LABEL,
                            source = 'oecd')])})
        
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
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param test 
##' @param checkCache 
##' @return 
##' @author Janko Cizel
createQueriedMacroDataset <- function(
    test = FALSE,
    checkCache = TRUE
){
    lookup <- createListOfSelectedVariables()

    if (test == FALSE){
        vars <- lookup %>>%
        list.map(unique(as.character(varcode)))
    } else {
        vars = lookup %>>%
        list.map(tail(unique(as.character(varcode)), 5))
    }

    
    result <- list()
    
    if (length(vars$imf) > 0){
        imf <- getIMFIFS()[,c('iso3','date',vars$imf), with = FALSE] 
        
        imf[,is.na(.SD),.SDcols = vars$imf] %>>%
        apply(1, all) %>>%
        (~ drop.rows.imf) %>>% invisible

        result[['imf']] <- imf[!drop.rows.imf]
    }

    if (length(vars$wb) > 0){
        if (checkCache == TRUE){
            .wb.cache <- WorldBankAPI::checkWorldBankCache(indicators = vars$wb)
            
            present <- .wb.cache$indicator.id %>>% unique
            remaining <- setdiff(vars$wb, present)
            
            .wb.remain <- WorldBankAPI::getWorldBankDataSeries(indicators = remaining)

            .wb <- rbindlist(list(.wb.cache,.wb.remain), fill = TRUE)
            .wb <- unique(.wb[month(date) == 12][!is.na(country.id) | !country.id %in% c("","0","NA")])
        } else {
            .wb <- WorldBankAPI::getWorldBankDataSeries(indicators = vars$wb)
            .wb <- unique(.wb[month(date) == 12])        
        }
        browser()
        test <- copy(.wb)
        test[,dup := .N,by = c('indicator.id','country.id','date')]
        test[,table(dup)]
        test[dup>1]
        .wb <- .wb[test$dup == 1]
        
        wb <- WorldBankAPI::createWorldBankDataset(.wb[,list(indicator.id,indicator.value,country.id,date,value)])
        ## test <- copy(wb)
        ## test[,dup := .N,by = c('country.id','date')]
        ## test[,table(dup)]
        ## test[dup>1][country.id == '1A']

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

        ## test <- copy(b)
        ## test[,dup := .N,by = c('iso3','date')]
        ## test[,table(dup)]
        ## test[dup>1]
        
        uniquecols <- setdiff(names(b),names(out))
        b <- b[,c("iso3","date",uniquecols), with = FALSE]

        setkey(out, iso3, date)
        setkey(b, iso3, date)
        
        out <- b[out, roll = 365]

        newcols <- setdiff(intersect(names(out), names(b)),c("iso3","date"))
        oldcols <- setdiff(names(out),newcols)
        setcolorder(out,c(oldcols,newcols))
    }

    lookupFinal <- 
        merge.data.frame(x = lookup %>>% rbindlist %>>% (? str(.)) %>>% data.frame,
                         y = out %>>% summarizeDataAvailability %>>% (? str(.)) %>>% data.frame,
                         by.x = 'varcode',
                         by.y = 'variable',
                         sort = FALSE) %>>% data.table %>>% (? str(.))
    
    lookupFinal %>>% unique %>>%
    write.csv('./inst/extdata/queries/Queried-Macro-Dataset-Lookup.csv')

    out <-
        structure(out,
                  lookup = lookupFinal %>>% data.table)
    
    return(out)
}



