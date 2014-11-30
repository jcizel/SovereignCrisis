getECBListOfVariables <- function(){
    o <- fread(input = '../SDMXWrappers/inst/extdata/ECB-VariableList.csv')
    return(o)
}

queryECBVariableList <- function(pattern){
    v <- getECBListOfVariables()

    out <- v[(toupper(TITLE_COMPL) %like% toupper(pattern))][, list(ID,TITLE_COMPL,UNIT,UNIT_MULT,YEARMIN,YEARMAX,db)]
    
    return(out)
}

compressECBVariableList <- function(data,id.pos = 3){
    ## dt <- queryECBVariableList(pattern)   
    dt <- copy(data)
    
    dt[, ID.new  := {
        ID %>>%
        stringr:::str_split('\\.') %>>%
        (x ~ list.map(x, {
            s <- .
            s[id.pos] <- '*'
            paste(s,collapse = '.')
        })) %>>%
        unlist
    }]

    dt[, query  := {
        ID %>>%
        stringr:::str_split('\\.') %>>%
        (x ~ list.map(x, {
            s <- .
            s[id.pos] <- '[A-Z]+'
            paste(s,collapse = '.')
        })) %>>%
        unlist
    }]    

    dt[, extract := {
        ID %>>%
        stringr:::str_split('\\.') %>>%
        (x ~ list.map(x, .[id.pos])) %>>%
        unlist
    }]

    out <- 
        dt[,list(
            label = tail(TITLE_COMPL,1),
            countries = .condense(extract),
            numiso = length(unique(extract)),
            yearmin = min(YEARMIN),
            yearmax = max(YEARMAX),
            unit = .condense(UNIT),
            unit.mult = .condense(UNIT_MULT),
            db = .condense(db),
            query = unique(query)
        )
         , by = ID.new]
    return(out)
}

## pattern = queryECBVariableList('debt') %>>% compressECBVariableList %>>%
## (query)
## tsdata[, table(DATE)]
## pattern = queryECBVariableList('gross external debt') %>>%
## compressECBVariableList %>>%
## (.[numiso>10]) %>>%
## (query)
## out <- constructECBDataset(pattern = pattern, tsdata = tsdata)


constructECBDataset <- function(
    pattern,
    id.pos = 3,
    tsdata = fread(input = '../SDMXWrappers/inst/extdata/ECB-TS.csv',drop = "V1")
){
    o <- tsdata[, list(ID, DATE, VALUE)]
    s <- fread(input = '../SDMXWrappers/inst/extdata/ECB-VariableList.csv')

    result <- 
        foreach (y = pattern,
                 .errorhandling = 'stop') %do% {
                     cat(y,'\n')
                     
                     dt <- o[grepl(pattern = y,ID,perl = FALSE)]

                     dt[, ID.new  := {
                         ID %>>%
                         stringr:::str_split('\\.') %>>%
                         (x ~ list.map(x, {
                             s <- .
                             s[id.pos] <- '*'
                             paste(s,collapse = '.')
                         })) %>>%
                         unlist
                     }]

                     dt[, iso3 := {
                         ID %>>%
                         stringr:::str_split('\\.') %>>%
                         (x ~ list.map(x, .[id.pos])) %>>%
                         unlist %>>% .lookupISOCode
                     }]

                     out <- 
                         data.table:::dcast.data.table(dt,
                                                       DATE + iso3 ~ ID.new,
                                                       value.var = 'VALUE')

                     out[, date := {
                         
                         DATE %>>%
                         stringr:::str_split('Q') %>>%
                         (x ~ list.map(x, {
                             o <- as.numeric(.)
                             year = o[[1]]
                             qtr = o[[2]]

                             date <- 
                                 switch(qtr,
                                        paste0(year,'-03-15'),
                                        paste0(year,'-06-15'),
                                        paste0(year,'-09-15'),
                                        paste0(year,'-12-15'))
                             date
                         })) %>>% unlist %>>% as.Date
                     }]

                     out[, DATE := NULL]
                     
                     out <-
                         structure(out,
                                   lookup = s[grepl(pattern = y,ID,perl = FALSE)] %>>% compressECBVariableList)

                     return(out)
                 }

    ## Merge based on iso3 and date (make sure both are present in all datasets)

    ids = unique(as.character(unlist(lapply(result, function(x) unique(x$iso3)))))
    ids = ids[!is.na(ids) & ids!=""]
    dates = .fCrDates(begin="1960-01-01",end="2014-07-31", frequency=apply.yearly)[[2L]]

    out <- CJ(iso3 = ids,date = dates)

    for (x in 1:length(result)){
        cat(x,"\n")
        b <- copy(result[[x]])
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
    
    lookup <- rbindlist(lapply(result,function(dt) attributes(dt)$lookup))

    out <-
        structure(out,
                  lookup = lookup)
    
    return(out)
}

## tsdata = fread(input = '../SDMXWrappers/inst/extdata/ECB-TS.csv',drop = "V1")
## ecb = constructECBDataset(tsdata = tsdata)


## queryECBVariableList('debt')
## compressECBVariableList('debt')[numiso > 10]
## compressECBVariableList('gross external debt')[numiso > 10]
## compressECBVariableList('interbank')[numiso > 10]
## compressECBVariableList('yield')[numiso > 10]

getIMFListOfVariables <- function(update = FALSE){
    .files <- list.files('./inst/extdata')
    if (('IMFListOfVariables.csv' %in% .files) && (update==FALSE)){
        out <- fread(input = './inst/extdata/IMFListOfVariables.csv')
    } else {
        lookup <- xlsx:::read.xlsx2(file = './inst/extdata/IMF/IFS/IMF-IFS_variables.xlsx',
                                    sheetIndex = 1 )
        .o1 <- 
            data.table(lookup)[, list(name = Variable,
                                      label = paste0(Table,": ", Concept,", ", scale_desc))]

        imfifs <- fread(input = "./inst/extdata/IMF/IFS/IMF-IFS_annual.csv",
                        header = TRUE
                        )
        
        .o2 <- summarizeDataAvailability(dt = getIMFIFS())
        
        .o3 <- imfifs[!is.na(iso3) & !is.na(value), {
            list(
                concept_desc = unique(concept_desc),
                scale_desc = paste(unique(scale_desc), collapse = "; "),
                unit_desc = paste(unique(unit_desc), collapse = "; "),
                frequency_desc = paste(unique(frequency_desc), collapse = "; ")                
            )
        }
                      , by = 'varcode']

        ## JOIN
        setkey(.o1, name)
        setkey(.o2, variable)
        setkey(.o3, varcode)

        out <- .o1[.o2]
        out <- .o3[out]
        
        write.csv(x = out,
                  file = './inst/extdata/IMFListOfVariables.csv')
    }

    return(out[,list(name = variable,
                     label = label)])
}

queryIMFVariableList <- function(pattern = ''){
    .files <- list.files("./inst/extdata")
    if ("IMFListOfVariables.csv" %in% .files) {
        v <- fread(input = "./inst/extdata/IMFListOfVariables.csv")
    }
    else {
        v <- getIMFListOfVariables()
    }
    out <- v[(toupper(label) %like% toupper(pattern))][, list(varcode, scale_desc, unit_desc, label, yearmin, yearmax, numiso, numobs)]
    return(out)
}

## queryIMFVariableList('yield')
## queryIMFVariableList('credit')
## queryIMFVariableList('arrear')

## getWorldBankListOfVariables <- WorldBankAPI::getWorldBankListOfVariables
## queryWorldBankVariableList <- WorldBankAPI::queryWorldBankVariableList
