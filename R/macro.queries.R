getECBListOfVariables <- function(){
    o <- fread(input = '../SDMXWrappers/inst/extdata/ECB-VariableList.csv')
    return(o)
}

queryECBVariableList <- function(pattern){
    v <- getECBListOfVariables()

    out <- v[(toupper(TITLE_COMPL) %like% toupper(pattern))][, list(ID,TITLE_COMPL,UNIT,UNIT_MULT,YEARMIN,YEARMAX,db)]
    
    return(out)
}

compressECBVariableList <- function(pattern,id.pos = 3){
    dt <- queryECBVariableList(pattern)

    dt[, ID.new := {
        ID %>>%
        stringr:::str_split('\\.') %>>%
        (x ~ list.map(x, paste0(.[-id.pos],collapse = '.'))) %>>%
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
            db = .condense(db)
        )
         , by = ID.new]
    return(out)
}

## queryECBVariableList('debt')
## compressECBVariableList('debt')[numiso > 10]
## compressECBVariableList('gross external debt')[numiso > 10]
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

getWorldBankListOfVariables <- WorldBankAPI::getWorldBankListOfVariables
queryWorldBankVariableList <- WorldBankAPI::queryWorldBankVariableList
