getAltmanZscore <- function(){

    alt <- fread(input = './inst/extdata/Altman PDs/ZSCORE_AGGREGATED.csv')


    alt[, date := {
        d <- DATE
        year = as.numeric(sapply(str_split(d, ":"), function(x) x[[1]]))
        quarter = as.numeric(sapply(str_split(d, ":"), function(x) x[[2]]))
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
            for (x in names(dtList)){
                .d <- copy(dtList[[x]])
                setkey(.d, iso3, date)
                dt <- .d[dt, roll = TRUE]
            }
        }
        return(dt)
    }
