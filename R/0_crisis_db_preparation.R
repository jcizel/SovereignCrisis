
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Crisis Event Counter 
##' @param x 
##' @param success 
##' @param failure 
##' @return A vector with event enumeration.
##' @author Janko Cizel
eventCounter <- function(x,
                         success = 0,
                         failure = 1){

    ## cat("###  Beginning of the failure counting.\n")
    ## cat("###  Success indicator:", success, "\n")
    ## cat("###  Failure indicator:", failure, "\n")    

    i = 1
    out <- c()
    for (y in 1:length(x)){
        ## cat("Iteration: ",y,". ")
        ## cat("Value of x: ",x[y],".")

        if (y == 1){
            if (is.na(x[y])) out = c(out,NA)
            else if (x[y] == failure) out = c(out,i)
            else if (x[y] == success) out = c(out,0)
        } else {
            if (is.na(x[y])){
                if (is.na(x[y-1])) out = c(out,0)
                else if (x[y-1] == success) {out = c(out,0)}
                else if (x[y-1] == failure) {out = c(out,i);}
            }
            else if (x[y] == success){
                if (is.na(x[y-1])) {out = c(out,0);}
                else if (x[y-1] == success) {out = c(out,0);}
                else if (x[y-1] == failure) {out = c(out,0);i=i+1;}
            } else if (x[y] == failure) {
                out = c(out,i)
            }            
        }
        ## cat("Counter: ",tail(out,1),"\n")
    }
    ## cat("###  END OF COUNTING  ###.\n")
    return(out)    
}    
 


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create Crisis Variables 
##' @param crisisDT 
##' @param crisisCol 
##' @param idCol 
##' @param timeCol 
##' @return Original data.table with included countdown variables.
##' @author Janko Cizel
createCrisisVariables <- function(crisisDT,
                                  crisisCol = "Banking Crisis",
                                  idCol = 'iso3',
                                  timeCol = 'date',
                                  groups =
                                  list("[-4:-1]"=expression(COUNTDOWN %between% c(-4,-1)),
                                       "[0]"=expression(COUNTDOWN == 0),
                                       "[1:4]"=expression(COUNTDOWN %between% c(1,4)))){
    crisis <- copy(crisisDT)
    crisis[, c('PCN','CN') := {
        ## cat("Country: ",.BY[[1]],"\n")
        list(eventCounter(get(crisisCol),success = 1,failure = 0),
             eventCounter(get(crisisCol),success = 0,failure = 1))
    }
         , by = idCol ]

    crisis <- crisis[order(get(idCol),-get(timeCol))]
    crisis[PCN > 0, COUNTDOWN := -(0:(.N-1))
         , by = c(idCol,'PCN')]
    
    crisis <- crisis[order(get(idCol),get(timeCol))]
    crisis[CN > 0, COUNTDOWN := (1:(.N))
         , by = c(idCol,'CN')]

    crisis[,
           COUNTDOWN := {
               COND <- (PCN == max(PCN)) & (max(PCN) > max(CN))
               COUNTDOWN[COND] <- NA
               COUNTDOWN
           },
           by = get(idCol)]

    ## crisis[, CRISIS := {
    ##     x <- rep(NA,times = length(COUNTDOWN))
    ##     x[COUNTDOWN %in% -3:0] <- 1
    ##     x[COUNTDOWN %in% 1:5] <- 2
    ##     x[is.na(x)] <- 0
    ##     x
    ## }]

    crisis[['CRISIS']] <- rep("CONTROL",times = length(crisis$COUNTDOWN))
    for (x in names(groups)){
        crisis[eval(groups[[x]]), CRISIS:= paste0(x)]
    }

    ## print(data.frame(crisis[, c(idCol, timeCol, crisisCol, "PCN", "CN", "COUNTDOWN","CRISIS"),with = FALSE]),
    ##       nrows = 100)

    return(crisis)
}

##' This function returns a dataset of Reinhardt and Rogoff, which dates 6
##' different types of crises for a set of 70 countries. 
##'
##' TODO: RR's database ends in 2010. Update the DB by including the sovereign
##' debt crisis episodes during 2010-2014.
##
##' @title Load Reinhardt and Rogoff Sovereign Crisis Database
##' @return data.table with crisis dataset. 
##' @author Janko Cizel
loadCrisisDB <- function(){
    out <- fread(input = './inst/extdata/SOVEREIGN/RR-crisis.dataset.csv',
                 header = TRUE)
    out[, date := as.Date(paste0(Year,"-12-30"))]

    setnames(out, "ISO3", "iso3")

    out[, debtcrisis := (`Foreign Sov Debt`*1 + `Domestic Sov Debt`*1)>0]
    
    return(out[,c("Country", "iso3", "date", "Independence", "Currency Crisis", 
                  "Inflation Crisis", "Stock Market Crash", "Domestic Sov Debt", 
                  "Foreign Sov Debt", "Banking Crisis", "Crisis Tally", "debtcrisis"),
               with = FALSE])
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create additional credit event indicators 
##' @return data.table with updated sovereign crisis indicators 
##' @author Janko Cizel
alternativeCrisisDB <- function(){
    ## TO BE COMPLETED
    require("GeneralUtilities")
    require(xts)
    
    ratings <- getSPRatings()
    ids = unique(ratings$iso3)
    ids = ids[!is.na(ids) & ids!=""]
    dates = .fCrDates(begin="1960-01-01",end="2012-12-31", frequency=apply.yearly)[[2L]] + 1

    o <- CJ(iso3 = ids,date = dates)
    setkey(o, iso3, date)
    setkey(ratings, iso3, date)
    out <- ratings[o, roll = 365]

    modCols <- c('rating','ratingnum')
    out[
      , paste(modCols) := lapply(.SD, na.locf, na.rm = FALSE)
      , by = "iso3"
      , .SDcols = modCols]

    out <- out[!is.na(rating)]

    out[, ratingdif1y := GeneralUtilities:::shift(ratingnum, lag = -2, dif = TRUE), by = 'iso3']

    ## out[, quantile(ratingdif1y,
    ##                probs = seq(0,1,0.01),
    ##                na.rm = TRUE)]

    ## data.frame(out[ratingdif1y <= -2])

    out[, ratingdrop := (ratingdif1y <= -2)*1]

    out <- 
        out[, list(iso3,
                   date,
                   ratingdif1y,
                   ratingdrop)]
    return(out)
}
