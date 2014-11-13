
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
                if (is.na(x[y-1])) out = c(out,NA)
                else if (x[y-1] == success) {out = c(out,NA)}
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
                                  timeCol = 'date'){
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

    crisis[, CRISIS := {
        x <- rep(NA,times = length(COUNTDOWN))
        x[COUNTDOWN %in% -3:0] <- 1
        x[COUNTDOWN %in% 1:5] <- 2
        x[is.na(x)] <- 0
        x
    }]

    print(data.frame(crisis[, c(idCol, timeCol, crisisCol, "PCN", "CN", "COUNTDOWN","CRISIS"),with = FALSE]),
          nrows = 100)

    return(crisis)
}
