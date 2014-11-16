##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Produce a Summary Table of Sovereign Crises 
##' @param crisisDB 
##' @param crisisTypes 
##' @param timeCol 
##' @param min.time 
##' @param idCol 
##' @return Latex table (without table environment)
##' @author Janko Cizel
tabulateCrises <- function(crisisDT,
                           crisisTypes = "Foreign Sov Debt",
                           timeCol = 'YEAR',
                           min.time = 1995,
                           idCol = 'iso3',
                           outfile){

    crisis_summary <- list()
    for (x in crisisTypes){
        crisis_summary[[x]] <- {
            crisis <- copy(crisisDT)

            crisis.2 <- 
                createCrisisVariables(
                    crisisDT = crisis,
                    crisisCol = x,
                    idCol = idCol,
                    timeCol = timeCol
                )


            o <- 
                crisis.2[get(x) != 0 & get(timeCol) > min.time
                       , list(period = {
                           o <- get(timeCol)
                           miny = min(o)
                           maxy = max(o)
                           if (miny == maxy)
                               paste0(miny)
                           else
                               paste0(min(o),"-",max(o))
                       })
                       , by = c(idCol, "CN")][
                           , list(periods = paste0(period, collapse = "; "))
                           , by = idCol][order(get(idCol))]
            
            setnames(o, "periods", x)

            crisis_summary[[x]] <- o
        }
    }
    

    crisis_summary.table <-
        Reduce(function(...) merge(..., by = idCol, all = TRUE), crisis_summary)


    out <- LaTeXTableGems:::dataTableToInnerLatex(crisis_summary.table,
                                                  outfile = outfile)
    

    return(out)
}


tabulateDataAvailability <- function(dt,
                                     outfile,
                                     selCols = names(dt),
                                     lookup.table = NULL,
                                     lookup.table.id = 'name',
                                     lookup.table.label = 'label'){

    require(LaTeXTableGems)
    o <- summarizeDataAvailability(dt)

    if (!is.null(lookup.table)){
        o[, label :=
              .lookupVariable(
                  variable,
                  lookup.table = lookup.table,
                  varNameCol = lookup.table.id,
                  return.cols = lookup.table.label
              )]
        o <- o[!is.na(label)][, list(variable,label,Availability)][, selCols, with = FALSE]
    }
    
    LaTeXTableGems:::dataTableToInnerLatex(dt = o[, selCols, with = FALSE],
                                           outfile = outfile)

    return(o)
}


## lookup.table <- rbindlist(
##     list(
##         data.table(name = 'ratingnum',
##                    label = 'S&P LT Foreign Issuer Sovereign Rating'),
##         data.table(name = 'cds',
##                    label = '5-Year Sovereign CDS Spread (Source: Bloomberg)'),
##         data.table(name = 'spread',
##                    label = 'Treasury Bond Spread above U.S. Treasury Yield (in b.p)')
##     )
## )


## bench <- getSovBenchmarks()
## r <- 
##     tabulateDataAvailability(dt = bench,
##                              outfile = "./inst/RESULTS/availability.tex",
##                              lookup.table = lookup.table,
##                              selCols = c("label", "Availability"))
