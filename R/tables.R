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


tabulateCorrelations <- function(
    data =
        augmentBenchmarkDataset(crisisdb = alternativeCrisisDB(),
                                dtList = list(getAltmanZscore(),
                                              getAggregatedBankscopePDs())),
    var = c('zscorepd75','SC_CLOSURE_ALL.Q3.','SC_OBR_EU.Q3.'),
    method = 'pearson',
    dif = FALSE,
    lag = -1,
    by = 'iso3',
    outfile = './inst/RESULTS/tabulateCorrelations.tex'
){
    if (!inherits(data, 'data.table')) stop('Data must be a data.table.')
    
    if (dif == TRUE){
        dt <- 
            GeneralUtilities:::shiftData(
                data = data,
                var = var,
                replace = TRUE,
                by = by,
                lag = lag,
                dif = TRUE
            )
    } else {
        dt <- copy(data)
    }
    
    output.list <-
        foreach (x = var,
                 .errorhandling = 'remove') %do% {
            analyseCorrelationsOverTime(data = dt, xvar = x, method = method)
        }    

    t <-
        Reduce(function(...) merge(..., by = 'date', all = TRUE), output.list)

    t <- t[date > '2004-01-01']
    t[, date := year(date)]

    out <- LaTeXTableGems:::dataTableToInnerLatex(t,
                                                  outfile = outfile)

    return(output.list)
}

## debug(LaTeXTableGems:::createLatexTableHeader)
## LaTeXTableGems:::createLatexTableHeader(outfile = './inst/RESULTS/tabulateCorrelations-head.tex')
## t1 <- tabulateCorrelations(outfile = './inst/RESULTS/tabulateCorrelations.tex')
## t2 <- tabulateCorrelations(dif = TRUE,
##                            lag = -1,
##                            outfile = './inst/RESULTS/tabulateCorrelations-dif.tex')

## t1 <- tabulateCorrelations(method = 'spearman',
##                            outfile = './inst/RESULTS/tabulateCorrelations-spearman.tex')
## t2 <- tabulateCorrelations(method = 'spearman',
##                            dif = TRUE,
##                            lag = -1,
##                            outfile = './inst/RESULTS/tabulateCorrelations-spearman-dif.tex')
## 
