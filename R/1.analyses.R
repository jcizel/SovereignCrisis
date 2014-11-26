analyseCorrelationsOverTime <- function(
    data,
    xvar = 'zscorepd75',
    benchVars = c('ratingnum','spread','cds'),
    timeVar = 'date',
    method = 'pearson'
)
{
    out <- 
        data[, {
            o <- 
                foreach(x = benchVars) %do% {
                    .c <- 
                        cor(get(x), get(xvar),
                            method = method,
                            use = 'pairwise.complete.obs')
                    
                    .n <- sum(complete.cases(get(x), get(xvar)))

                    paste0(round(.c,3)," [N=",.n,"]")
                }
            names(o) <- vars
            o
        }
         , keyby = timeVar]
    
    return(out)
}   
