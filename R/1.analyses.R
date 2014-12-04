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
            o <- list()
            for (x in benchVars) {
                d <- .SD[,c(x,xvar), with = FALSE]
                d <- d[complete.cases(d)]
                
                ## print(sprintf('iso:%s Y:%.0f x=%.2f y=%.2f',unique(iso3),year(.BY[[1]]),d$x,d$y))
                .c <- 
                    try(cor(d,
                            method = method,
                            use = 'complete.obs')[1,2])
                if (inherits(.c,'try-error')) {
                    .c <- NA
                    .n <- 0
                } else {
                    if (is.na(.c)) .n <- 0
                    else .n <- nrow(d)
                }
                o[[x]] <- sprintf("%+.3f [N=%4.0f]", .c, .n) 
            }
            
            o
        }
         , keyby = timeVar]
    
    return(out)
}



analyseCorrelationsByGroup <- function(
    data,
    xvar = 'zscorepd75',
    benchVars = c('ratingnum','spread','cds'),
    group = 'date',
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

                    sprintf("%+.3f [N=%4.0f]", .c, .n)
                }
            names(o) <- benchVars
            o
        }
         , keyby = group]
    
    return(out)
}   
