.tsLinePlot <- function(data,
                        x,
                        y,
                        limits = as.Date(c('1990-01-01','2013-01-01')),
                        format = "%Y",
                        xlabel = "",
                        ylabel = ""){
    require(scales)
    o <- ggplot(data = data)
    o <- o + geom_step(aes_string(x = x,
                                  y = y))
    o <- o + theme_bw()
    o <- o + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   axis.text.y = element_text(size = 10))
    o <- o +  scale_x_date(labels = date_format(format = format),
                           limits = limits)
    o <- o + labs(x = xlabel,
                  y = ylabel)
    return(o)
}

.crisisRegion <- function(plot,
                          crisisdata){
    if (nrow(crisisdata)==0){
        o <- plot
    } else {
        o <- plot + geom_rect(data = crisisdata,
                              aes(xmin = mind.event,
                                  xmax = maxd.event,
                                  ymin=-Inf, ymax=Inf,
                                  fill = t),
                              fill="red",
                              alpha = 0.2,
                              inherit.aes = FALSE)
    }
    return(o)
}

## x = "ARG"
## o <- .tsLinePlot(data = getSPRatings()[iso3==x],
##                  x='date',
##                  y='ratingnum',
##                  limits = as.Date(c('1990-01-01','2013-01-01'))
##                  ) + .crisisRegion(data = crises2[iso3==x])

## filename = '~/Downloads/test.pdf'
## ggsave(plot = o,
##        filename = filename)





plotSovBenchmarks <- function(isoSel = "ARG",
                              limits = as.Date(c('1990-01-01','2013-01-01')),
                              filename = '~/Downloads/test.pdf')
{
    x <- isoSel
    
    crises = loadCrisisDB()
    crises[, debtcrisis := (`Foreign Sov Debt`*1 + `Domestic Sov Debt`*1)>0]
    crises <- 
        createCrisisVariables(
            crisisDT = crises,
            crisisCol = 'debtcrisis',
            idCol = 'iso3',
            timeCol = 'date'
        )
    
    crises2 <- 
        crises[CN>0, list(mind.event = min(get(timeCol)),
                          maxd.event = max(get(timeCol)))
             , by = c('iso3', 'CN')]

    plot_list <- list()
    plot_list[['rating']] <- try({
        d = getSPRatings()[iso3==x]
        if (nrow(d)==0) stop("No data available")
        o <- .tsLinePlot(data = d,
                         x='date',
                         y='ratingnum',
                         limits = limits,
                         ylabel = "S&P Sovereign Rating"
                         ) 
        .crisisRegion(plot = o,
                      crisisdata = crises2[iso3==x])
    })
    
    plot_list[['cds']] <- try({
        d <- getBloombergSovCDS()[iso3==x]
        if (nrow(d)==0) stop("No data available")        
        o <- .tsLinePlot(data = d,
                         x='date',
                         y='cds',
                         limits = limits,                    
                         ylabel = "5-Year Sovereign CDS Spread"
                         ) 
        .crisisRegion(plot = o,
                      crisisdata = crises2[iso3==x])
    })

    plot_list[['spread']] <- try({
        d <- getSovBondSpreads()[iso3==x]
        if (nrow(d)==0) stop("No data available")        
        o <- .tsLinePlot(data = d,
                         x='date',
                         y='spread',
                         limits = limits,                    
                         ylabel = "Sovereign Bond Yield Spread"
                         )
        .crisisRegion(plot = o,
                      crisisdata = crises2[iso3==x])        
    })    

    plot_list <- Filter(function(x) !"try-error" %in% class(x), plot_list)
    
    g <- 
        do.call("arrangeGrob",
                c(plot_list,
                  list(
                      as.table = FALSE,
                      ncol = 1,
                      ## widths = 20,
                      ## heights = 5,
                      default.units = "cm"
                      ## main =
                      ## textGrob(.BY,
                      ##          gp=gpar(cex=1),
                      ##          just="top")
                      )
                  ))

    ggsave(filename = filename,
           width = 310,
           height = 210,
           units = "mm",
           plot = g)           
}

## plotSovBenchmarks(isoSel = "ARG")
## plotSovBenchmarks(isoSel = "BRA")
## plotSovBenchmarks(isoSel = "ESP")
## plotSovBenchmarks(isoSel = "GRC")
## plotSovBenchmarks(isoSel = "PRT")


