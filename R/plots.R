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




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Line plots of sovereign credit ratings, CDS spreads, and government
##' bond spreads 
##' @param isoSel ISO3 country code of a country for which the plot is to be created. 
##' @param limits a Date vector indicating the time range of the plot
##' @param filename path of the resulting plot (in pdf format)
##' @return NULL
##' @author Janko Cizel
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

    return(NULL)
}

## plotSovBenchmarks(isoSel = "ARG",
##                   filename = './inst/RESULTS/plotSovereignBenchmarkIndicators.pdf')

## plotSovBenchmarks(isoSel = "ARG")
## plotSovBenchmarks(isoSel = "BRA")
## plotSovBenchmarks(isoSel = "ESP")
## plotSovBenchmarks(isoSel = "GRC")
## plotSovBenchmarks(isoSel = "PRT")


.densityPlot <- function(data,
                         x = 'ratingnum',
                         xlabel = "",
                         ylabel = "",
                         grouplabel = "Crisis Indicator"){
    require(ggthemes)    
    o <- ggplot(data = data)  
    o <- o + geom_density(aes_string(x = x,
                                     linetype = "as.factor(CRISIS)"
                                     )) 
    o <- o + scale_linetype_stata() 
    o <- o + theme_bw()             
    o <- o + guides(size = FALSE) 
    o <- o + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   axis.text.y = element_text(size = 10))
    o <- o + labs(x = xlabel,
                  y = ylabel,
                  linetype = grouplabel)
    ## o <- o + theme(legend.position="none")
    return(o)
}

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
.grabLegend <- function(plot){
    tmp <- ggplot_gtable(ggplot_build(plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Density plots of benchmark indicators for different crisis groups 
##' @param crisisType Type of crisis used in grouping observations
##' @param filename path to the resulting pdf file with graph output
##' @param adjust Adjust for time trends?
##' @return NULL
##' @author Janko Cizel
plotDensityAroundCrisisEvents <- function(crisisType = "Sovereign Debt Crisis",
                                          filename = '~/Downloads/test.pdf',
                                          adjust = FALSE,
                                          plotDefinition =
                                          list('ratingnum' =
                                               list(x = 'ratingnum',
                                                    xlabel = 'S&P Sovereign Credit Rating'),
                                               'cds' =
                                               list(x = 'cds',
                                                    xlabel = '5-Year Sovereign CDS Spread'),
                                               'spread' =
                                               list(x = 'spread',
                                                    xlabel = 'Sovereign Bond Yield Spread'))){
    dt <- prepareCrisisBenchmarkDataset()

    dt1 <- 
        createCrisisVariables(crisisDT = dt,
                              crisisCol = crisisType,
                              idCol = "iso3",
                              timeCol = "year")

    if (adjust == TRUE){
        cols <- c('ratingnum',
                  'cds',
                  'spread')
        dt1[, paste(cols) := lapply(.SD, function(x){
            o <- (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
            return(o)
        }), by = year, .SDcols = cols]
    }
    
    plot_list <- list()

    for (x in names(plotDefinition)){
        plot_list[[x]] <- local({
            .t <- plotDefinition[[x]]
            o <- .densityPlot(data = dt1,
                              x = .t$x,
                              xlabel = .t$label)
            ## o <- o + scale_x_continuous(limits = c(0,10000))
            o
        })
    }
    
    plot_list <- Filter(function(x) !"try-error" %in% class(x), plot_list)

    ## l <- .grabLegend(plot_list[[1]])

    g <- 
        do.call("arrangeGrob",
                c(plot_list,
                  list(
                      ## legend = legend,
                      as.table = FALSE,
                      ncol = 1
                      )
                  ))

    ggsave(filename = filename,
           width = 210,
           height = 210,
           units = "mm",
           plot = g)
    
    return(NULL)
}

## plotDensityAroundCrisisEvents(crisisType = "Sovereign Debt Crisis",
##                               filename = './inst/RESULTS/plotSovereignBenchmarkDens.pdf',
##                               adjust = TRUE)

## plotDensityAroundCrisisEvents(crisisType = "Sovereign Debt Crisis",
##                               adjust = TRUE)
