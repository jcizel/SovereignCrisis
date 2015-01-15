shinyServer(function(input,output,session){
    set.seed(122)

    output[['lookupTable']] <-
        renderDataTable(
            lookup[,
                   list(
                       Name = varcode,
                       Source = source,
                       `Beginning` = yearmin,
                       `End` = yearmax,
                       `Number of Countries` = numiso,
                       Label = label
                   )]
        )

    output[['crisisSummary']] <-
        renderDataTable(
            crisisSummary$data
        )

    output[['benchAvailability']] <-
        renderDataTable(
            bench.availability
        )

    output[['countryTable']] <-
        renderDataTable(
            countries[,
                      list(
                          `ISO3` = id,
                          `Name` = name,
                          `Capital` = capitalCity,
                          `Region` = region.value,
                          `Administration Region` = adminregion.value,
                          `Income Level` = incomeLevel.value,
                          `Lending Type` = lendingType.value
                      )]
        )
    

    output[['plot3']] <- renderDygraph({
        if (input$benchdataSel == 1){
            dygraph(ratm[sprintf("%s/%s",
                                 input$dates[1],
                                 input$dates[2]),
                         c(input$ctrySel)]) %>>%
            dyHighlight(highlightCircleSize = 5,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>>%
            dyRangeSelector()
        } else if (input$benchdataSel == 2){
            dygraph(cdsm[sprintf("%s/%s",
                                 input$dates[1],
                                 input$dates[2]),
                         c(input$ctrySel)]) %>>%
            dyHighlight(highlightCircleSize = 5,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>>%
            dyRangeSelector()
        } else {
            dygraph(spreadsm[sprintf("%s/%s",
                                     input$dates[1],
                                     input$dates[2]),
                             c(input$ctrySel)]) %>>%
            dyHighlight(highlightCircleSize = 5,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>>%
            dyRangeSelector()            
        }
    })

    output[['plot4']] <- rCharts::renderChart2({
        bench[,tooltip := sprintf("Country: %s <br/> Region: %s <br/> Income: %s <br/> Date: %s",
                           iso3,
                           region.value,
                           incomeLevel.value,                           
                           date)]
        if (input$`plot4-group` != "none"){
            highchartScatter2d(
                data = bench,
                x = "cds",
                y = "ratingnum",
                group = input$`plot4-group`,
                tooltipVar = 'tooltip'
            ) -> p
        } else {
            highchartScatter2d(
                data = bench,
                x = "cds",
                y = "ratingnum",
                tooltipVar = 'tooltip'
            ) -> p
        }
        return(p)
    })
    
    output[['plot5']] <- rCharts::renderChart2({
        bench[, iso3f := as.factor(iso3)]
        bench[, y := iso3f %>>% as.numeric - 1]
        bench[, x := year(date)]
        bench[, value := ratingnum]

        bench[year(date)>1980 & !is.na(x) & !is.na(y) & !is.na(value),list(x,y,value)] %>>%
        plyr::alply(1, as.list) -> .data
        attributes(.data) <- NULL
        names(.data) <- NULL

        ## .data %>>%
        ## list.group(iso3) -> .data

        p <- rCharts::Highcharts$new()
        p$series(data = .data, type = 'heatmap', name = 'Test',
                 color = "#cccccc",
                 turboThreshold = 10000)
        p$yAxis(categories = (levels(bench$iso3f)),
                gridLineWidth = 0,
                title = 'Country')

        p$xAxis(title=list(text = "Year"))
        
        p$addParams(colorAxis =
                        list(min = 0,
                             max = 21,
                             maxColor= '#3060cf',
                             minColor= '#c4463a'
                             ),
                    dom = 'plot5'
                    )

        p$legend(align='right',
                 layout='vertical',
                 margin=0,
                 verticalAlign='top',
                 y=25,
                 symbolHeight=320,
                 floating = FALSE)        

                 
        p$chart(zoomType = "x", type = 'heatmap')
        p$exporting(enabled = TRUE)

        ## p$addAssets(js =
        ##                 c("https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js",
        ##                   "https://code.highcharts.com/highcharts.js",
        ##                   "https://code.highcharts.com/highcharts-more.js",
        ##                   "https://code.highcharts.com/modules/exporting.js",
        ##                   "https://code.highcharts.com/modules/heatmap.js"
        ##                   )
        ##             )
        return(p)
    })



    output[['plot3d']] <- rCharts::renderChart2({
        bench[, iso3f := as.factor(iso3)]
        bench[, y := cds]
        bench[, x := spread]
        bench[, z := ratingnum]

        bench[year(date)>1980 & !is.na(x) & !is.na(y) & !is.na(z),list(x,y,z)] %>>%
        plyr::alply(1, as.list) -> .data
        attributes(.data) <- NULL
        names(.data) <- NULL

        ## .data %>>%
        ## list.group(iso3) -> .data

        p <- rCharts::Highcharts$new()
        p$series(data = .data, type = 'scatter', name = 'Test',
                 colorByPoint = TRUE,
                 turboThreshold = 10000)

        p$chart(
            renderTo = 'container',
            options3d =
                list(
                    enabled = TRUE,
                    alpha =  10,
                    beta =  30,
                    depth = 250,
                    viewDistance =  5
                )
        )


        p$plotOptions(
            scatter = list(
                width = 10,
                height = 10,
                depth = 10
            )
        )
        
        p$exporting(enabled = TRUE)

        ## p$addAssets(js =
        ##                 c("https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js",
        ##                   "https://code.highcharts.com/highcharts.js",
        ##                   "https://code.highcharts.com/highcharts-more.js",
        ##                   "https://code.highcharts.com/modules/exporting.js",
        ##                   "https://code.highcharts.com/modules/heatmap.js",
        ##                   "https://code.highcharts.com/highcharts-3d.js"
        ##                   )
        ##             )

        p$setTemplate(
            script = "chart-3d.html"
        )

        p$addParams(dom = 'plot3d')
        
        return(p)
    })    


    
})




