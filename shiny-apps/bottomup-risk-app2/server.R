shinyServer(function(input,output,session){
    set.seed(122)

    output[['lookupTable']] <-
        renderDataTable(
            macrodata.availability[,
                                   list(
                                       Name = name,
                                       Label = label %>>% as.character,
                                       ## Source = source,
                                       `Beginning` = yearmin,
                                       `End` = yearmax,
                                       `Number of Countries` = numiso,
                                       `Number of Observations` = numobs
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

    output[['zscoreTable']] <- renderDataTable({
        zscore[,
               list(
                   `Company Name` = conm,
                   `Date (Y:Q)` = DATE,
                   `Sales` = SALE,
                   `PU Model Score` = PU60_US_SEL3,
                   `PR Model Score` = PR60_US_SEL3,
                   `PU Model PD` = PD_PU60_US_SEL3,
                   `PR Model PD` = PD_PR60_US_SEL3
               )]
    })

    output[['bankpdTable']] <- renderDataTable({
        bankpd[!is.na(SC_CLOSURE_ALL)|!is.na(SC_OBR_EU), list(
            `Fiscal Year` = YEAR,
            `Bank Name` = NAME,
            `Accounting Consolidation` = CONSOL,
            `Country Code` = CTRYCODE,
            `Total Assets (in million USD)` = TOTASSUSD,
            `Bank Closure Model Score` = SC_CLOSURE_ALL,
            `Open-Bank Resolution Model Score` = SC_OBR_EU
        )]
    })
    
    output[['bottomupscore-plot1']] <- renderDygraph({
        rm(.data.)
        if (input[['modelselfirm']]==1){
            zscore[gvkey %in% input[["companyName"]],
                   list(NAME = conm,
                        DATE,
                        Y = get(input[['radio-scoreName-firm']]))] %>>%
            dcast.data.table(formula = DATE ~ NAME) -> .data.
        } else {
            bankpd[INDEX %in% input[["bankName"]],
                   list(NAME = NAME,
                        DATE,
                        Y = get(input[['radio-scoreName-bank']]))] %>>%
            dcast.data.table(formula = DATE ~ NAME) -> .data.            
        }

        .data.[,.SD,.SDcols = -c('DATE')] %>>% as.xts(order.by = .data.$DATE) -> .data
        dygraph(.data) %>>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>>%
        dyRangeSelector()
    })

    output[['bottomupscore-plot2']] <- renderDygraph({
        rm(.data.)
        if (input[['modelsel']]==1){
            if (input[['radio-aggregation']] == 'a'){
                .data. <- copy(zscore.agg)
            } else {
                .data. <- copy(zscore.agg.w)
            }
        } else {
            if (input[['radio-aggregation']] == 'a'){
                .data. <- copy(bankpd.agg)
            } else {
                .data. <- copy(bankpd.agg.w)
            }
        }
        
        if (input[['modelsel']]==1)
            var = input[['radio-scoreName']]
        else
            var = input[['radio-scoreName2']]

        .data.[fic %in% input[['countryName']] &
                      NAME == var,
                   c("fic","DATE",input[['radio-statistic']]), with = FALSE]  %>>%
        dcast.data.table(DATE ~ fic) -> .data        

        .data[,.SD,.SDcols = -c('DATE')] %>>% as.xts(order.by = .data$DATE) -> .data
        dygraph(.data) %>>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>>%
        dyRangeSelector()
        
    })


    output[['bottomupscore-plot3']] <- renderDygraph({
        rm(.data.)
        if (input[['modelsel']]==1){
            if (input[['radio-aggregation']] == 'a'){
                .data. <- copy(bankpd.agg)
            } else {
                .data. <- copy(bankpd.agg.w)
            }
        } else {
            if (input[['radio-aggregation']] == 'a'){
                .data. <- copy(bankpd.agg)
            } else {
                .data. <- copy(bankpd.agg.w)
            }
        }
        
        if (input[['radio-mode']]=='a')
            var = input[['radio-scoreName']]
        else
            var = input[['radio-scoreName2']]
        
        .data.[fic %in% input[['countryName']] &
                      NAME == var,
                   c("fic","DATE",input[['radio-statistic']]), with = FALSE]  %>>%
        dcast.data.table(DATE ~ fic) -> .data

        .data[, DATE := {
            x <- DATE
            year =
                gsub("^([0-9]+):([0-9])",
                     "\\1",
                     x) %>>% as.numeric
            qtr =
                gsub("^([0-9]+):([0-9])",
                     "\\2",
                     x) %>>% as.numeric
            as.Date(sprintf("%s-%s-%s",
                            year,
                            qtr * 3,
                            1))
        }]

        .data %>>% data.frame -> .data
        
        ## data must be a data.frame!!
        p <- plotTimeSeries(data = .data,ind.date = 1,ind.curve = 2:ncol(.data))        

        p$addParams(dom = "bottomupscore-plot3")
        return(p)
    })    
    
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


    output[['plotDebt']] <- renderDygraph({
        rm(.data.)
        rrdebt[iso3 %in% input[['debtCtrySel']],
               list(iso3,
                    date,
                    Y = get(input[['debtMeasure']]))] %>>%
        data.table(key = c('iso3','date')) %>>% unique %>>%
        dcast.data.table(date ~ iso3) -> .data.
        .data.[,.SD,.SDcols = -c('date')] %>>%
        as.xts(order.by = .data.$date) -> .data.
        
        dygraph(.data.) %>>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>>%
        dyRangeSelector()

    })

    output[['plotHouse']] <- renderDygraph({
        rm(.house)
        house[iso3 %in% input[['houseCtrySel']],
              list(iso3,
                   date,
                   Y = houseix)] %>>%
        data.table(key = c('iso3','date')) %>>% unique %>>%
        dcast.data.table(date ~ iso3) -> .house
        .house[,.SD,.SDcols = -c('date')] %>>%
        as.xts(order.by = .house$date) -> .house
        
        dygraph(.house) %>>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>>%
        dyRangeSelector()
    })    

    updateSelectizeInput(
        session,
        'macroMeasure',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            .x
        },
        selected = "GFDD.DM.06"
    )

    updateSelectizeInput(
        session,
        'macroDenominator',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            c(.x,
              "None" = "UNITDENOM")
        },
        selected = "UNITDENOM"
    )    
    
    
    output[['plotMacro']] <- renderDygraph({
        rm(.data.)
        macrodata[, UNITDENOM := 1]

        if (input[['macroFix']] == 'Variable'){
            .data. <- copy(macrodata[iso3 %in% input[['macroCtrySel']]])
            .data.[,
                   list(iso3,
                        date,
                        Y = get(input[['macroMeasure']])/get(input[['macroDenominator']]))] %>>%
            data.table(key = c('iso3','date')) %>>% unique %>>%
            dcast.data.table(date ~ iso3) -> .data.
            .data.[,.SD,.SDcols = -c('date')] %>>%
            as.xts(order.by = .data.$date) -> .data.
        } else {
            .data. <- copy(macrodata[iso3 == input[['macroCtrySel']][[1L]]])            
            .keys <- .data.[,
                            .SD,
                            .SDcols = c('iso3','date')]
            
            denom <-
                .data.[[input[['macroDenominator']]]]

            .data. <- .data.[,c(input[['macroMeasure']]),with = FALSE]
            
            .data. <- .data.[,
                             (input[['macroMeasure']]) :=lapply(.SD,function(x){
                                 x/denom
                             }),
                             .SDcols =
                                 input[['macroMeasure']]]

            .data. <- cbind(.keys, .data.)
            
            .data. %>>%
            data.table(key = c('iso3','date')) %>>% unique -> .data.

            .data.[,.SD,.SDcols = -c('iso3','date')] %>>%
            as.xts(order.by = .data.$date) -> .data.
        }

        
        dygraph(.data.) %>>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE) %>>%
        dyRangeSelector()

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


    updateSelectizeInput(
        session,
        'macroMeasure-1',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            .x
        },
        selected = NULL
    )

    updateSelectizeInput(
        session,
        'macroMeasure-2',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            .x
        },
        selected = NULL
    )    

    updateSelectizeInput(
        session,
        'dataset-macroMeasure-GDP',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            .x
        },
        selected = c('NY.GDP.MKTP.CD','NY.GDP.MKTP.CN')
    )
    
    updateSelectizeInput(
        session,
        'dataset-macroMeasure',
        choices = {
            .x <- macrodatalookup$name %>>% as.character %>>% as.list
            names(.x) <-
                sprintf("%s; Code: %s",
                        macrodatalookup$label,
                        .x)
            .x
        },
        selected = c(
            'AMT.A.PCT.P1',
            'GC.XPN.INTP.RV.ZS',
            'GC.REV.XGRT.GD.ZS',
            'GC.TAX.TOTL.GD.ZS',
            'GC.TAX.YPKG.RV.ZS',
            ## CASH SURPLUS/DEFICIT
            'GC.BAL.CASH.GD.ZS',
            'GC.BAL.CASH.CN',
            ## TRADE BALANCE,
            'L78AC&D',
            'DT.DOD.DIMF.CD',
            ## IMF
            'L2AT',
            ## HOUSEHOLD,
            'NE.CON.PETC.ZS',
            ## STOCK MARKET
            'GFDD.SM.01',
            "GFDD.OM.02",
            'L62',
            ## IMPORT?EXPORT
            "TM.VAL.MRCH.XD.WD",
            "TX.VAL.MRCH.XD.WD",
            "TT.PRI.MRCH.XD.WD",
            "NE.TRD.GNFS.ZS",
            "PX.REX.REER",
            "EXC",
            ## INFLATION
            'FP.CPI.TOTL',
            'FP.CPI.TOTL.ZG'
        )
    )

    datasetInput <- reactive({
        prepareDataset(
            benchMeasures = input[['datasetBenchVarSel']],
            crisisIndicators = input[['datasetCrisisSel']],
            bottomupMeasuresNonFin = input[['dataset-scoreName-nonfin']],
            weightedNonFin = FALSE,
            includeStatsNonFin = input[['dataset-statistic-nonfin']],
            bottomupMeasuresFin = input[['dataset-scoreName-fin']],
            weightedFin = FALSE,
            includeStatsFin = input[['dataset-statistic-fin']],
            PublicDebtMeasures = input[['dataset-publicDebt']],
            HousePriceMeasures = input[['dataset-includeHouse']],
            OtherMacroMeasures = input[['dataset-macroMeasure']]
        )
    })

    output[['downloadData']] <- downloadHandler(
        filename = function(){
            sprintf("%s-%s.%s",
                    input[['fileName']],
                    Sys.Date(),
                    input[['fileType']])
        },
        content = function(file){
            if (input[['fileType']]=="dta"){
                dt <- copy(datasetInput())
                setnames(dt,
                         names(dt),
                         .stataNameConversions(names(dt)))
                
                foreign::write.dta(dt,
                                   file = file)
            } else if (input[['fileType']]=="csv"){
                dt <- copy(datasetInput())
                write.csv(dt, file = file)
            } else {
                dt <- copy(datasetInput())                
                save(dt, file = file)
            }
            ## write.csv(datasetInput(),
            ##           file = file)
            ## foreign::write.dta(bench,
            ##                    file = file)
        }
    )

    output[['scatterPlot']] <-
        rCharts::renderChart2({
            macrodata[,tooltip := sprintf('Country: %s <hr/> Date: %s',
                                          iso3,
                                          date)]
            highchartScatter2d(
                data = macrodata,
                x = input[['macroMeasure-1']],
                y = input[['macroMeasure-2']],
                tooltipVar = 'tooltip'
            ) -> p
            return(p)
        })
})




