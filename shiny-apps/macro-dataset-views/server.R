.availability <- function(dt = macro,
                          var = "UPP.INS.DEMO.XQ"){
    .dt <- dt[!is.na(get(var))]

    .dt[, list(Beginning = min(year(date)),
               End = max(year(date)))
        , keyby = iso3]    
}


.scatterplot <- function(dt = macro,
                         var1 = 'GC.TAX.TOTL.GD.ZS',
                         var2 = 'FR.INR.RINR'
                         ){
    library(plotly)
    setkeyv(lookup,"varcode")
    
    py <- plotly(username="jcizel", key="lm8qr95w48","https://plot.ly")
    
    dt <- dt[!is.na(get(var1)) & !is.na(get(var2))]
    trace <-
        list(
            x = dt[[var1]] ,
            y = dt[[var2]] ,
            mode = 'markers',
            text = sprintf("%s:%4.0f",dt$iso3,year(dt$date)),
            type = 'scatter'
        )

    data = list(trace)

    layout <- list(
        title = "Test",
        xaxis = list(
            title = lookup[var1,label][[1L]] %>>% as.character,
            showgrid = FALSE,
            zeroline = FALSE,
            autotick = TRUE,            
            showticklabels = TRUE
        ),
        yaxis = list(
            title = lookup[var2,label][[1L]] %>>% as.character,
            showline = FALSE,
            autotick = TRUE,
            showticklabels = TRUE
        ),
        hovermode = 'closest'
    )
    
    response <-
        py$plotly(data,
                  kwargs=
                      list(layout=layout,
                           hovermode = 'closest',
                           filename="line-style",
                           fileopt="overwrite"))
    url <- response$url
    return(url)
}


shinyServer(function(input, output) {
    output$lookupTable <-
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

    output$availability <-
        renderDataTable(.availability(var = input$variable))
    
    output$text1 <- renderText(paste(input$variable))
    
    output$plot1 <- renderPlot({
        if (input$check_winsor== TRUE)
            hist(macro[[input$variable]] %>>% winsorize,
                 breaks = 100,
                 main = NULL)
        else
            hist(macro[[input$variable]],
                 breaks = 100,
                 main = NULL)            
    })

    output$plot2 <- renderUI({
        url <- .scatterplot(var1 = input$var1,
                            var2 = input$var2)
        tags$iframe(src=url,
                    frameBorder="0",
                    height=1000,
                    width=1000
                    )
    })

    output$print1 <- renderPrint({
        if (input$check_winsor== TRUE)
            summary(macro[[input$variable]] %>>% winsorize)
        else
            summary(macro[[input$variable]])
    })
})
