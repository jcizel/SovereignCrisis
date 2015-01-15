## ui.R ##
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Plots",
            icon = icon("th"),
            tabName = "plots",
            ## menuSubItem(
            ##     "Bottom-Up Risk Indicators (firm level)",
            ##     tabName = "boottomupPlots-firm"),
            menuSubItem(
                "Bottom-Up Risk Indicators (country level)",
                tabName = "boottomupPlots-country"),
            menuSubItem(
                "Credit Ratings, CDS & Bond Spreads",
                tabName = "plot1"
            )),        
        menuItem(
            "Data Availability",
            tabName = "availability",
            icon = icon("dashboard"),
            menuSubItem(
                "Countries Overview",
                tabName = "countries"
            ),
            menuSubItem(
                "Overview of Bottom-Up Risk Measures",
                tabName = "bottomupTables"
            ),            
            menuSubItem(
                "Sovereign Crises",
                tabName = "availability2"
            ),
            menuSubItem(
                "Sovereign Risk Measures",
                tabName = "availability3"
            ),
            menuSubItem(
                "Macroeconomic Indicators",
                tabName = "availability1"
            )),
        menuItem(
            "Regressions",
            tabName = "reg",
            menuSubItem(
                "Main Results",
                tabName = "reg1"
            ))
    )
)

body <-
    dashboardBody(
        tags$head(
            tags$script(src = "https://code.highcharts.com/highcharts.js"),
            tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
            tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
            tags$script(src = "https://code.highcharts.com/modules/heatmap.js"),
            tags$script(src = "https://code.highcharts.com/highcharts-3d.js")
        ),
        tabItems(
            tabItem(
                tabName = "countries",
                fluidRow(
                    box(
                        title = "Overview of Countries",
                        br(),
                        wellPanel(),
                        br(),
                        dataTableOutput('countryTable'),
                        width = 12
                    )
                )
            ),
            tabItem(
                tabName = "bottomupTables",
                fluidRow(
                    box(
                        dataTableOutput('zscoreTable'),
                        width = 12
                    ),
                    box(
                        dataTableOutput('bankpdTable'),
                        width = 12
                    )                    
                )
            ),            
            tabItem(
                tabName = "availability1",
                fluidRow(
                    box(
                        title = "Data Availability",                        
                        helpText("Below is a summary of the database of macroeconomic indicators that I compiled from several publicly available sources."),
                        hr(),
                        wellPanel(
                            selectizeInput(
                                "inSource",
                                h4("Select sources:"),                                       
                                choices =
                                    list(
                                        "World Bank" = "wb",
                                        "IMF" = "imf",
                                        "OECD" = "oecd"
                                    ),
                                selected = c("wb","imf","oecd"),
                                multiple = TRUE
                            )
                        ),
                        width = 12
                    ),
                    box(
                        fluidRow(
                            column(12,
                                   dataTableOutput('lookupTable'))
                        ),
                        width = 12
                    )
                )
            ),
            tabItem(
                tabName = "availability2",
                fluidRow(
                    box(
                        title = "Overview of the Sovereign Crisis Database",
                        br(),
                        wellPanel(),
                        br(),
                        dataTableOutput('crisisSummary'),
                        width = 12
                    )
                )
            ),
            tabItem(
                tabName = "availability3",
                fluidRow(
                    box(
                        dataTableOutput('benchAvailability')
                    )
                )
            ),
            tabItem(
                tabName = "plot1",
                fluidRow(
                    box(
                        title = "Controls",
                        radioButtons(
                            "benchdataSel",
                            label = "Select a sovereign risk measure:",
                            choices =
                                list("S&P Sovereign Rating" = 1,
                                     "Sovereign CDS Spread" = 2,
                                     "Sovereign Bond Spread" = 3),
                            selected = 1),
                        selectizeInput(
                            "ctrySel",
                            "Country:",
                            choices = names(ratm)[-1],
                            selected = c("GRC"),
                            multiple = TRUE,
                            width = '100%'
                        ),
                        dateRangeInput("dates",
                                       label = "Date range",
                                       start = "2000-01-01",
                                       end = "2012-12-31"),
                        width = 12
                    )            
                ),
                fluidRow(
                    box(
                        dygraphOutput(
                            "plot3", height = "500px"),
                        footer = "Test",
                        width = 12
                    )
                ),
                fluidRow(
                    box(
                        p("How would you like to group the data?"),
                        radioButtons(
                            "plot4-group",
                            label = "",
                            choices =
                                list(
                                    `No grouping` = "none",
                                    `Country` = "iso3",
                                    `Region` = "region.value",
                                    `Administration Region` = "adminregion.value",
                                    `Income Level` = "incomeLevel.value",
                                    `Lending Type` = "lendingType.value"
                                ),
                            selected = "incomeLevel.value"
                        ),
                        width = 2
                    )
                )
            ),
            tabItem(
                tabName = "reg1",
                fluidRow(
                    box(
                        img(src="reg-rat.png")
                    ),
                    box(
                        img(src="reg-spread.png")
                    )                                        
                ),
                fluidRow(
                    box(
                        img(src="reg-cds.png")
                    )
                )
            ),
            ## tabItem(
            ##     tabName = "boottomupPlots-firm",
            ##     fluidRow(
            ##         box(
            ##             h3("Latest update of Z-metrics and banking model scores at a firm- and bank-level"),
            ##             p("Instructions: Choose the parameters on the right, and the graph below will be automatically updated."),
            ##             width = 6
            ##         ),                    
            ##         box(
            ##             radioButtons(
            ##                 "modelselfirm",
            ##                 "Select Bottom-Up Model: ",
            ##                 choices = list(
            ##                     "Non-Financial Sector Model" = 1,
            ##                     "Financial Sector Model" = 2
            ##                 ),
            ##                 selected = c(1)
            ##             ),                        
            ##             conditionalPanel(
            ##                 condition = "input.modelselfirm==1",
            ##                 radioButtons(
            ##                     "radio-scoreName-firm",
            ##                     "Select bottom-up measure for non-financial sector health: ",
            ##                     choices =
            ##                         list('PU Model Score'='PU60_US_SEL3',
            ##                              'PR Model Score'='PR60_US_SEL3',
            ##                              'PU Model PD'='PD_PU60_US_SEL3',
            ##                              'PR Model PD'='PD_PR60_US_SEL3'),
            ##                     selected = c('PU60_US_SEL3'),
            ##                 )                            
            ##             ),
            ##             conditionalPanel(
            ##                 condition = "input.modelselfirm==2",
            ##                 radioButtons(
            ##                     "radio-scoreName-bank",
            ##                     "Select bottom-up measure for non-financial sector health: ",
            ##                     choices =
            ##                         list('Bank Closure Model Score'='SC_CLOSURE_ALL',
            ##                              'Open Bank Resolution Model Score'='SC_OBR_EU'),
            ##                     selected = c('SC_CLOSURE_ALL'),
            ##                 )                            
            ##             ),
            ##             conditionalPanel(
            ##                 condition = "input.modelselfirm==1",
            ##                 selectizeInput(
            ##                     "companyName",
            ##                     "Company name: ",
            ##                     choices = {
            ##                         zscore[,list(conm,gvkey,fic)] %>>% unique -> .l
            ##                         .x <- .l$gvkey %>>% as.list
            ##                         names(.x) <- sprintf("%s (%s); GVKEY: %s",
            ##                                              .l$conm,
            ##                                              .l$fic,
            ##                                              .l$gvkey)
            ##                         .x
            ##                     },
            ##                     selected = c(8546,12141,1690,6066,203892),
            ##                     multiple = TRUE
            ##                 )                            
            ##             ),
            ##             conditionalPanel(
            ##                 condition = "input.modelselfirm==2",
            ##                 selectizeInput(
            ##                     "bankName",
            ##                     "Bank name: ",
            ##                     choices = {
            ##                         bankpd[!is.na(SC_CLOSURE_ALL),
            ##                                list(NAME,INDEX,fic)] %>>% unique -> .l2
            ##                         .x <- .l2$INDEX %>>% as.list
            ##                         names(.x) <- sprintf("%s (%s); BVDID: %s",
            ##                                              .l2$NAME,
            ##                                              .l2$fic,
            ##                                              .l2$INDEX)
            ##                         .x
            ##                     },
            ##                     selected = c(50633),
            ##                     multiple = TRUE
            ##                 )                            
            ##             ),                        
            ##             width = 6
            ##         )
            ##     ),
            ##     fluidRow(
            ##         box(
            ##             dygraphOutput(
            ##                 'bottomupscore-plot1', height = "500px"),
            ##             width = 12
            ##         )
                    
            ##     )
            ##     ## fluidRow(
            ##     ##     box(
            ##     ##         highchartsOutput(
            ##     ##             'bottomupscore-plot3', 'highstock'),
            ##     ##         HTML('<style>#bottomupscore-plot3 {width: 100%; height: 800px}</style>'),
            ##     ##             width = 12
            ##     ##            )
            ##     ## ),
            ##     ## fluidRow(
            ##     ##     box(
            ##     ##         rCharts::chartOutput("plot4", lib = "highcharts"),
            ##     ##         HTML('<style>.rChart {width: 100%; height: 600px}</style>'),
            ##     ##         width = 12
            ##     ##     )
            ##     ## ),
            ##     ## fluidRow(
            ##     ##     box(
            ##     ##         dygraphOutput(
            ##     ##             'bottomupscore-plot1', height = "500px"),
            ##     ##         width = 12
            ##     ##     )                    
            ##     ## ),
                
            ##     ## fluidRow(
            ##     ##     box(
            ##     ##         rCharts::chartOutput("plot5", lib = "highcharts"),
            ##     ##         HTML('<style>#plot5 {width: 100%; height: 3000px}</style>'),
            ##     ##         width = 12
            ##     ##     )                    
            ##     ## ),
            ##     ## fluidRow(
            ##     ##     box(
            ##     ##         rCharts::chartOutput("plot3d", lib = "highcharts"),
            ##     ##         HTML('<style>#plot3d {width: 100%; height: 900px}</style>'),
            ##     ##         width = 12
            ##     ##     )                    
            ##     ## )                
            ## ),
            tabItem(
                tabName = "boottomupPlots-country",
                fluidRow(
                    box(
                        h3("Latest update of Z-metrics and banking model scores at a country level"),
                        p("Instructions: Choose the parameters on the right, and the graph below will be automatically updated."),
                        width = 6
                    ),                    
                    box(
                        selectizeInput(
                            "countryName",
                            "Country name: ",
                            choices = unique(zscore.agg$fic),
                            selected = c("USA","NLD","ITA","GRC"),
                            multiple = TRUE
                        ),
                        radioButtons(
                            "modelsel",
                            "Select Bottom-Up Model: ",
                            choices = list(
                                "Non-Financial Sector Model" = 1,
                                "Financial Sector Model" = 2
                            ),
                            selected = c(1)
                        ),                        
                        conditionalPanel(
                            condition = "input.modelsel==1",
                            radioButtons(
                                "radio-scoreName",
                                "Select bottom-up measure for non-financial sector health: ",
                                choices =
                                    list('PU Model Score'='PU60_US_SEL3',
                                         'PR Model Score'='PR60_US_SEL3',
                                         'PU Model PD'='PD_PU60_US_SEL3',
                                         'PR Model PD'='PD_PR60_US_SEL3'),
                                selected = c('PU60_US_SEL3'),
                            )                            
                        ),
                        conditionalPanel(
                            condition = "input.modelsel==2",
                            radioButtons(
                                "radio-scoreName2",
                                "Select bottom-up measure for non-financial sector health: ",
                                choices =
                                    list('Bank Closure Model'='SC_CLOSURE_ALL',
                                         'Open Bank Resolution Model'='SC_OBR_EU'),
                                selected = c('SC_CLOSURE_ALL'),
                            )                            
                        ),
                        radioButtons(
                            "radio-aggregation",
                            "Should aggregation be done on weighted or non-weighted basis? (weights consist of firms' sales) ",
                            choices =
                                list('Non-Weighted'='a',
                                     'Weighted'='b'),
                            selected = c('a'),
                        ),
                        radioButtons(
                            "radio-statistic",
                            "Which aggregated statistic should be reported? ",
                            choices = names(zscore.agg[,.SD,.SDcols = -c("DATE","fic","N","SUM","NAME")]),
                            selected = c('90%'),
                            inline = TRUE
                        ),                        
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        dygraphOutput(
                            'bottomupscore-plot2', height = "500px"),
                        width = 12
                    )                    
                )
                ## fluidRow(
                ##     box(
                ##         highchartsOutput(
                ##             'bottomupscore-plot3', 'highstock'),
                ##         HTML('<style>#bottomupscore-plot3 {width: 100%; height: 800px}</style>'),
                ##             width = 12
                ##            )
                ## ),
                ## fluidRow(
                ##     box(
                ##         rCharts::chartOutput("plot4", lib = "highcharts"),
                ##         HTML('<style>.rChart {width: 100%; height: 600px}</style>'),
                ##         width = 12
                ##     )
                ## ),
                ## fluidRow(
                ##     box(
                ##         dygraphOutput(
                ##             'bottomupscore-plot1', height = "500px"),
                ##         width = 12
                ##     )                    
                ## ),
                
                ## fluidRow(
                ##     box(
                ##         rCharts::chartOutput("plot5", lib = "highcharts"),
                ##         HTML('<style>#plot5 {width: 100%; height: 3000px}</style>'),
                ##         width = 12
                ##     )                    
                ## ),
                ## fluidRow(
                ##     box(
                ##         rCharts::chartOutput("plot3d", lib = "highcharts"),
                ##         HTML('<style>#plot3d {width: 100%; height: 900px}</style>'),
                ##         width = 12
                ##     )                    
                ## )                
            )            
        )
    )


dashboardPage(
    dashboardHeader(
        title = "Bottom-Up Risk"),
    sidebar,
    body,
    skin = 'black'
)



