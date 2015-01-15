## ui.R ##
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", icon = icon("home",lib="glyphicon"), tabName = "welcome",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem(
            "Dataset construction",
            tabName = "dataset",
            icon = icon("file",lib="glyphicon")            
        ),
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
            ),
            menuSubItem(
                "Debt",
                tabName = "debtPlots"
            ),
            menuSubItem(
                "House prices",
                tabName = "housePlots"
            ),            
            menuSubItem(
                "Macro Variables",
                tabName = "macroPlots"
            )),        
        menuItem(
            "Data Availability",
            tabName = "availability",
            icon = icon("dashboard"),
            menuSubItem(
                "Countries Overview",
                tabName = "countries"
            ),
            ## menuSubItem(
            ##     "Overview of Bottom-Up Risk Measures",
            ##     tabName = "bottomupTables"
            ## ),            
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


welcome <-
    tabItem(
        tabName = "welcome",
        fluidRow(
            box(
                title = h1("Welcome to SovRisk Analytics")
            )
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
            welcome,
            tabItem(
                tabName = "countries",
                fluidRow(
                    box(
                        title = h1("Overview of Countries"),
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
                        ## wellPanel(
                        ##     selectizeInput(
                        ##         "inSource",
                        ##         h4("Select sources:"),                                       
                        ##         choices =
                        ##             list(
                        ##                 "World Bank" = "wb",
                        ##                 "IMF" = "imf",
                        ##                 "OECD" = "oecd"
                        ##             ),
                        ##         selected = c("wb","imf","oecd"),
                        ##         multiple = TRUE
                        ##     )
                        ## ),
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
            tabItem(
                tabName = "debtPlots",
                fluidRow(
                    box(
                        title = "Controls",
                        radioButtons(
                            "debtMeasure",
                            label = "Select a sovereign risk measure:",
                            choices =
                                list("External Debt (General & Central Government); scaled by exports" = "E_GCG_E",
                                     "Domestic & External Debt (General & Central Government); scaled by export" = "DE_GCG_E",
                                     "Domestic & External Debt (General & Central Government); scaled by GDP" = "DE_GCG_G",
                                     "Domestic & External Debt (All Public Debt); scaled by GDP" = "DE_GGG_G",
                                     "Private & Public Debt; scaled by exports" = "PP_GE_E",
                                     "Private & Public Debt; scaled by GDP" = "PP_GE_G",
                                     "Domestic & External Debt (combined); scaled by GDP" = "DE_G_GDP_COMP"),
                            selected = "DE_G_GDP_COMP"),
                        selectizeInput(
                            "debtCtrySel",
                            "Country:",
                            choices = unique(rrdebt$iso3),
                            selected = c("GRC"),
                            multiple = TRUE,
                            width = '100%'
                        ),
                        width = 6
                    )            
                ),
                fluidRow(
                    box(
                        dygraphOutput(
                            "plotDebt", height = "500px"),
                        footer = "Test",
                        width = 12
                    )
                )                
            ),
            tabItem(
                tabName = "housePlots",
                fluidRow(
                    box(
                        selectizeInput(
                            "houseCtrySel",
                            "Country:",
                            choices = unique(house$iso3),
                            selected = c("GRC","USA"),
                            multiple = TRUE,
                            width = '100%'
                        ),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        dygraphOutput(
                            "plotHouse", height = "500px"),
                        ## footer = "Test",
                        width = 12
                    )
                )                
            ),
            tabItem(
                tabName = "macroPlots",
                fluidRow(
                    box(
                        title = "Controls",
                        radioButtons(
                            'macroFix',
                            label = "What to fix in the graph below?",
                            choices = c("Country","Variable"),
                            selected = "Variable",
                            inline = TRUE
                        ),                                                
                        selectizeInput(
                            "macroMeasure",
                            label = "Select a sovereign risk measure:",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectizeInput(
                            "macroDenominator",
                            label = "Select a sovereign risk measure:",
                            choices = NULL,
                            multiple = FALSE
                        ),
                        selectizeInput(
                            "macroCtrySel",
                            "Country:",
                            choices = unique(macrodata$iso3),
                            selected = c("GRC","USA"),
                            multiple = TRUE,
                            width = '100%'
                        ),
                        width = 6
                    ),
                    box(
                        h3("Dynamics of Macroeconomic Indicators"),
                        p("Our database of macroeconomic indicators consists of more than 10,000 variables coming from World Bank, OECD, and IMF. "),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        dygraphOutput(
                            "plotMacro", height = "500px"),
                        footer = "Test",
                        width = 12
                    )
                ),
                fluidRow(
                    ## box(
                    ##     selectizeInput(
                    ##         "macroMeasure-1",
                    ##         label = "Select a sovereign risk measure:",
                    ##         choices = NULL,
                    ##         multiple = FALSE
                    ##     ),                        
                    ##     width = 6
                    ## ),
                    ## box(
                    ##     selectizeInput(
                    ##         "macroMeasure-2",
                    ##         label = "Select a sovereign risk measure:",
                    ##         choices = NULL,
                    ##         multiple = FALSE
                    ##     ),                        
                    ##     width = 6
                    ## ),
                    ## box(
                    ##     rCharts::chartOutput("scatterPlot", lib = "highcharts"),
                    ##     HTML('<style>.rChart {width: 100%; height: 600px}</style>'),
                    ##     width = 12
                    ## )
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
                                selected = c('SC_CLOSURE_ALL')
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
            ),
            
            ## DATASET CONSTRUCTION
            tabItem(
                tabName = "dataset",
                box(
                    title = "Benchmark Sovereign Risk Measures",
                    checkboxGroupInput(
                        'datasetBenchVarSel',
                        label = "Select benchmark variables to be included in the dataset",
                        choices =
                            list("S&P Sovereign Rating" = "ratingnum",
                                 "Sovereign CDS Spread" = "cds",
                                 "Sovereign Bond Spread" = "spread"),                       
                        selected = c("cds","ratingnum","spread")
                    ),
                    width = 6
                ),
                box(
                    title = "Sovereign Crisis Indicators",
                    checkboxGroupInput(
                        'datasetCrisisSel',
                        label = "Select Crisis Indicators to Include:",
                        choices =
                            list("Stock market crash (Reinhardt & Rogoff)" = "RRCRISIS_SMCRASH",
                                 "Currency crisis (Reinhardt & Rogoff)" = "RRCRISIS_CURR",
                                 "Inflation crisis (Reinhardt & Rogoff)" = "RRCRISIS_INFL",
                                 "Domestic sovereign debt crisis (Reinhardt & Rogoff)" = "RRCRISIS_DSDEBT",
                                 "Foreign sovereign debt crisis (Reinhardt & Rogoff)" = "RRCRISIS_FSDEBT",
                                 "Banking crisis (Reinhardt & Rogoff)" = "RRCRISIS_BANK",
                                 "Banking crisis (World Bank)" = "WBCRISIS_BANK"),
                        selected = c("RRCRISIS_DSDEBT","RRCRISIS_FSDEBT")
                    ),
                    ## checkboxInput(
                    ##     'datasetCreateTTE',
                    ##     label = tags$b("Create Time-to-Event variables for each of selected crisis indicators? (name: [CRISIS.INDICATOR]_TTE)"),
                    ##     value = FALSE
                    ## ),
                    width = 6
                ),
                box(
                    title = "Non-Financial Bottom-Up Risk Measure",
                    checkboxGroupInput(
                        "dataset-scoreName-nonfin",
                        "Select bottom-up measure for non-financial sector health: ",
                        choices =
                            list('PU Model Score'='PU60_US_SEL3',
                                 'PR Model Score'='PR60_US_SEL3',
                                 'PU Model PD'='PD_PU60_US_SEL3',
                                 'PR Model PD'='PD_PR60_US_SEL3'),
                        selected = c('PU60_US_SEL3'),
                    ),                            
                    radioButtons(
                        "dataset-aggregation-nonfin",
                        "Should aggregation be done on weighted or non-weighted basis? (weights consist of firms' sales) ",
                        choices =
                            list('Non-Weighted (suffix = NW)'=FALSE,
                                 'Weighted (suffix = W)'=TRUE),
                        selected = FALSE,
                    ),
                    checkboxGroupInput(
                        "dataset-statistic-nonfin",
                        "Which aggregated statistic should be reported? ",
                        choices = names(zscore.agg[,.SD,.SDcols = -c("DATE","fic","N","SUM","NAME")]),
                        selected = c('MEAN','50%','70%','90%'),
                        inline = TRUE
                    ),
                    p("Note: Original non-financial scores are multiplied by -1. (explanation: as originally constructed, the non-financial health scores are negatively associated with risk. On the contrary, financial scores are constructed to be positively associated with risk.)"),
                    width = 6
                ),
                box(
                    title = "Financial Sector  Bottom-Up Risk Measure",
                    checkboxGroupInput(
                        "dataset-scoreName-fin",
                        "Select bottom-up measure for non-financial sector health: ",
                        choices =
                            list('Bank Closure Model'='SC_CLOSURE_ALL',
                                 'Open Bank Resolution Model'='SC_OBR_EU'),
                        selected = c('SC_CLOSURE_ALL')                            
                    ),                            
                    radioButtons(
                        "dataset-aggregation-fin",
                        "Should aggregation be done on weighted or non-weighted basis? (weights consist of firms' sales) ",
                        choices =
                            list('Non-Weighted (suffix = NW)'=FALSE,
                                 'Weighted (suffix = W)'=TRUE),
                        selected = FALSE,
                    ),
                    checkboxGroupInput(
                        "dataset-statistic-fin",
                        "Which aggregated statistic should be reported? ",
                        choices = names(zscore.agg[,.SD,.SDcols = -c("DATE","fic","N","SUM","NAME")]),
                        selected = c('MEAN','50%','70%','90%'),
                        inline = TRUE
                    ),                    
                    width = 6
                ),
                box(
                    title = "Additional Macroeconomic Measures",
                    tags$hr(),
                    ## h4("GDP"),
                    ## selectizeInput(
                    ##     "dataset-macroMeasure-GDP",
                    ##     label = NULL,
                    ##     choices = NULL,
                    ##     multiple = TRUE
                    ## ),                    
                    ## tags$hr(),
                    h4("Public Debt (stocks and flows)"),
                    p("Our principal source of public debt stocks is the database of Reinhardt and Rogoff. To our knowledge this database has the most complete and comprehensive coverage."),
                    checkboxGroupInput(
                        "dataset-publicDebt",
                        label = NULL,
                        choices =
                            list("External Debt (General & Central Government); scaled by exports" = "E_GCG_E",
                                 "Domestic & External Debt (General & Central Government); scaled by export" = "DE_GCG_E",
                                 "Domestic & External Debt (General & Central Government); scaled by GDP" = "DE_GCG_G",
                                 "Domestic & External Debt (All Public Debt); scaled by GDP" = "DE_GGG_G",
                                 "Private & Public Debt; scaled by exports" = "PP_GE_E",
                                 "Private & Public Debt; scaled by GDP" = "PP_GE_G",
                                 "Domestic & External Debt (combined); scaled by GDP" = "DE_G_GDP_COMP"),
                        selected = 'DE_G_GDP_COMP'
                    ),
                    p("Other potentially useful public debt stock measures include: AMT.A.PCT.P1"),                    
                    tags$hr(),                    
                    h4("House Price Indices"),
                    radioButtons(
                        "dataset-includeHouse",
                        label = "Include house price index (source: BIS)?",
                        choices =
                            list("Yes" = TRUE,
                                 "No" = FALSE),
                        selected = TRUE
                    ),
                    ## tags$hr(),                    
                    ## h4("Tax Revenues"),
                    ## tags$hr(),                    
                    ## h4("Primary Budget Deficit"),
                    tags$hr(),                    
                    h4("Other (e.g. tax revenues, budget deficits)."),
                    p("Consult 'Macroeconomic Indicators' section of the 'Data Availability' header to view all available variables and their availability across time and countries."),
                    selectizeInput(
                        "dataset-macroMeasure",
                        label = NULL,
                        choices = NULL,
                        multiple = TRUE
                    ),                    
                    width = 12
                ),
                ## box(
                ##     title = "Final touches",
                ##     width = 8
                ## ),
                box(
                    title = "Download",
                    hr(),
                    textInput(
                        "fileName",
                        label = "Enter file name:",
                        value = "TEST"
                    ),
                    radioButtons(
                        "fileAddDate",
                        label = "Add date signature to the filename?",
                        choices = c("Yes","No"),
                        selected = "Yes"
                    ),
                    radioButtons(
                        "fileType",
                        label = "Choose file type:",
                        choices = c(
                            "Stata" = "dta",
                            "R" = "RData",
                            "CSV" = "csv"
                        ),
                        selected = "dta"
                    ),
                    downloadButton('downloadData', 'Download'),
                    width = 4
                )                
           ) 
        ),
        tags$head(tags$style(HTML('
      body > .header .logo {
        font-weight: bold;
        font-size: 12px;
      }
    ')))
    )


dashboardPage(
    title = "SovRisk Analytics",
    dashboardHeader(
        title = "SovRisk Analytics by Janko Cizel",
        dropdownMenu(type = "tasks", badgeStatus = "success",
                     taskItem(value = 90, color = "green",
                              "Dataset construction"
                              ),
                     taskItem(value = 70, color = "aqua",
                              "Data availability reports"
                              ),
                     taskItem(value = 60, color = "yellow",
                              "Visualizations"
                              ),
                     taskItem(value = 40, color = "red",
                              "Analysis"
                              ),
                     taskItem(value = 40, color = "red",
                              "Report"
                              )                     
                     )
    ),
    sidebar,
    body,
    skin = 'black'
)
