## ui.R ##
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Test",
            tabName = "test"
        ),
        menuItem(
            "Data Availability",
            tabName = "availability",
            icon = icon("dashboard"),
            menuSubItem(
                "Countries Overview",
                tabName = "countries"
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
            "Plots",
            icon = icon("th"),
            tabName = "plots",
            menuSubItem(
                "Credit Ratings",
                tabName = "plot1")),
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
                tabName = "test",
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
                ),
                fluidRow(
                    box(
                        rCharts::chartOutput("plot4", lib = "highcharts"),
                        HTML('<style>.rChart {width: 100%; height: 600px}</style>'),
                        width = 12
                    )
                ),
                fluidRow(
                    box(
                        rCharts::chartOutput("plot5", lib = "highcharts"),
                        HTML('<style>#plot5 {width: 100%; height: 3000px}</style>'),
                        width = 12
                    )                    
                ),
                fluidRow(
                    box(
                        rCharts::chartOutput("plot3d", lib = "highcharts"),
                        HTML('<style>#plot3d {width: 100%; height: 900px}</style>'),
                        width = 12
                    )                    
                )                
            )
        )
    )


dashboardPage(
    dashboardHeader(
        title = "Sovereign Risk"),
    sidebar,
    body
)



