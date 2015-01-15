shinyUI(
    mainPanel(
        width = 12,
        tabsetPanel(
            tabPanel("Variables",
                     fluidPage(
                         fluidRow(
                             column(12,
                                    selectInput("dataset",
                                                "Dataset:",
                                                unique("Test"))
                                    )
                         ),
                         fluidRow(
                             column(12,
                                    dataTableOutput('lookupTable'))
                         )
                     )),           
            tabPanel(
                "Plots",
                fluidPage(
                    fluidRow(
                        column(12,
                               selectizeInput(
                                   "variable",
                                   "Variable: ",
                                   choices = lookupSelections,
                                   multiple = FALSE,
                                   options = list(maxOptions = 1000),
                                   width = '100%'
                               ))
                    ),
                    fluidRow(
                        column(12,
                               verbatimTextOutput('text1')
                               )
                    ),
                    fluidRow(
                        column(12,
                               checkboxInput('check_winsor',"Winsorize?",TRUE)
                               )                        
                    ),
                    fluidRow(
                        column(12,
                               verbatimTextOutput('print1')
                               )
                    ),
                    fluidRow(
                        column(12,
                               plotOutput('plot1')
                               )
                    ),
                    h2("Availability:"),
                    fluidRow(
                        column(12,
                               dataTableOutput('availability'))
                    )
                )
            ),
            tabPanel(
                "Scatter",
                fluidPage(
                    fluidRow(
                        column(6,
                               selectizeInput(
                                   "var1",
                                   "Y-Variable: ",
                                   choices = lookupSelections[1:100],
                                   multiple = FALSE,
                                   options = list(maxOptions = 1000),
                                   width = '100%'
                               )),
                        column(6,
                               selectizeInput(
                                   "var2",
                                   "X-Variable: ",
                                   choices = lookupSelections[1:100],
                                   multiple = FALSE,
                                   options = list(maxOptions = 1000),
                                   width = '100%'
                               )),
                        fluidRow(
                            htmlOutput("plot2") 
                        )
                    )
                )
            )
        )
    )
)








