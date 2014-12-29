library(shiny)
library(ggplot2)  # for the diamonds dataset

shinyUI(
    fluidPage(
        fluidRow(
            column(12,
                   selectInput("dataset",
                               "Dataset:",
                               c("IMF","World Bank","OECD"))
                   )
        ),
        fluidRow(
            column(12,
                   dataTableOutput('mytable1'))
        )
    )
)








