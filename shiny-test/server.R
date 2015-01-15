library(shiny)
library(data.table)
library(pipeR)

imf <- fread('./data/IMFListOfVariables.csv')[, list(Variable = varcode,
                                                     Label = label,
                                                     MinYear = yearmin,
                                                     MaxYear = yearmax,
                                                     Span = yearmax - yearmin + 1,
                                                     NumIso = numiso
                                                     )         
                                              ] %>>% unique

wb <- fread('./data/WorldBankListOfVariables.csv')[, list(Variable = id,
                                                          Source = source.value,
                                                          Label = name,
                                                          Notes = sourceNote)]

oecd <- fread('./data/OECD-VariableList.csv')[, list(IX = as.numeric(V1),
                                                     Variable = VARNAME,
                                                     Source =
                                                         SOURCE %>>% gsub(pattern = "(.+)OECD/(.+)/LOOKUPTABLE\\.csv",replacement = "\\2"),
                                                     Label = LABEL), key = VARNAME] %>>% unique

oecd[,VARNAME := NULL]





shinyServer(function(input, output) {


    output$mytable1 <- renderDataTable(
        options = list(
            pageLength = 10
            ## initComplete = I("function () {
            ## var api = this.api();

            ## api.columns().indexes().flatten().each( function ( i ) {
            ##     var column = api.column( i );
            ##     var select = $('<select><option value=\"\"></option></select>')
            ##         .appendTo( $(column.footer()).empty() )
            ##         .on( 'change', function () {
            ##             var val = $.fn.dataTable.util.escapeRegex(
            ##                                         $(this).val()
            ##                                     );

            ##             column
            ##                 .search( val ? '^'+val+'$' : '', true, false )
            ##                 .draw();
            ##         } );

            ##     column.data().unique().sort().each( function ( d, j ) {
            ##         select.append( '<option value=\"'+d+'\">'+d+'</option>' )
            ##     } );
            ## } );}")
        ),
        expr = {
            if (input$dataset == "IMF"){
                dt <- imf
            } else if (input$dataset == "World Bank"){
                dt <- wb
            } else if (input$dataset == "OECD"){
                dt <- oecd
            }
            dt
        })
})
