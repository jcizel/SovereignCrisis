highchartScatter2d <- function(
    data,
    x,
    y,
    group = NULL,
    tooltipVar = NULL,
    tooltip = "#!
function() {
return 'X: '+ this.point.x + '<br/>' +
'Y: ' + this.point.y; } !#",
    xlabel = "x",
    ylabel = "y"
){
    require(pipeR)
    require(rlist)
    
    if (!inherits(data,'data.table'))
        stop('`data` must be a data.table')
    
    if (!is.null(group))
        if (!is.null(tooltipVar))
            .data = copy(data[,c(x,y,group,tooltipVar),with = FALSE])
        else
            .data = copy(data[,c(x,y,group),with = FALSE])
    else
        if (!is.null(tooltipVar))
            .data = copy(data[,c(x,y,tooltipVar),with = FALSE])
        else
            .data = copy(data[,c(x,y,group),with = FALSE])
          
    .data[, x := get(x)]
    .data[, y := get(y)]

    .data[!is.na(x) & !is.na(y)] %>>%
    plyr::alply(1, as.list) -> .d
    attributes(.d) <- NULL
    names(.d) <- NULL

    if (!is.null(group)){
        .d %>>%
        rlist::list.group(get(group)) -> .d
    }

    p <- rCharts::Highcharts$new()

    if (!is.null(group)){
        .d %>>%
        rlist::list.map({
            p$series(
                data = .,
                type = 'scatter',
                name = .name
            )
        }) %>>% invisible
    } else {
        p$series(data = .d,
                 type = 'scatter',
                 name = "")
    }
        
    p$chart(backgroundColor = NULL)
    
    p$plotOptions(
        scatter = list(
            cursor = "pointer",
            marker = list(
                symbol = "circle",
                radius = 5
            )
        )
    )

    p$chart(zoomType = "xy")
    p$exporting(enabled = TRUE)

    if (!is.null(tooltipVar)){
        tooltip.new =
            sprintf("#!
function() {
return this.point.%s + '<br/>' + 
'X: '+ this.point.x + '<br/>' +
'Y: ' + this.point.y; } !#",
                    tooltipVar
                    )
        p$tooltip(useHTML = T, formatter = tooltip.new)        
    } else {
        p$tooltip(useHTML = T, formatter = tooltip)
    }
    
    p$xAxis(title = list(text = xlabel),
            labels = list(format = "{value}"))
    p$yAxis(title = list(text = ylabel),
            labels = list(format = "{value}"))

    return(p)
}


## Tests
## highchartScatter2d(data = bench,
##                    x = "cds",
##                    y = "ratingnum")

## highchartScatter2d(data = bench,
##                    x = "cds",
##                    y = "ratingnum",
##                    group = 'iso3')

## highchartScatter2d(data = bench,
##                    x = "cds",
##                    y = "ratingnum",
##                    tooltipVar = 'iso3'
##                    )

## bench[,tooltip := sprintf("Country: %s <br/> Date: %s",
##                           iso3,
##                           date)]
## highchartScatter2d(data = bench,
##                    x = "cds",
##                    y = "ratingnum",
##                    tooltipVar = 'tooltip'
##                    )

## highchartScatter2d(data = bench,
##                    x = "cds",
##                    y = "ratingnum",
##                    group = 'iso3',
##                    tooltipVar = 'tooltip'
##                    )

