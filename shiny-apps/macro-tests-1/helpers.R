getWorldBankCountries <-
    function (url = "http://api.worldbank.org", type = "countries") 
{
    string <- paste(url, type, sep = "/")
    query <- paste0(string, "?", "format=json", "&per_page=1000000")
    raw.data <- RJSONIO::fromJSON(query)
    data <- raw.data[[2]]
    dat <- lapply(data, function(l) {
        as.data.table(as.list(unlist(l)))
    })
    dat = rbindlist(dat, fill = TRUE)
    return(dat)
}
