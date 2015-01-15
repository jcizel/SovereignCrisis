.stataNameConversions <- function(s){
    s %>>%
    gsub(pattern = '[[:punct:]]+',
         replacement = "_") %>>%
    gsub(pattern = '^([[:digit:]])(.+)',
         replacement = "_\\1\\2") %>>%
    gsub(pattern = '[[:space:]]',
         replacement = "_") %>>%    
    (~ out)
    
    return(out)
}


convertToStata <- function(data,
                           lookup = NULL,
                           nameCol = 'varcode',
                           labelCol = 'label',
                           filename = 'macro-dataset',
                           folder = './inst/extdata/stata/'){

    if (!inherits(data, 'data.table') | !inherits(lookup, 'data.table'))
        stop('lookup must be a data.table!')

    dt <- copy(data)
    setnames(data,
             names(data),
             .stataNameConversions(names(data)))
    
    foreign::write.dta(data,
                       file = paste0(folder,filename,'.dta'))

    if (!is.null(lookup)){
        sink(paste0(folder,filename,'-labels.do'))
        cat('cd "',getwd(),'"\n', sep = "")
        cat('use "',paste0(folder,filename,'.dta'),'", clear\n', sep = "")
        lookup[, list(name = .stataNameConversions(get(nameCol)),
                      label = label)] %>>%
        (df ~ df[,sprintf('label variable %s "%s"', name, label)]) %>>%
        paste(collapse = "\n") %>>% cat
        sink()
    }

    cat('File written to: ', paste0(folder,filename,'.dta'),".\n", sep = "")
}
