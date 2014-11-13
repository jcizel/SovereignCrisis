.innerTEX <- function(output,file){
    sink(file)
    cat(paste(output,collapse = "\n"))
    sink()
    cat(paste0("The table was written to the file '", file,
               "'.\n"))
    return(NULL)
}

.simpleCap <- function(x) {
    o <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
    return(o)
}

.sanitize <- function(s) {
    o <- gsub("\\&","\\\\&",s)
    return(o)
}

.indent <- function(s) {
    o <- paste0("\\hspace{0.3cm}",s)
    return(o)
}
